# Setup ========================================================================
## Libraries -------------------------------------------------------------------
# Reading files
library(readxl)
library(readr)
library(haven)

# Data and others
library(tidyverse)
library(magrittr)
library(glue)
library(purrr)
library(janitor)
library(tictoc)
# library(MASS)

# Tables
library(modelsummary)
library(stargazer)
library(kableExtra)

# Models and fancy standard errors
library(car)
library(sandwich)
library(lmtest)
library(quantreg)
library(nprobust)

## Useful Stuff ----------------------------------------------------------------
# Useful commands for cleaning the working environment
rm(list = ls())
cat("\014")

# Directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
str_base_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
str_figure_directory <- paste(str_base_directory, "figures", sep = "/")
str_table_directory <- paste(str_base_directory, "tables", sep = "/")

# Seed
set.seed(121019)

# Question 2: Estimating the MTE Function ======================================
# Setting simulation parameters
N <- 10000  # Sample size
M <- 1000   # Number of Monte Carlo simulations

## DGP -------------------------------------------------------------------------
# Parameters
av = 0
bv = 1
az = 0
bz = .5
gamma = 1 / bz
sigma0 = 1
sigmaU = 1
beta0 = 1
beta1 = 2
beta2 = 3

V_grid = seq(.1, .9, by = .1)

run_simulation = FALSE

# Function
dgp = function(N, av. = av, bv. = bv, az. = az, bz. = bz, gamma. = gamma, 
               sigma0. = sigma0, sigmaU. = sigmaU, 
               beta0. = beta0, beta1. = beta1, beta2. = beta2){
   
   # Creating variables
   V = runif(N, min = av., max = bv.)
   Z = runif(N, min = az., max = bz.)
   U = rnorm(N, mean = 0, sd = sigmaU.)
   
   # Potential outcomes
   Y0 = rnorm(N, mean = 0, sd = sigma0.)
   Y1 = Y0 + U + beta0. + beta1. * V + beta2. * V**2
   
   # Propensity to be treated given Z
   PZ = gamma. * Z
   
   # Treatment indicator
   D = ifelse(PZ >= V, 1, 0)
   
   # Outcome variable
   Y = (1 - D) * Y0 + D * Y1  # ifelse(D == 1, Y1, Y0)
   
   # Creating dataframe
   data <- as.data.frame(list(D, Z, PZ, V, U, Y, Y0, Y1))
   colnames(data) <- c("D", "Z", "PZ", "V", "U", "Y", "Y(0)", "Y(1)")
   
   # Returning
   return(data)
}


## MTE Estimators --------------------------------------------------------------
# Functions
estimate_mte_parametric = function(Y, PZ){
   
   # In the PSET, we found that under our DGP, a good parametric form for the
   # MTE is being quadratic on V. This implies E[Y | P(Z) = p] is cubic on p.
   # Since this is linear on parameters, we can estimate it using OLS.
   
   # Creating variable
   PZ2 = PZ ** 2 / 2
   PZ3 = PZ ** 3 / 3
   
   # OLS
   ols_mte_parametric <- lm(Y ~ PZ + PZ2 + PZ3)
   
   # Returning
   return(ols_mte_parametric)
}

estimate_mte_non_parametric = function(Y, PZ, V_grid. = V_grid){
   
   # We don't have covariates, so we can just run lprobust directly.
   # If we had covariates, we would have to run the more complicated procedure
   # of the last lecture of Microeconometrics I (see PSet 05 of that course).
   
   # We want a local quadratic (and not linear) regression, so we set p = 2.
   # Also, the MTE is the derivative of the conditional expectation function,
   # so we set deriv = 1.
   # Finally, we run the regressions on the grid we are interested in
   mte_non_parametric <- lprobust(y = Y, x = PZ, 
                                  eval = V_grid., p = 2, deriv = 1)
   
   # Returning
   return(mte_non_parametric)
}
   
calculate_bias = function(model, bool_parametric, 
                          V_grid. = V_grid, 
                          beta0. = beta0, beta1. = beta1, beta2. = beta2){
   
   # Calculating estimated MTE functions
   if (bool_parametric){  # Parametric model
      # Coefficients
      beta0_est = coef(model)[2]
      beta1_est = coef(model)[3]
      beta2_est = coef(model)[4]
      
      # Estimated MTE at desired grid points
      est_MTE = beta0_est + beta1_est * V_grid. + beta2_est * V_grid.**2
      
   } else {  # Nonparametric model
      # Will collect tau.us since it is the default of nprobust.plot
      est_MTE = as.data.frame(model$Estimate)$tau.us
   }
   
   # True MTE (recall that E[U] = 0)
   true_MTE = beta0. + beta1. * V_grid. + beta2. * V_grid.**2
   
   # Bias
   abs_bias = est_MTE - true_MTE
   rel_bias = 100 * abs_bias / true_MTE
   
   # Data
   data <- as.data.frame(list(V_grid., true_MTE, est_MTE, abs_bias, rel_bias))
   colnames(data) <- c("V_grid", "true_MTE", "est_MTE", "abs_bias", "rel_bias")
   
   # Returning
   return(data)
}

# Testing
# test_dgp = dgp(N = N)
# parametric_results = estimate_mte_parametric(Y = test_dgp$Y, PZ = test_dgp$PZ)
# non_parametric_results = estimate_mte_non_parametric(Y = test_dgp$Y, 
#                                                      PZ = test_dgp$PZ)


## Simulation ------------------------------------------------------------------
if (run_simulation){

   # Monte Carlo simulations
   tic()
   for (m in 1:M){
      # Progress
      print(paste0(round(100 * m / M, digits = 1), "%..."))
      
      # Generating DGPs
      data <- dgp(N)
      
      # Estimating MTEs at our grid points
      parametric_results = estimate_mte_parametric(Y = data$Y, 
                                                   PZ = data$PZ)
      non_parametric_results = estimate_mte_non_parametric(Y = data$Y, 
                                                           PZ = data$PZ)
      
      # Calculating bias
      bias_df_parametric = calculate_bias(parametric_results, 
                                          bool_parametric = TRUE)
      bias_df_non_parametric = calculate_bias(non_parametric_results,
                                              bool_parametric = FALSE)
      
      # Identifying simulation index
      bias_df_parametric$simulation_id = m
      bias_df_non_parametric$simulation_id = m
      
      # Storing results: each dataframe has results for all values of V_grid
      if (m == 1){
         df_results_parametric <- as.data.frame(bias_df_parametric)
         df_results_non_parametric <- as.data.frame(bias_df_non_parametric)
      } else {
         df_results_parametric <- as.data.frame(
            rbind(df_results_parametric, bias_df_parametric)
         )
         
         df_results_non_parametric <- as.data.frame(
            rbind(df_results_non_parametric, bias_df_non_parametric)
         )
      }
   }
   toc() # 8.2 hours
   
   # Saving because this takes a while
   saveRDS(df_results_parametric, "parametric_mte_results.rds")
   saveRDS(df_results_non_parametric, "non_parametric_mte_results.rds")
}

## Graphs and Statistics -------------------------------------------------------
# Reading simulation results
df_results_parametric <- readRDS("parametric_mte_results.rds")
df_results_non_parametric <- readRDS("non_parametric_mte_results.rds")

# Identifying and combining datasets
df_results_parametric$model <- "parametric"
df_results_non_parametric$model <- "non_parametric"
df_results <- rbind(df_results_parametric, df_results_non_parametric)

### MTE ------------------------------------------------------------------------
# Will plot the results alongside their "bootstrap" confidence intervals for
# each v in V_grid. 

# Step 1: Calculate the median, 5th and 95th percentiles for the 90% C.I.
summary_df <- df_results %>%
   mutate(model = ifelse(model == 'parametric', 
                         'Parametric', 
                         'Nonparametric')) %>% 
   group_by(V_grid, model) %>%
   summarize(
      true_MTE = mean(true_MTE),  # Since true_MTE is constant per V_grid
      median_est_MTE = median(est_MTE),
      lower_ci = quantile(est_MTE, 0.05),
      upper_ci = quantile(est_MTE, 0.95)
   )

# Step 2: Plot the data
summary_df %>% 
   ggplot(aes(x = V_grid)) +
   # Line for true MTE
   geom_line(aes(y = true_MTE, color = 'True MTE'), size = 1) +
   geom_point(aes(y = true_MTE, color = 'True MTE'), size = 2, shape = 15) +
   
   # Line for median estimated MTE
   geom_line(aes(y = median_est_MTE, color = 'Median Estimated MTE'), 
             size = 1, linetype = 'dashed') +
   geom_point(aes(y = median_est_MTE, color = 'Median Estimated MTE'), size = 2) +
   
   # Shaded region for 90% CI
   geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, 
                   fill = 'Median Estimated MTE'), 
               alpha = 0.2, show.legend = F) +
   scale_x_continuous(breaks = V_grid) + 
   scale_y_continuous(breaks = seq(0, 6, by = 1)) + 
   labs(x = "Grid for V", y = 'MTE and 90% Simulated CIs') +
   scale_color_manual(values = c('True MTE' = '#1f77b4', 
                                 'Median Estimated MTE' = 'darkgray')) + 
   scale_fill_manual(values = c('Median Estimated MTE' = 'darkgray')) + 
   facet_wrap(~ model) +
   theme_classic() +
   theme(legend.position = 'top', 
         legend.title = element_blank(),
         legend.text = element_text(size = 11),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         strip.text = element_text(size = 12))
ggsave("figures/mte_estimators.pdf", height = 6, width = 9, dpi = 600)

### Relative Bias --------------------------------------------------------------
# Step 1: Calculate the median, 5th and 95th percentiles for the 90% C.I.
summary_df_bias <- df_results %>%
   mutate(model = ifelse(model == 'parametric', 
                         'Parametric', 
                         'Nonparametric')) %>% 
   group_by(V_grid, model) %>%
   summarize(
      mean_rel_bias = mean(rel_bias),
      median_rel_bias = median(rel_bias),
      lower_ci = quantile(rel_bias, 0.05),
      upper_ci = quantile(rel_bias, 0.95)
   )

# Step 2: Plot the data
summary_df_bias %>% 
   ggplot(aes(x = V_grid)) +
   
   # Median and mean bias
   geom_line(aes(y = median_rel_bias, color = 'Median Relative Bias'), 
             size = 1, linetype = 'dashed') +
   geom_point(aes(y = median_rel_bias, color = 'Median Relative Bias'), 
              size = 2, shape = 16) +
   geom_point(aes(y = mean_rel_bias, color = 'Mean Relative Bias'), 
              size = 2, shape = 17) +
   
   # Line for 0
   geom_hline(yintercept = 0, color = 'black', size = .5, linetype = 'dotted') +
   
   # Shaded region for 90% CI
   geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, 
                   fill = 'Median Relative Bias'), 
               alpha = 0.2, show.legend = F) +
   scale_x_continuous(breaks = V_grid) + 
   scale_y_continuous(breaks = seq(-50, 50, by = 5)) + 
   labs(x = "Grid for V", y = 'Relative Bias (in p.p.) and 90% Simulated CIs') +
   scale_color_manual(values = c('Mean Relative Bias' = '#d62728', 
                                 'Median Relative Bias' = 'darkgray')) + 
   scale_fill_manual(values = c('Median Relative Bias' = 'darkgray')) + 
   facet_wrap(~ model) +
   theme_classic() +
   theme(legend.position = 'top', 
         legend.title = element_blank(),
         legend.text = element_text(size = 11),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         strip.text = element_text(size = 12))
ggsave("figures/rel_bias_estimators.pdf", height = 6, width = 9, dpi = 600)
