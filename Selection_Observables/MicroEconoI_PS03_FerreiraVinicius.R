# Setup ========================================================================
## Libraries -------------------------------------------------------------------
# Reading files
library(readxl)
library(readr)
library(haven)

# Data
library(tidyverse)
library(magrittr)
library(glue)
library(purrr)
library(janitor)
library(tictoc)
library(MASS)

# Tables
library(modelsummary)
library(stargazer)
library(kableExtra)

# Models and fancy standard errors
library(car)
library(sandwich)
library(lmtest)
library(quantreg)

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

# Question 2: Different ATE Estimators =========================================
# Set parameters
N <- 10000  # Sample size
M <- 1000   # Number of Monte Carlo simulations

## DGPs -----------------------------------------------------------------------
dgp1 <- function(N){
   U_dgp1 <- runif(N)
   epsilon_dgp1 <- runif(N)
   
   X_dgp1 <- MASS::mvrnorm(N, mu = c(0, 0), 
                           Sigma = matrix(c(1, 0.9, 0.9, 1), 2, 2))
   
   X1_dgp1 <- pnorm(X_dgp1[, 1])
   X2_dgp1 <- pnorm(X_dgp1[, 2])
   
   D_dgp1 <- as.numeric(X1_dgp1 + (X1_dgp1 ** 2) >= 2 * U_dgp1)
   
   Y0_dgp1 <- 1 + X1_dgp1 + epsilon_dgp1
   Y1_dgp1 <- Y0_dgp1 + 2 + X1_dgp1
   
   Y_dgp1 <- ifelse(D_dgp1 == 1, Y1_dgp1, Y0_dgp1)
   
   # Creating dataframe
   data_dgp1 <- as.data.frame(list(D_dgp1, X1_dgp1, X2_dgp1, 
                                   Y_dgp1, Y0_dgp1, Y1_dgp1))
   colnames(data_dgp1) <- c("D", "X1", "X2", "Y", "Y(0)", "Y(1)")
   
   # Returning
   return(data_dgp1)
}

dgp2 <- function(N){
   U_dgp2 <- runif(N)
   epsilon_dgp2 <- runif(N)
   
   X_dgp2 <- MASS::mvrnorm(N, mu = c(0, 0), 
                           Sigma = matrix(c(1, 0.9, 0.9, 1), 2, 2))
   
   X1_dgp2 <- pnorm(X_dgp2[, 1])
   X2_dgp2 <- pnorm(X_dgp2[, 2])
   
   D_dgp2 <- as.numeric(X1_dgp2 + X2_dgp2 >= 2 * U_dgp2)
   
   Y0_dgp2 <- 1 + X1_dgp2 + X2_dgp2 + epsilon_dgp2
   Y1_dgp2 <- Y0_dgp2 + 2 + X1_dgp2 + X2_dgp2
   
   Y_dgp2 <- ifelse(D_dgp2 == 1, Y1_dgp2, Y0_dgp2)
   
   # Creating dataframe
   data_dgp2 <- as.data.frame(list(D_dgp2, X1_dgp2, X2_dgp2, 
                                   Y_dgp2, Y0_dgp2, Y1_dgp2))
   colnames(data_dgp2) <- c("D", "X1", "X2", "Y", "Y(0)", "Y(1)")
   
   # Returning
   return(data_dgp2)
}

dgp3 <- function(N){
   U_dgp3 <- runif(N)
   epsilon_dgp3 <- runif(N)
   
   X_dgp3 <- MASS::mvrnorm(N, mu = c(0, 0), 
                           Sigma = matrix(c(1, 0.9, 0.9, 1), 2, 2))
   
   X1_dgp3 <- pnorm(X_dgp3[, 1])
   X2_dgp3 <- pnorm(X_dgp3[, 2])
   
   D_dgp3 <- as.numeric(X1_dgp3 + (X1_dgp3 ** 2) >= 2 * U_dgp3)
   
   Y0_dgp3 <- 1 + X1_dgp3 + X2_dgp3 + epsilon_dgp3
   Y1_dgp3 <- Y0_dgp3 + 2 + X1_dgp3 + X2_dgp3
   
   Y_dgp3 <- ifelse(D_dgp3 == 1, Y1_dgp3, Y0_dgp3)
   
   # Creating dataframe
   data_dgp3 <- as.data.frame(list(D_dgp3, X1_dgp3, X2_dgp3, 
                                   Y_dgp3, Y0_dgp3, Y1_dgp3))
   colnames(data_dgp3) <- c("D", "X1", "X2", "Y", "Y(0)", "Y(1)")
   
   # Returning
   return(data_dgp3)
}


## ATE Estimators --------------------------------------------------------------
# Functions
estimate_ate_dgps <- function(data){
   # Sample means of covariates
   data %<>% 
      dplyr::mutate(X1_mean = mean(X1), 
                    X2_mean = mean(X2),
                    X1_demeaned = X1 - X1_mean,
                    X2_demeaned = X2 - X2_mean)
   
   # True ATE
   true_ate <- data %>% 
      dplyr::summarise(ATE = mean(`Y(1)` - `Y(0)`)) %>% 
      dplyr::pull(1)
   
   # CEF estimator
   cef_estimator <- lm(Y ~ D + X1 + D:X1_demeaned, 
                       data = data)$coefficients[[2]]
   
   # IPW 
   ## Estimating propensity scores with a logit model involving flexible
   ## functions of the covariates (see IW 2009 p. 34-35)
   model_ps <- glm(D ~ X1 + X2 + X1**2 + X2**2, 
                   family = binomial(link = 'logit'), 
                   data = data)
   
   ## Adding to dataset
   data$ps <- predict(model_ps, newdata = data, type = 'response')
   
   ## Calculating IPW estimator
   ipw_estimator <- data %>% 
      mutate(term1 = D * Y / ps, weights1 = D / ps,
             term2 = (1 - D) * Y / (1 - ps), weights2 = (1 - D) / (1 - ps)) %>% 
      dplyr::summarise(ipw = sum(term1) / sum(weights1) - 
                          sum(term2) / sum(weights2)) %>% 
      dplyr::pull(1)
   
   ## Other (maybe wrong) way to calculate IPW
   # ipw_estimator_alt <- mean((data$D * data$Y) / data$ps -
   #                             ((1 - data$D) * data$Y) / (1 - data$ps))
   
   # Doubly-Robust estimator
   ## Control group
   control_dr <- lm(Y ~ X1_demeaned, data = data %>% filter(D == 0),
                    weights = 1 / (1 - ps))
   
   ## Treatment group
   treat_dr <- lm(Y ~ X1_demeaned, data = data %>% filter(D == 1),
                  weights = 1 / ps)
   
   ## ATE estimator
   dr_estimator <- treat_dr$coefficients[[1]] - 
      control_dr$coefficients[[1]]
   
   # Naive comparison of means
   naive_estimator <- mean(data$Y[data$D == 1]) - mean(data$Y[data$D == 0])
   
   # Returning estimators
   return(list("CEF" = cef_estimator, "IPW" = ipw_estimator,
               "DR" = dr_estimator, "Naive" = naive_estimator,
               "True" = true_ate))
}

## Simulation ------------------------------------------------------------------
# Monte Carlo simulations
tic()
for (m in 1:M){
   # Generating DGPs
   data_dgp1 <- dgp1(N)
   data_dgp2 <- dgp2(N)
   data_dgp3 <- dgp3(N)
   
   # Estimating ATEs
   estimators_dgp1 <- estimate_ate_dgps(data_dgp1)
   estimators_dgp2 <- estimate_ate_dgps(data_dgp2)
   estimators_dgp3 <- estimate_ate_dgps(data_dgp3)
   
   # Storing results
   if (m == 1){
      df_estimators_dgp1 <- as.data.frame(estimators_dgp1)
      df_estimators_dgp2 <- as.data.frame(estimators_dgp2)
      df_estimators_dgp3 <- as.data.frame(estimators_dgp3)
   } else {
      df_estimators_dgp1 <- as.data.frame(rbind(df_estimators_dgp1, 
                                                estimators_dgp1))
      df_estimators_dgp2 <- as.data.frame(rbind(df_estimators_dgp2, 
                                                estimators_dgp2))
      df_estimators_dgp3 <- as.data.frame(rbind(df_estimators_dgp3, 
                                                estimators_dgp3))
   }
}
toc()  # 5 min and a half (337s)


### Relative Bias --------------------------------------------------------------
#### DGP 1 ---------------------------------------------------------------------
bias_cef_dgp1 <- df_estimators_dgp1 %>% 
   dplyr::summarise(bias = 100 * mean((CEF - True) / True)) %>% 
   pull(1)

bias_ipw_dgp1 <- df_estimators_dgp1 %>% 
   dplyr::summarise(bias = 100 * mean((IPW - True) / True)) %>% 
   pull(1)

bias_dr_dgp1 <- df_estimators_dgp1 %>% 
   dplyr::summarise(bias = 100 * mean((DR - True) / True)) %>% 
   pull(1)

bias_naive_dgp1 <- df_estimators_dgp1 %>% 
   dplyr::summarise(bias = 100 * mean((Naive - True) / True)) %>% 
   pull(1)

#### DGP 2 ---------------------------------------------------------------------
bias_cef_dgp2 <- df_estimators_dgp2 %>% 
   dplyr::summarise(bias = 100 * mean((CEF - True) / True)) %>% 
   pull(1)

bias_ipw_dgp2 <- df_estimators_dgp2 %>% 
   dplyr::summarise(bias = 100 * mean((IPW - True) / True)) %>% 
   pull(1)

bias_dr_dgp2 <- df_estimators_dgp2 %>% 
   dplyr::summarise(bias = 100 * mean((DR - True) / True)) %>% 
   pull(1)

bias_naive_dgp2 <- df_estimators_dgp2 %>% 
   dplyr::summarise(bias = 100 * mean((Naive - True) / True)) %>% 
   pull(1)

#### DGP 3 ---------------------------------------------------------------------
bias_cef_dgp3 <- df_estimators_dgp3 %>% 
   dplyr::summarise(bias = 100 * mean((CEF - True) / True)) %>% 
   pull(1)

bias_ipw_dgp3 <- df_estimators_dgp3 %>% 
   dplyr::summarise(bias = 100 * mean((IPW - True) / True)) %>% 
   pull(1)

bias_dr_dgp3 <- df_estimators_dgp3 %>% 
   dplyr::summarise(bias = 100 * mean((DR - True) / True)) %>% 
   pull(1)

bias_naive_dgp3 <- df_estimators_dgp3 %>% 
   dplyr::summarise(bias = 100 * mean((Naive - True) / True)) %>% 
   pull(1)

#### Plots ---------------------------------------------------------------------
# Combine the bias metrics into a dataframe
bias_data <- data.frame(
   DGP = rep(c("DGP1", "DGP2", "DGP3"), each = 4),
   Estimator = rep(c("CEF", "IPW", "DR", "Naive"), times = 3),
   Bias = c(
      bias_cef_dgp1, bias_ipw_dgp1, bias_dr_dgp1, bias_naive_dgp1,
      bias_cef_dgp2, bias_ipw_dgp2, bias_dr_dgp2, bias_naive_dgp2,
      bias_cef_dgp3, bias_ipw_dgp3, bias_dr_dgp3, bias_naive_dgp3
   )
)

# Create the bar plots
bias_data %>% 
   ggplot(aes(x = DGP, y = Bias, fill = Estimator)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "DGP", y = "Average Relative Bias (in %)") +
   theme_bw() + 
   theme(legend.position = 'top', legend.title = element_blank())
ggsave("figures/ate_estimators_relative_bias.pdf", 
       height = 6, width = 9, dpi = 600)

bias_data %>% 
   ggplot(aes(x = Estimator, y = Bias, fill = DGP)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Estimator", y = "Average Relative Bias (in %)") +
   theme_bw() + 
   theme(legend.position = 'top', legend.title = element_blank())
ggsave("figures/ate_estimators_relative_bias2.pdf", 
       height = 6, width = 9, dpi = 600)

# Bar plots without Naive 
bias_data %>% 
   filter(Estimator != "Naive") %>% 
   ggplot(aes(x = DGP, y = Bias, fill = Estimator)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "DGP", y = "Average Relative Bias (in %)") +
   theme_bw() + 
   theme(legend.position = 'top', legend.title = element_blank())
ggsave("figures/ate_estimators_relative_bias_wo_naive.pdf", 
       height = 6, width = 9, dpi = 600)

bias_data %>% 
   filter(Estimator != "Naive") %>% 
   ggplot(aes(x = Estimator, y = Bias, fill = DGP)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Estimator", y = "Average Relative Bias (in %)") +
   theme_bw() + 
   theme(legend.position = 'top', legend.title = element_blank())
ggsave("figures/ate_estimators_relative_bias2_wo_naive.pdf", 
       height = 6, width = 9, dpi = 600)

### Variance -------------------------------------------------------------------
#### DGP 1 ---------------------------------------------------------------------
# Calculate the variance of each estimator across the Monte Carlo simulations
var_cef_dgp1 <- df_estimators_dgp1 %>% 
   dplyr::summarise(variance = var(CEF)) %>% 
   pull(1)

var_ipw_dgp1 <- df_estimators_dgp1 %>% 
   dplyr::summarise(variance = var(IPW)) %>% 
   pull(1)

var_dr_dgp1 <- df_estimators_dgp1 %>% 
   dplyr::summarise(variance = var(DR)) %>% 
   pull(1)

var_naive_dgp1 <- df_estimators_dgp1 %>% 
   dplyr::summarise(variance = var(Naive)) %>% 
   pull(1)

#### DGP 2 ---------------------------------------------------------------------
var_cef_dgp2 <- df_estimators_dgp2 %>% 
   dplyr::summarise(variance = var(CEF)) %>% 
   pull(1)

var_ipw_dgp2 <- df_estimators_dgp2 %>% 
   dplyr::summarise(variance = var(IPW)) %>% 
   pull(1)

var_dr_dgp2 <- df_estimators_dgp2 %>% 
   dplyr::summarise(variance = var(DR)) %>% 
   pull(1)

var_naive_dgp2 <- df_estimators_dgp2 %>% 
   dplyr::summarise(variance = var(Naive)) %>% 
   pull(1)

#### DGP 3 ---------------------------------------------------------------------
var_cef_dgp3 <- df_estimators_dgp3 %>% 
   dplyr::summarise(variance = var(CEF)) %>% 
   pull(1)

var_ipw_dgp3 <- df_estimators_dgp3 %>% 
   dplyr::summarise(variance = var(IPW)) %>% 
   pull(1)

var_dr_dgp3 <- df_estimators_dgp3 %>% 
   dplyr::summarise(variance = var(DR)) %>% 
   pull(1)

var_naive_dgp3 <- df_estimators_dgp3 %>% 
   dplyr::summarise(variance = var(Naive)) %>% 
   pull(1)

# Combine the variance metrics into a dataframe
variance_data <- data.frame(
   DGP = rep(c("DGP1", "DGP2", "DGP3"), each = 4),
   Estimator = rep(c("CEF", "IPW", "DR", "Naive"), times = 3),
   Variance = c(
      var_cef_dgp1, var_ipw_dgp1, var_dr_dgp1, var_naive_dgp1,
      var_cef_dgp2, var_ipw_dgp2, var_dr_dgp2, var_naive_dgp2,
      var_cef_dgp3, var_ipw_dgp3, var_dr_dgp3, var_naive_dgp3
   )
)

# Create the bar plot for variance
variance_data %>% 
   ggplot(aes(x = Estimator, y = Variance, fill = DGP)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Estimator", y = "Variance") +
   theme_bw() + 
   theme(legend.position = 'top', legend.title = element_blank())
ggsave("figures/ate_estimators_variance.pdf", 
       height = 6, width = 9, dpi = 600)

variance_data %>% 
   ggplot(aes(x = DGP, y = Variance, fill = Estimator)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Estimator", y = "Variance") +
   theme_bw() + 
   theme(legend.position = 'top', legend.title = element_blank())
ggsave("figures/ate_estimators_variance2.pdf", 
       height = 6, width = 9, dpi = 600)


# Question 3: Quantile Regression ==============================================
# Loading dataset
data(engel)

# Estimating effects for the median
fit_median <- quantreg::rq(foodexp ~ income, data = engel, tau = 0.5)
summary(fit_median)

# Estimating effects for all ventiles
taus <- seq(0.05, .95, by = 0.05)
fit_ventiles <- quantreg::rq(foodexp ~ income, data = engel, tau = taus)
summary_fit_ventiles <- summary(fit_ventiles)

# Extracting coefficients and confidence intervals
## Both of them
coef_data <- do.call(rbind, lapply(summary_fit_ventiles, function(x) {
   coef_df <- as.data.frame(x$coefficients)
   coef_df$tau <- x$tau
   coef_df$term <- rownames(coef_df)
   return(coef_df)
}))

## Filterng for intercept and income
intercept_data <- coef_data %>% filter(term == "(Intercept)")
income_data <- coef_data %>% filter(term == "income")

# Plotting intercept
intercept_data %>% 
ggplot(aes(x = tau, y = coefficients)) +
   geom_point(color = '#1f77b4', size = 3) +
   geom_errorbar(aes(ymin = `lower bd`, ymax = `upper bd`), width = 0.02,
                 color = '#1f77b4') +
   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
   labs(x = "Tau (Ventiles)", y = "Intercept") +
   theme_bw()
ggsave("figures/quantile_intercept.pdf", height = 6, width = 9, dpi = 600)

# Plotting intercept
income_data %>% 
   ggplot(aes(x = tau, y = coefficients)) +
   geom_point(color = '#e377c2', size = 3) +
   geom_errorbar(aes(ymin = `lower bd`, ymax = `upper bd`), width = 0.02,
                 color = '#e377c2') +
   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
   labs(x = "Tau (Ventiles)", y = "Income Coefficient") +
   theme_bw()
ggsave("figures/quantile_income.pdf", height = 6, width = 9, dpi = 600)

# Plotting conditional quantile functions
## Extracting coefficients
coefficients <- as.data.frame(t(coef(fit_ventiles)))
coefficients$tau <- taus

# #Creating a plot with a gradient for the quantile lines
engel %>% 
   ggplot(aes(x = income, y = foodexp)) + 
   geom_point(color = 'gray') + 
   ## Quantile conditional functions
   geom_abline(aes(intercept = coefficients$`(Intercept)`, 
                   slope = coefficients$income, 
                   color = tau), 
               data = coefficients, size = 0.33) +
   ## Median
   geom_abline(aes(intercept = coefficients$`(Intercept)`[taus == 0.5], 
                   slope = coefficients$income[taus == 0.5]), 
               color = "black", size = .75, linetype = "solid") +
   ## Conditional expectation function
   geom_smooth(method = 'lm', linetype = 'dashed', 
               color = 'black', se = FALSE, size = .75) + 
   labs(x = "Household Income", y = "Food Expenditure") + 
   scale_color_gradient(low = "lightblue", high = "darkblue") +
   labs(color = "Quantile")+ 
   theme_bw() + 
   theme(legend.position = 'top')
ggsave("figures/conditional_quantiles_functions.pdf", 
       height = 6, width = 9, dpi = 600)
