# Setup ========================================================================
## Libraries -------------------------------------------------------------------
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

# Question 1: Nonparametric and Semiparametric Regressions =====================
# Set parameters
N <- 10000  # Sample size

## 1. DGP ----------------------------------------------------------------------
dgp <- function(N, beta1, beta2){
   epsilon1 <- rnorm(N)
   epsilon2 <- rnorm(N)
   
   X <- MASS::mvrnorm(N, mu = c(0, 0), 
                      Sigma = matrix(c(1, .9, .9, 1), nrow = 2, ncol = 2))
   
   X1 <- pnorm(X[, 1])
   X2 <- pnorm(X[, 2])
   
   f <- sin(beta1 * X1)
   g <- sin(beta1 * X1) + beta2 * X2
   
   Y1 <- f + epsilon1
   Y2 <- g + epsilon2
   
   # Creating dataframe
   data_dgp <- as.data.frame(list(X1, X2, Y1, Y2, f, g))
   colnames(data_dgp) <- c("X1", "X2", "Y1", "Y2", "f", "g")
   
   # Returning
   return(data_dgp)
}

# Simulating 
data <- dgp(N, beta1 = 4, beta2 = 2)

## 2. Function f ---------------------------------------------------------------
data %>% 
   ggplot(aes(x = X1, y = f)) + 
   geom_line(linewidth = 1) + 
   labs(x = expression(X[1]), y = expression(f(X[1]))) + 
   theme_bw()
ggsave("figures/item2_f.pdf", height = 6, width = 9, dpi = 600)

## 3. Estimating f with a nonparametric linear regression using Y1 and X1 ------
# Using nprobust::lprobust with all the default values
tic()
np_est_f_Y1 <- lprobust(y = data$Y1, x = data$X1)
toc() # 30s

## 4. Plotting estimated function ----------------------------------------------
# Data
np_est_f_Y1_data <- as.data.frame(np_est_f_Y1$Estimate)

# Using built-in function for base graph (already uses bias-corrected statistics)
g_est_f_Y1 <- nprobust.plot(np_est_f_Y1, alpha = .05, type = 'line',
                         lty = 2, lwd = 1, pwd = 2,
                         lcol = '#ff7f0e', pcol = '#ff7f0e',
                         CIshade = .5, CIcol = '#ff7f0e',
                         legendGroups = "Estimated")
# Adding actual function
g_est_f_Y1 <- g_est_f_Y1 +
   geom_line(data = data, aes(x = X1, y = f), linewidth = 1, color = 'black') +
   labs(x = expression(X[1]), y = expression(f(X[1]))) + 
   theme_bw() + 
   theme(legend.position = "none")

g_est_f_Y1
ggsave("figures/item4_estimated_f_Y1.pdf", height = 6, width = 9, dpi = 600)

## 5. Estimating f with a nonparametric linear regression using Y2 and X1 ------
# Using nprobust::lprobust with all the default values
tic()
np_est_f_Y2 <- lprobust(y = data$Y2, x = data$X1)
toc() # 38s

## 6. Plotting estimated function ----------------------------------------------
# Data
np_est_f_Y2_data <- as.data.frame(np_est_f_Y2$Estimate)

# Using built-in function for base graph (already uses bias-corrected statistics)
g_est_f_Y2 <- nprobust.plot(np_est_f_Y2, alpha = .05, type = 'line',
                            lty = 2, lwd = 1, pwd = 2,
                            lcol = '#1f77b4', pcol = '#1f77b4',
                            CIshade = .5, CIcol = '#1f77b4',
                            legendGroups = "Estimated")
# Adding actual function
g_est_f_Y2 <- g_est_f_Y2 +
   geom_line(data = data, aes(x = X1, y = f), linewidth = 1, color = 'black') +
   labs(x = expression(X[1]), y = expression(f(X[1]))) + 
   theme_bw() + 
   theme(legend.position = "none")

g_est_f_Y2
ggsave("figures/item6_estimated_f_Y2.pdf", height = 6, width = 9, dpi = 600)


## 7. Estimating f with a semiparametric linear regression using Y2, X1, X2 ----
# Will follow the steps described in "Estimating Model 4" in the Lecture Notes
# Our 'treatment' variable is X1, while the parametric covariate is X2

### (a) ----
# Fitting a nonparametric local linear regression of X2 on X1
# Have to use all grid points for X1

# Ordering the points by X1
data %<>% arrange(X1)
   
# Estimating
tic()
np_est_f_X2_on_X1 <- lprobust(y = data$X2, x = data$X1, neval = N)
toc() # 230s (4min)

### (b) ----
# Collecting residuals (in the plot, they plot usual estimates with
# bias corrected standard errors)
np_est_f_X2_on_X1_residuals <- data$X2 - np_est_f_X2_on_X1$Estimate[, 'tau.us']

### (c) ----
# Fitting a nonparametric local linear regression of Y2 on X1
tic()
np_est_f_Y2_on_X1 <- lprobust(y = data$Y2, x = data$X1, neval = N)
toc() # 3min

### (d) ----
# Collecting residuals
np_est_f_Y2_on_X1_residuals <- data$Y2 - np_est_f_Y2_on_X1$Estimate[, 'tau.us']

### (e) ----
# Estimating B (parametric coefficient of the covariates (X2)) with
# a non-parametric version of the FWL
parametric_model <- lm(np_est_f_Y2_on_X1_residuals ~ 
                          -1 + np_est_f_X2_on_X1_residuals)
(beta2_est <- parametric_model$coefficients[[1]])  # 1.971068
summary(parametric_model)

### (f) ----
# Residualizing Y from its parametric part
data$residualized_Y2_parametric <- data$Y2 - data$X2 * beta2_est

### (g) ----
# Estimating f with a local linear regression
tic()
np_est_f_final <- lprobust(y = data$residualized_Y2_parametric, 
                           x = data$X1)
toc() # 30s


## 8. Plotting estimated function ----------------------------------------------
# Data
np_est_f_final_data <- as.data.frame(np_est_f_final$Estimate)

# Using built-in function for base graph (already uses bias-corrected statistics)
g_est_f_final <- nprobust.plot(np_est_f_final, alpha = .05, type = 'line',
                            lty = 2, lwd = 1, pwd = 2,
                            lcol = '#d62728', pcol = '#d62728',
                            CIshade = .5, CIcol = '#d62728',
                            legendGroups = "Estimated")
# Adding actual function
g_est_f_final <- g_est_f_final +
   geom_line(data = data, aes(x = X1, y = f), linewidth = 1, color = 'black') +
   labs(x = expression(X[1]), y = expression(f(X[1]))) + 
   theme_bw() + 
   theme(legend.position = "none")

g_est_f_final
ggsave("figures/item8_estimated_f_Y2_on_X1_X2.pdf", 
       height = 6, width = 9, dpi = 600)
