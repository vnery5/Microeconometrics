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

# Tables
library(modelsummary)
library(stargazer)
library(kableExtra)

# Models and fancy standard errors
library(car)
library(sandwich)
library(lmtest)

# Non-parametric and RDD stuff
library(nprobust)
library(rdrobust)
library(rddensity)
library(RATest)

## Useful Stuff ----------------------------------------------------------------
# Useful commands for cleaning the working environment
rm(list = ls())
cat("\014")

# Directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
str_base_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

str_data_directory <- paste(str_base_directory, "data", sep = "/")
str_figure_directory <- paste(str_base_directory, "figures", sep = "/")
str_table_directory <- paste(str_base_directory, "tables", sep = "/")

# Seed
set.seed(121019)

# Question 1: Regression Discontinuity Design ==================================
# Reading data
df_senate <- read_csv(paste0(str_data_directory, "/rdrobust_senate.csv"))

# Changing types
df_senate %<>% 
   mutate(state = as.factor(state), 
          year = as.factor(year),
          class = as.factor(class)
          ) %>% 
   as.data.frame()

## a. Summary Statistics -------------------------------------------------------
# Separate numeric and factor columns
numeric_cols <- sapply(df_senate, is.numeric)
factor_cols <- sapply(df_senate, is.factor)

# Numeric Variables
latex_table_numeric <- kable(summary(df_senate[, numeric_cols]), 
                     format = "latex", booktabs = TRUE) %>%
   kable_styling(latex_options = c("striped", "hold_position"))

write(latex_table_numeric, 
      file = paste0(str_table_directory, "/summary_statistics_table_numeric.tex"))

# Factor Variables
latex_table_factor <- kable(summary(df_senate[, factor_cols]), 
                            format = "latex", booktabs = TRUE) %>%
   kable_styling(latex_options = c("striped", "hold_position"))

write(latex_table_factor, 
      file = paste0(str_table_directory, "/summary_statistics_table_factor.tex"))

## b. Graphical Analysis -------------------------------------------------------
# The graphical package is BAD; will get data and construct the graph myself

# Object
rd_plot_object <- rdplot(y = df_senate$vote, x = df_senate$margin, c = 0,
       # kernel = 'epanechnikov',
       # scale = 5,
       binselect = "es", # IMSE-optimal
       hide = TRUE)

# Plotting
ggplot() + 
   geom_point(data = rd_plot_object$vars_bins,
              aes(x = rdplot_mean_bin, y = rdplot_mean_y),
              color = 'black', size = 2) + 
   geom_line(data = rd_plot_object$vars_poly %>%
                mutate(rdplot_y = ifelse(rdplot_x == 0, NA, rdplot_y)),
             aes(x = rdplot_x, y = rdplot_y),
             color = "#1f77b4", size = 1) + 
   geom_vline(xintercept = 0, color = 'darkgray', linetype = 'dashed') + 
   
   geom_ribbon(data = rd_plot_object$vars_bins %>% 
                  dplyr::filter(rdplot_mean_bin < 0), 
               aes(x = rdplot_mean_bin, ymin = rdplot_ci_l, ymax =rdplot_ci_r),
               alpha = .2) + 
   geom_ribbon(data = rd_plot_object$vars_bins %>% 
                  dplyr::filter(rdplot_mean_bin >= 0), 
               aes(x = rdplot_mean_bin, ymin = rdplot_ci_l, ymax =rdplot_ci_r),
               alpha = .2) + 
   
   geom_smooth(data = df_senate,
               aes(x = margin, y = vote), 
               method = 'lm', se = FALSE, color = 'red', linetype = 'dashed') + 
   
   scale_x_continuous(breaks = seq(-100, 100, by = 20)) + 
   scale_y_continuous(breaks = seq(0, 100, by = 10)) + 
   labs(x = "Vote Margin in Election at t",
        y = "Vote Share in Election at t + 1") + 
   theme_classic() + 
   theme(legend.position = 'none', 
         axis.text.x = element_text(size = 11),
         axis.text.y = element_text(size = 11),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12))
ggsave("figures/rdplot_shade.pdf", height = 6, width = 9, dpi = 600)

ggplot() + 
   geom_point(data = rd_plot_object$vars_bins,
              aes(x = rdplot_mean_bin, y = rdplot_mean_y),
              color = 'black', size = 2) + 
   geom_line(data = rd_plot_object$vars_poly %>%
                mutate(rdplot_y = ifelse(rdplot_x == 0, NA, rdplot_y)),
             aes(x = rdplot_x, y = rdplot_y),
             color = "#1f77b4", size = 1) + 
   geom_vline(xintercept = 0, color = 'darkgray', linetype = 'dashed') + 
   
   geom_errorbar(data = rd_plot_object$vars_bins, 
               aes(x = rdplot_mean_bin, ymin = rdplot_ci_l, ymax =rdplot_ci_r)) + 
   
   geom_smooth(data = df_senate,
               aes(x = margin, y = vote), 
               method = 'lm', se = FALSE, color = 'red', linetype = 'dashed') + 
   
   scale_x_continuous(breaks = seq(-100, 100, by = 20)) + 
   scale_y_continuous(breaks = seq(0, 100, by = 10)) + 
   labs(x = "Vote Margin in Election at t",
        y = "Vote Share in Election at t + 1") + 
   theme_classic() + 
   theme(legend.position = 'none', 
         axis.text.x = element_text(size = 11),
         axis.text.y = element_text(size = 11),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12))
ggsave("figures/rdplot_error_bar.pdf", height = 6, width = 9, dpi = 600)

## c. Treatment Effect ---------------------------------------------------------
# We are in a sharp RDD setting, so don't need to specify treatment variable
# and its associated probabilities.
# Sharp: only gets elected (treated) if margin > 0.

# Bandwidth automatically selected by the package (MSE optimal). 
# Epa. kernel (default is triangular)
# heteroskedasticity-robust nearest neighbor variance estimator 
rd_results <- rdrobust(y = df_senate$vote, x = df_senate$margin, c = 0,
                       p = 2, q = 3,
                       kernel = "epanechnikov")
summary(rd_results)

# Tried to use covariates (even removing missing values), but kept getting error

## d. Density Test -------------------------------------------------------------
# Implement McCrary’s Density Test using rddensity
rd_manipulation <- rddensity(X = df_senate$margin, c = 0, 
                             p = 2, kernel = "epanechnikov")
summary(rd_manipulation)

## e. Visual Density Test ------------------------------------------------------
rdplotdensity(rdd = rd_manipulation,
              X = df_senate$margin,
              plotGrid = "es",
              type = 'both',
              lwd = 1,
              pty = 16,
              pwd = 2,
              pcol = c('#1f77b4', '#ff7f0e'),
              lcol = c('#1f77b4', '#ff7f0e'),
              histFillCol = 'darkgray',
              histLineCol = 'darkgray',
              xlabel = "Margin at Previous Election",
              ylabel = "Density")
ggsave("figures/rd_density_plot_manipulation.pdf", 
       height = 6, width = 9, dpi = 600)

## f. Balance Test -------------------------------------------------------------
# Will use the permutation test of Canay and Kamat (2017).
# Class is a factor, so will need to construct columns of indicator variables
df_senate %<>% 
   mutate(class1 = ifelse(class == 1, 1, 0),
          class2 = ifelse(class == 2, 1, 0),
          class3 = ifelse(class == 3, 1, 0))

# Test
rd_permutation_test <- RATest::RDperm(
   W = c("population", "class1", "class2", "class3"),
   z = "margin",
   cutoff = 0,
   data = df_senate)

rd_permutation_test2 <- RATest::RDperm(
   W = c("population", "class"),
   z = "margin",
   cutoff = 0,
   data = df_senate %>% mutate(class = as.numeric(class)))

# Summaries: p-value of joint test is big, no signs of discontinuities
summary(rd_permutation_test)
summary(rd_permutation_test2)

# Plots
plot(rd_permutation_test, w = 'population')

# With all covariates
df_senate_no_missing <- df_senate %>% 
   dplyr::filter(!is.na(termshouse), !is.na(vote), !is.na(termssenate))

rd_permutation_test_all <- RATest::RDperm(
   W = c("population", "class1", "class2", "class3", 
         "termshouse", "termssenate"),
   z = "margin",
   cutoff = 0,
   data = df_senate_no_missing)

summary(rd_permutation_test_all)

rd_permutation_test_all2 <- RATest::RDperm(
   W = c("population", "class", 
         "termshouse", "termssenate"),
   z = "margin",
   cutoff = 0,
   data = df_senate_no_missing %>% mutate(class = as.numeric(class)))

summary(rd_permutation_test_all2)
