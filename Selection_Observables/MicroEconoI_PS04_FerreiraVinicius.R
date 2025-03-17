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

# Specification Tests
library(pstest)
library(sensemakr)

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

# Question 1: Specification Tests ==============================================
#' Will use the pstest from Sant'anna and Song (2019) to verify if propensity
#' score specifications are correct

# Reading data
data_q1 <- read_csv("data/WTOdata.csv")

# Propensity score specifications
## Their package requires that we manually pass the interaction terms and the
## constant, so we create them already
data_q1 %<>% 
   mutate(constant = 1,
          rgdpch_areap = rgdpch * areap,
          rgpdch_polity = rgdpch * polity,
          areap_polity = areap * polity)

## Formulas
formula_spec1 <- gattwto ~ 1 + rgdpch + areap + polity

formula_spec2 <- gattwto ~ 1 + rgdpch + areap + polity + 
   rgdpch_areap + rgpdch_polity + areap_polity

# Sant'anna and Song use a probit to model scores, so we follow them
model_ps_spec1 <- glm(formula_spec1, 
                      family = binomial(link = 'probit'),
                      data = data_q1)

model_ps_spec2 <- glm(formula_spec2, 
                      family = binomial(link = 'probit'),
                      data = data_q1)

# Adding propensity scores to dataframe
data_q1$ps_spec1 <- predict(model_ps_spec1, newdata = data_q1, type = 'response')
data_q1$ps_spec2 <- predict(model_ps_spec2, newdata = data_q1, type = 'response')

# Calling the pstest function exactly as they do, but with fewer bootstraps
## Spec1
pstest_spec1 <- pstest(d = data_q1$gattwto, 
                       pscore = data_q1$ps_spec1,
                       xpscore = data_q1 %>% 
                          dplyr::select(constant, rgdpch, areap, polity),
                       model = 'probit',
                       pscore.model = model_ps_spec1,
                       nboot = 10000)

### Summary
summary(pstest_spec1)

## Spec2
pstest_spec2 <- pstest(d = data_q1$gattwto, 
                       pscore = data_q1$ps_spec2,
                       xpscore = data_q1 %>% 
                          dplyr::select(constant, rgdpch, areap, polity,
                                        rgdpch_areap, rgpdch_polity, areap_polity),
                       model = 'probit',
                       pscore.model = model_ps_spec2,
                       nboot = 10000)

### Summary
summary(pstest_spec2)

# Constructing Dataframe to export
## For each specification
spec1_results <- as.data.frame(list(
   "CvM Statistic" = pstest_spec1$cvmtest,
   "CvM P-Value" = pstest_spec1$pvcvm,
   "KS Statistic" = pstest_spec1$kstest,
   "KS P-Value" = pstest_spec1$pvks
))

spec2_results <- as.data.frame(list(
   "CvM Statistic" = pstest_spec2$cvmtest,
   "CvM P-Value" = pstest_spec2$pvcvm,
   "KS Statistic" = pstest_spec2$kstest,
   "KS P-Value" = pstest_spec2$pvks
))

## Binding results
spec_results <- rbind(spec1_results, spec2_results)

## Transposing and rounding
spec_results %<>% 
   t() %>% 
   as.data.frame() %>% 
   mutate(across(where(is.numeric), ~ round(., 2)))

## Column names
colnames(spec_results) <- c("Specification 1", "Specification 2")

## Stargazer
stargazer(spec_results, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Propensity Score Specification Test Results",
          out = paste(str_table_directory, "ps_results_q1.tex", sep = "/"))

#' We see that, when using the CvM Statistic, we have evidence that the first
#' specification is not correctly specified, as we have a p-value of 0.01.
#' This evidence is less strong when using the KS Statistic.
#' For both statistics, we don't have evidence to reject the null that the
#' second specification is correctly specified.


# Question 2: Sensitivity Analysis =============================================
#' Will use the sensemakr from Cinelli and Hazlet (2020) to verify if their
#' results are robust to misspecification, in the sense that there aren't
#' unobservable confounders that are strong enough to overturn the findings

# Reading data
data_q2 <- read_csv("data/darfur.csv")

# runs regression model
model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
               pastvoted + hhsize_darfur + female + village, data = data_q2)

# runs sensemakr for sensitivity analysis
sensitivity <- sensemakr(model, treatment = "directlyharmed",
                         benchmark_covariates = "female",
                         kd = 1:3)
# short description of results
sensitivity

# long description of results
summary(sensitivity)

# Cinelli and Hazlet are interested in the effect of being in an 
# attacked village on individual's support of peace in Sudan's western region
# of Darfur. They argue that attacks were indiscriminate within a given village,
# with the exception that woman suffered more than man.
# Thus, controlling for village and sex should make CIA hold
# Their sensitivity analysis aims to measure how strong an unobservable
# confounder ought to be to completely erase the results.
# For better interpretability, they display the results in terms of a baseline
# covariate, which is being a woman in our case.

# Their main formula (Equation 1) is
formula_q2 <- peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
   pastvoted + hhsize_darfur + female + factor(village)

# Running the model
model_q2 <- lm(formula_q2, data = data_q2)

# Sensitivity analysis
sensitivity <- sensemakr(model_q2, treatment = "directlyharmed",
                         benchmark_covariates = "female",
                         kd = 1:3)

# Results
## Short form (this is their Table 1)
sensitivity

## Long form
summary(sensitivity)

## Table 1 ---------------------------------------------------------------------
#' Taking their argument as credible, having suffered from violence in the
#' village attacks increases support of peace by about 10% on their index.
#' This estimate is significant, with a t-statistic of 4.18 when assuming
#' homoskedasticity.
#' 
#' The Robustness Value (RV) is 13.87\%. This means that unobservable confounders
#' would have to explain 13.87\% of the residual variance (residual: that is 
#' not explained by the other covariates) of both the treatment
#' and the outcome in order for the estimate of DirectHarm to go to zero.
#' Note that this implies that any confounder that is 'weaker' than this
#' is not capable to completely erase the results.
#' 
#' However, we usually are not interested in the cases where confounders are
#' strong enough to completely erase the effect we are interested in, but whether
#' they can affect the significance of the result at usual significance levels
#' (in our case, 5%).
#' For that to be the case, the unobservable confounders would have to explain
#' about 7.6% of the residual variance of both the treatment and the outcome.
#' 
#' Finally, the Partial R^2 of treatment with outcome is 2.2% (after controlling
#' for the other covariates, treatment explains 2.2% of the peace index 
#' variability). They show that this is also the percentage of the residual
#' variance of treatment that an extreme confounder who explains 
#' 100\% of the residual variance of the outcome would need to be responsible 
#' for in order to bring the effect measured by our estimator to zero. 
#' 
#' Note that, at the end, robustness to misspecification is determined by the 
#' share of variation of the outcome that the treatment uniquely explains.
#' 
#' The footnote tells us what would be the sensitivity statistics of a 
#' unobservable confounder that is as strong as the Female indicator.
#' This confounder would explain about 12.5% of the residual variance of the
#' outcome, while only 0.9% of the residual variance of the treatment.
#' Since the RV is higher than both statistics, it would not be enough
#' to erase all results. Furthermore, since 2.2% is higher than 0.9%,
#' a extreme confounder that explains 100\% of the residual variance of the
#' outcome and that is as strong as female in terms of association to treatment
#' would not be able to completely erase the results.

# Reproducing the body of Table 1
ovb_minimal_reporting(sensitivity)

## Figure 2 --------------------------------------------------------------------
# Figure 2a: contour plot of the point estimate
plot(sensitivity)

# Figure 2b: contour plot of the t-statistic
plot(sensitivity, sensitivity.of = "t-value")

# Contour plots of confidence intervals
plot(sensitivity, sensitivity.of = "lwr")
plot(sensitivity, sensitivity.of = "upr")

#' Figure 2a plots the contour lines of the point estimate.
#' That is, what would be the point estimate of DirectHarm if confounders
#' explained a given amount of the residual variance of treatment (horizontal
#' axis) and of the outcome (vertical axis).
#' We see that not even a confounder three times as strong as female would be 
#' enough to completely erase the estimated effect.
#' However, the point estimate could be reduced to as low as 0.03 from the 
#' original value of 0.097.

#' Figure 2b shows the contour lines of the t-statistic.
#' That is, what would be the t-statistic of the coefficient of DirectHarm 
#' if confounders explained a given amount of the residual variance of treatment 
#' (horizontal axis) and of the outcome (vertical axis).
#' We see that not even a confounder two times as strong as female would be 
#' enough to overturn the signifificance of our result at the usual 5\% level.
#' However, a confounder three times as strong would inflate the variance of
#' the estimator in such a way that the estimate would not be significant at
#' the 5\% level, being marginally significant at the 10\% level (t-stat of 1.64),
#' This becomes more apparent when looking at the contour plot for the 
#' lower bound of the confidence interval.

#' Overall, results are relatively robust to misspecification, as we would need
#' extremely strong confounders that are not observable in order to overturn the
#' point estimate and the significance of the results.
#' Of course, the existence of such a variable is impossible to verify, but the
#' tests give some reassurance and steers the discussion in a more disciplined,
#' quantitative direction.

# For fun, also replicating Figure 3 (extreme scenario)
plot(sensitivity, type = "extreme")

