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

# Tables
library(modelsummary)
library(stargazer)
library(kableExtra)

# Models and fancy standard errors
library(leebounds)
library(car)
library(sandwich)
library(lmtest)

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

# Question 1: Data Cleaning ====================================================
# Reading data
data <- haven::read_dta("base_lotteries.dta")

# Data has only those who participated in the first lottery
# Important: we have perfect compliance, so won lottery = enrollment
# Treated: won the lottery in that or in previous year
# Control: didn't win the lottery 

# We are assuming those that enrolled in the 8th stay enrolled in the 9th,
# and the only of enrollment in 9th grade for those that participated in 
# and lost the 8th grade lottery is through the 9th grade lottery

# Creating indicator of treatment
data %<>% 
   mutate(treated8 = ifelse(won_lotteryA == 1, 1, 0),
          treated9 = ifelse((treated8 == 1) | (won_lotteryB == 1), 1, 0))

# Calculating means and standard deviations
## When using standard deviations, we assume variance of the mean is known

# 7th grade scores
data %<>% 
   mutate(score_lp_7_mean = mean(score_lp_7, na.rm = TRUE),
          score_lp_7_sd = sd(score_lp_7, na.rm = TRUE),
          score_lp_7_std = (score_lp_7 - score_lp_7_mean) / score_lp_7_sd,
          score_mt_7_mean = mean(score_mt_7, na.rm = TRUE),
          score_mt_7_sd = sd(score_mt_7, na.rm = TRUE),
          score_mt_7_std = (score_mt_7 - score_mt_7_mean) / score_mt_7_sd) %>% 
   dplyr::select(-score_lp_7_mean, -score_lp_7_sd,
                 -score_mt_7_mean, -score_mt_7_sd)

# For the 8th and 9th grade, we only use control statistics
## 8th grade
data_8th_grade_stats <- data %>% 
   filter(treated8 == 0) %>% 
   summarise(score_lp_8_mean = mean(score_lp_8, na.rm = TRUE),
             score_lp_8_sd = sd(score_lp_8, na.rm = TRUE),
             score_mt_8_mean = mean(score_mt_8, na.rm = TRUE),
             score_mt_8_sd = sd(score_mt_8, na.rm = TRUE))

## 9th grade
data_9th_grade_stats <- data %>% 
   filter((treated8 == 0) & (treated9 == 0)) %>% 
   summarise(score_lp_9_mean = mean(score_lp_9, na.rm = TRUE),
             score_lp_9_sd = sd(score_lp_9, na.rm = TRUE),
             score_mt_9_mean = mean(score_mt_9, na.rm = TRUE),
             score_mt_9_sd = sd(score_mt_9, na.rm = TRUE))

## Adding information to main dataframe
data$score_lp_8_std <- (data$score_lp_8 - data_8th_grade_stats$score_lp_8_mean) /
   data_8th_grade_stats$score_lp_8_sd

data$score_mt_8_std <- (data$score_mt_8 - data_8th_grade_stats$score_mt_8_mean) /
   data_8th_grade_stats$score_mt_8_sd

data$score_lp_9_std <- (data$score_lp_9 - data_9th_grade_stats$score_lp_9_mean) /
   data_9th_grade_stats$score_lp_9_sd

data$score_mt_9_std <- (data$score_mt_9 - data_9th_grade_stats$score_mt_9_mean) /
   data_9th_grade_stats$score_mt_9_sd

# Question 2: Balance Tests ====================================================
# Our covariates are sex, race, mother_schooling, father_schooling, ever_failed
# Due to them being factors, we create indicators for better interpretation

## Indicators ------------------------------------------------------------------
# Dataframe with only the covariates and treated columns
data_covariates <- data %>% 
   dplyr::select(student_code, sex, race, mother_schooling, 
                 father_schooling, ever_failed_school,
                 score_lp_7, score_mt_7, score_lp_7_std, score_mt_7_std,
                 won_lotteryA, won_lotteryB, lotteryB)

# Exploding the 'sex' column into 'sex_female' and 'sex_male'
data_covariates %<>% 
   mutate(
      sex_male = ifelse(sex == 1, 1, 0),
      sex_female = ifelse(sex == 2, 1, 0)
   )

# Exploding the 'race' column into indicator columns
data_covariates %<>% 
   mutate(
      race_white = ifelse(race == 1, 1, 0),
      race_brown = ifelse(race == 2, 1, 0),
      race_black = ifelse(race == 3, 1, 0),
      race_yellow = ifelse(race == 4, 1, 0),
      race_indigenous = ifelse(race == 5, 1, 0)
   )

# Exploding the 'mother_schooling' column into indicator columns
data_covariates %<>% 
   mutate(
      mother_schooling_less_5th = ifelse(mother_schooling == 1, 1, 0),
      mother_schooling_5th_9th = ifelse(mother_schooling == 2, 1, 0),
      mother_schooling_9th_HS = ifelse(mother_schooling == 3, 1, 0),
      mother_schooling_HS_College = ifelse(mother_schooling == 4, 1, 0),
      mother_schooling_College = ifelse(mother_schooling == 5, 1, 0),
      mother_schooling_above_College = ifelse(mother_schooling == 6, 1, 0),
      mother_schooling_unknown = ifelse(mother_schooling == 7, 1, 0)
   )

# Exploding the 'father_schooling' column into indicator columns
data_covariates %<>% 
   mutate(
      father_schooling_less_5th = ifelse(father_schooling == 1, 1, 0),
      father_schooling_5th_9th = ifelse(father_schooling == 2, 1, 0),
      father_schooling_9th_HS = ifelse(father_schooling == 3, 1, 0),
      father_schooling_HS_College = ifelse(father_schooling == 4, 1, 0),
      father_schooling_College = ifelse(father_schooling == 5, 1, 0),
      father_schooling_above_College = ifelse(father_schooling == 6, 1, 0),
      father_schooling_unknown = ifelse(father_schooling == 7, 1, 0)
   )

# Exploding the 'ever_failed' column into indicator columns
data_covariates %<>% 
   mutate(
      ever_failed_never = ifelse(ever_failed_school == 1, 1, 0),
      ever_failed_once = ifelse(ever_failed_school == 2, 1, 0),
      ever_failed_more_than_once = ifelse(ever_failed_school == 3, 1, 0)
   )

# Select only indicator columns and baseline scores
data_covariates %<>% 
   dplyr::select(-sex, -race, -mother_schooling, 
                 -father_schooling, -ever_failed_school)

## (a), (c) --------------------------------------------------------------------
### Tests ----------------------------------------------------------------------
# Function to calculate means and p-values for each variable
t_test_means <- function(df, group_var) {
   df %>%
      summarise(across(
         -all_of(group_var),
         list(
            mean_treated = ~ mean(.x[!!sym(group_var) == 1], na.rm = TRUE),
            se_treated = ~ sd(.x[!!sym(group_var) == 1], na.rm = TRUE) / 
               sqrt(sum(!is.na(.x[!!sym(group_var) == 1]))),
            
            mean_control = ~ mean(.x[!!sym(group_var) == 0], na.rm = TRUE),
            se_control = ~ sd(.x[!!sym(group_var) == 0], na.rm = TRUE) / 
               sqrt(sum(!is.na(.x[!!sym(group_var) == 0]))),
            
            difference = ~ mean(.x[!!sym(group_var) == 1], na.rm = TRUE) - 
               mean(.x[!!sym(group_var) == 0], na.rm = TRUE),
            
            se_difference = ~ t.test(.x ~ df[[group_var]], 
                                     alternative = 'two.sided',
                                     var.equal = FALSE,
                                     conf.level = .9)$stderr,
            p_value = ~ t.test(.x ~ df[[group_var]], 
                               alternative = 'two.sided',
                               var.equal = FALSE,
                               conf.level = .9)$p.value
         ),
         .names = "{col}__{fn}"
      )) %>%
      pivot_longer(cols = everything(),
                   names_to = c(".value", "variable"),
                   names_pattern = "(.*)__(.*)")
}

# Apply the function to get means and p-values
## 8th grade
data_balance_8th_grade <- t_test_means(
   data_covariates %>% select(-student_code, -won_lotteryB, -lotteryB),
   group_var = "won_lotteryA"
)

## 9th grade (control: participated in lottery and lost)
data_balance_9th_grade <- t_test_means(
   data_covariates %>% 
      filter(lotteryB == 1) %>% 
      select(-student_code, -won_lotteryA, -lotteryB),
   group_var = "won_lotteryB"
)

## Lottery B (only those that didn't win the first)
data_balance_lotteryB <- t_test_means(
   data_covariates %>% 
      filter(won_lotteryA == 0) %>% 
      select(-student_code, -won_lotteryA, -won_lotteryB),
   group_var = "lotteryB"
)

### Latex ----------------------------------------------------------------------
# Transposing, rounding and adding stars so it is pretty
data_balance_8th_grade %<>% 
   mutate(across(is.numeric, round, digits = 2)) %>% 
   t %>%
   as.data.frame %>% 
   row_to_names(row_number = 1) %>% 
   mutate(p_value = as.double(p_value),
          p_value = ifelse(p_value > .1, 
                           p_value,
                           ifelse(p_value > .05, 
                                  paste0(p_value, '*'),
                                  paste0(p_value, '**')
                           )
   )
   ) %>% 
   rename_all(~paste0(.x, "_8th"))
   
data_balance_9th_grade %<>% 
   mutate(across(is.numeric, round, digits = 2)) %>% 
   t %>%
   as.data.frame %>% 
   row_to_names(row_number = 1) %>% 
   mutate(p_value = as.numeric(p_value),
          p_value = ifelse(p_value > .1, 
                           p_value,
                           ifelse(p_value > .05, 
                                  paste0(p_value, '*'),
                                  paste0(p_value, '**')
                           )
          )
   ) %>% 
   rename_all(~paste0(.x, "_9th"))

data_balance_lotteryB %<>% 
   mutate(across(is.numeric, round, digits = 2)) %>% 
   t %>%
   as.data.frame %>% 
   row_to_names(row_number = 1) %>% 
   mutate(p_value = as.numeric(p_value),
          p_value = ifelse(p_value > .1, 
                           p_value,
                           ifelse(p_value > .05, 
                                  paste0(p_value, '*'),
                                  paste0(p_value, '**')
                           )
          )
   ) %>% 
   rename_all(~paste0(.x, "_B"))

# Creating a big table
data_balance <- cbind(
   data_balance_8th_grade, data_balance_9th_grade, data_balance_lotteryB
)

# Row names
rownames(data_balance) <- c(
   "Language Score (7th Grade)",
   "Math Score (7th Grade)",
   "Std. Language Score (7th Grade)",
   "Std. Math Score (7th Grade)",
   "Sex: Male",
   "Sex: Female",
   "Race: White",
   "Race: Brown",
   "Race: Black",
   "Race: Yellow",
   "Race: Indigenous",
   "Mother's Education: < 5th Grade",
   "Mother's Education: 5th-9th Grade",
   "Mother's Education: 9th Grade-HS",
   "Mother's Education: HS-College",
   "Mother's Education: College",
   "Mother's Education: > College",
   "Mother's Education: Unknown",
   "Father's Education: < 5th Grade",
   "Father's Education: 5th-9th Grade",
   "Father's Education: 9th Grade-HS",
   "Father's Education: HS-College",
   "Father's Education: College",
   "Father's Education: > College",
   "Father's Education: Unknown",
   "Never Failed",
   "Failed Once",
   "Failed More than Once"
)

# Stargazer
stargazer(data_balance, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Balance Test Results",
          out = paste(str_table_directory, "balance_test.tex", sep = "/"))

### Multiple Outcomes ----------------------------------------------------------
# Implementing Young (2019) Randomized Inference (RI) procedure to account 
# for multiple hypothesis testing

# Selecting relevant columns (that have the differences)
## Using the notation from the slides, each column will be one B(Te)
BTe <- data_balance %>% 
   dplyr::select(starts_with("difference")) %>% 
   mutate(across(everything(), as.numeric))

# We will also create a copy of our dataset for convenience
## Will need to deselect linearly dependent columns for the inversion
## of the variance-covariance matrix
data_covariates_RI <- data_covariates %>% 
   dplyr::select(-score_mt_7_std, -score_lp_7_std, -sex_male,
                 -race_indigenous,
                 -mother_schooling_less_5th, 
                 -father_schooling_less_5th, -ever_failed_more_than_once)

## Excluding this rows from BTe
BTe <- BTe[-c(3, 4, 5, 11, 12, 19, 28), ]

# Other datasets
data_covariates_RI_lotteryB <- data_covariates_RI %>% 
   filter(lotteryB == 1) %>% 
   select(-student_code, -won_lotteryA, -lotteryB, -won_lotteryB)

data_covariates_RI_lost_lotteryA <- data_covariates_RI %>% 
   filter(won_lotteryA == 0) %>% 
   select(-student_code, -won_lotteryA, -lotteryB, -won_lotteryB)

# Relevant numbers
num_won_lotteryA <- sum(data_covariates$won_lotteryA)
num_won_lotteryB <- sum(data_covariates$won_lotteryB)
num_lotteryB <- sum(data_covariates$lotteryB)

# Doing 250 permutations
n_permutations <- 250

## Matrices to store results
matrix_BTn_won_lotteryA <- matrix(0, nrow = n_permutations, 
                                ncol = nrow(BTe))

matrix_BTn_won_lotteryB <- matrix(0, nrow = n_permutations, 
                                ncol = nrow(BTe))

matrix_BTn_lost_lotteryA <- matrix(0, nrow = n_permutations, 
                                ncol = nrow(BTe))

## Looping
tic()
for (permutation in 1:n_permutations){
   # Restarting dummy 'treatment' columns
   data_covariates_RI$won_lotteryA_permutation <- 0
   data_covariates_RI_lotteryB$won_lotteryB_permutation <- 0
   data_covariates_RI_lost_lotteryA$lotteryB_permutation <- 0
   
   # Allocating 'treatments'
   shuffled_indices_all <- sample(1:nrow(data_covariates_RI))
   shuffled_indices_lotteryB <- sample(1:nrow(data_covariates_RI_lotteryB))
   shuffled_indices_lost_lotteryA <- sample(1:nrow(data_covariates_RI_lost_lotteryA))
   
   # Assign treatment based on the shuffled indices
   data_covariates_RI$won_lotteryA_permutation[
      shuffled_indices_all[1:num_won_lotteryA]
   ] <- 1

   data_covariates_RI_lotteryB$won_lotteryB_permutation[
      shuffled_indices_lotteryB[1:num_won_lotteryB]
   ] <- 1
   
   data_covariates_RI_lost_lotteryA$lotteryB_permutation[
      shuffled_indices_lost_lotteryA[1:num_lotteryB]
   ] <- 1
   
   # Calculating differences
   data_balance_8th_grade_RI <- t_test_means(
      data_covariates_RI %>% 
         select(-student_code, -won_lotteryA, -won_lotteryB, -lotteryB),
      group_var = "won_lotteryA_permutation"
   ) %>% 
      t %>% 
      as.data.frame %>% 
      row_to_names(row_number = 1) %>% 
      select(difference)
   
   data_balance_9th_grade_RI <- t_test_means(
      data_covariates_RI_lotteryB, group_var = "won_lotteryB_permutation"
   ) %>% 
      t %>% 
      as.data.frame %>% 
      row_to_names(row_number = 1) %>% 
      select(difference)
   
   data_balance_lotteryB_RI <- t_test_means(
      data_covariates_RI_lost_lotteryA, group_var = "lotteryB_permutation"
   ) %>% 
      t %>% 
      as.data.frame %>% 
      row_to_names(row_number = 1) %>% 
      select(difference)
   
   # Appending to matrices
   matrix_BTn_won_lotteryA[permutation, ] <-
      as.numeric(data_balance_8th_grade_RI$difference)
   
   matrix_BTn_won_lotteryB[permutation, ] <-
      as.numeric(data_balance_9th_grade_RI$difference)
   
   matrix_BTn_lost_lotteryA[permutation, ] <-
      as.numeric(data_balance_lotteryB_RI$difference)
}
toc()  # 83s with 250 permutations

# Calculating inverse covariance matrices
cov_matrix_inv_BTn_won_lotteryA <- solve(cov(matrix_BTn_won_lotteryA))
# cov_matrix_inv_BTn_won_lotteryB <- solve(cov(matrix_BTn_won_lotteryB))
cov_matrix_inv_BTn_lost_lotteryA <- solve(cov(matrix_BTn_lost_lotteryA))

# Second is computationally singular - really.
# Probably due to some characteristic, wont bother to check and will just ignore

# Quadratic forms of actual 'treatment'
## 8th grade lottery
quadratic_8th_grade <- (BTe$difference_8th %*% 
   cov_matrix_inv_BTn_won_lotteryA %*% BTe$difference_8th)[1, 1]

## 9th grade lottery
quadratic_9th_grade <- (BTe$difference_9th %*% 
   diag(nrow(BTe)) %*% BTe$difference_9th)[1, 1]

## Lottery B
quadratic_lotteryB <- (BTe$difference_B %*% 
  cov_matrix_inv_BTn_lost_lotteryA %*% BTe$difference_B)[1, 1]

# Calculating probability that placebo 'treatments' have higher quadratic forms
## Lists
quadratic_8th_grade_perm <- rep(NA, n_permutations)
quadratic_9th_grade_perm <- rep(NA, n_permutations)
quadratic_lotteryB_perm <- rep(NA, n_permutations)

## Quadratic forms of permutations
for (i in 1:n_permutations){
   quadratic_8th_grade_perm[i] <- (matrix_BTn_won_lotteryA[i, ] %*% 
     cov_matrix_inv_BTn_won_lotteryA %*% matrix_BTn_won_lotteryA[i, ])[1, 1]
   
   quadratic_9th_grade_perm[i] <- (matrix_BTn_won_lotteryB[i, ] %*% 
      diag(nrow(BTe)) %*% matrix_BTn_won_lotteryB[i, ])[1, 1]
   
   quadratic_lotteryB_perm[i] <- (matrix_BTn_lost_lotteryA[i, ] %*% 
      cov_matrix_inv_BTn_lost_lotteryA %*% matrix_BTn_lost_lotteryA[i, ])[1, 1]
}

# Probabilities
(1 / n_permutations) * sum((quadratic_8th_grade_perm > quadratic_8th_grade))
(1 / n_permutations) * sum((quadratic_9th_grade_perm > quadratic_9th_grade))
(1 / n_permutations) * sum((quadratic_lotteryB_perm > quadratic_lotteryB))


# Question 3: Alternative Inference ============================================
# Interest outcome: 8th grade scores
# Will use absolute variables to preserve interpretation

## Regressions -----------------------------------------------------------------
# Models
## Without controls
reg_8th_grade_mt <- lm(score_mt_8 ~ treated8, data = data)

## With controls
reg_8th_grade_mt_controls <- lm(
   score_mt_8 ~ treated8 + factor(sex) + factor(race) + 
      factor(mother_schooling) + factor(father_schooling) + 
      factor(ever_failed_school) + score_lp_7 + score_mt_7, 
   data = data)

# Summaries
summary(reg_8th_grade_mt)
summary(reg_8th_grade_mt_controls)

# Coefficients
beta <- reg_8th_grade_mt$coefficients[[2]]
beta_controls <- reg_8th_grade_mt_controls$coefficients[[2]]

# Robust SEs of the treatment estimator
reg_8th_grade_mt_rob_se <- 
   diag(vcovHC(reg_8th_grade_mt, type = "HC3") ** (1 / 2))[2]

reg_8th_grade_mt_controls_rob_se <- 
   diag(vcovHC(reg_8th_grade_mt_controls, type = "HC3") ** (1 / 2))[2]

# t-statistics
tstat <- beta / reg_8th_grade_mt_rob_se
tstat_controls <- beta_controls / reg_8th_grade_mt_controls_rob_se

# p-values
p_value_beta <- 2 * pt(tstat, df = reg_8th_grade_mt$df.residual, lower.tail = F)
p_value_beta_controls <- 2 * pt(tstat_controls, 
                                df = reg_8th_grade_mt_controls$df.residual, 
                                lower.tail = F)

## Permutation Test ------------------------------------------------------------
# There are 483 observations, which is a lot of permutations to do

# Proportion of treated: 31% (150)
num_treatment <- sum(data$won_lotteryA)
prob_treatment <- sum(data$won_lotteryA) / nrow(data)

num_treatment
prob_treatment

# Doing 1000 permutations
n_permutations <- 1000

## Lists
beta_permutations <- rep(NA, n_permutations)
tstat_permutations <- rep(NA, n_permutations)

beta_permutations_controls <- rep(NA, n_permutations)
tstat_permutations_controls <- rep(NA, n_permutations)

## Loop
tic()
for (permutation in 1:n_permutations){
   # Restarting dummy treatment column
   data$treated8_permutation <- 0
   
   # Allocating treatment
   ## Shuffle the student_code indices
   shuffled_indices_perm <- sample(1:nrow(data))
   
   ## Assign treatment based on the shuffled indices
   data$treated8_permutation[shuffled_indices_perm[1:num_treatment]] <- 1

   # Regressions
   reg_8th_grade_mt_perm <- lm(score_mt_8 ~ treated8_permutation, data = data)
   
   reg_8th_grade_mt_controls_perm <- lm(
      score_mt_8 ~ treated8_permutation + factor(sex) + factor(race) + 
         factor(mother_schooling) + factor(father_schooling) + 
         factor(ever_failed_school) + score_lp_7 + score_mt_7, 
      data = data)
   
   # Coefficients (in absolute value)
   beta_perm <- abs(reg_8th_grade_mt_perm$coefficients[[2]])
   beta_perm_controls <- abs(reg_8th_grade_mt_controls_perm$coefficients[[2]])
   
   # Robust SEs
   reg_8th_grade_mt_perm_rob_se <- 
      diag(vcovHC(reg_8th_grade_mt_perm, type = "HC3") ** (1 / 2))[2]
   
   reg_8th_grade_mt_controls_perm_rob_se <- 
      diag(vcovHC(reg_8th_grade_mt_controls_perm, type = "HC3") ** (1 / 2))[2]
   
   # Adding coefficients to list
   beta_permutations[permutation] <- beta_perm
   beta_permutations_controls[permutation] <- beta_perm_controls
   
   # Adding t-stats
   tstat_permutations[permutation] <- beta_perm / reg_8th_grade_mt_perm_rob_se
   tstat_permutations_controls[permutation] <- beta_perm_controls / 
      reg_8th_grade_mt_controls_perm_rob_se
}
toc()  # 34 seconds

# Excluding the dummy treatment column
data %<>% 
   select(-treated8_permutation)

# Calculating p values
## No controls (simple OLS)
p_value_beta_perm <- (1 / n_permutations) * sum((beta_permutations > beta))
p_value_tstat_perm <- (1 / n_permutations) * sum((tstat_permutations > tstat))

## OLS with controls
p_value_beta_controls_perm <- (1 / n_permutations) * 
   sum((beta_permutations_controls > beta_controls))

p_value_tstat_controls_perm <- (1 / n_permutations) * 
   sum((tstat_permutations_controls > tstat_controls))

### Graphs ---------------------------------------------------------------------
#### |B| -----------------------------------------------------------------------
# Without Controls
ggplot(data.frame(beta_permutations), aes(x = beta_permutations)) +
   geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
   geom_vline(aes(xintercept = beta), color = "black", linetype = "dashed") +
   annotate("text", x = beta, y = Inf, vjust = 1.5, hjust = -.1, color = 'black',
            label = expression("Original " * hat(beta))) +
   labs(
      title = expression("Distribution of |" * hat(beta) * "| in the Permutation Tests"),
      x = expression("Permutations " * hat(beta)),
      y = "Frequency"
   ) +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(str_figure_directory, "permutation_beta.pdf", sep = "/"), 
       height = 6, width = 9, dpi = 600)

# With Controls
ggplot(data.frame(beta_permutations_controls), 
       aes(x = beta_permutations_controls)) +
   geom_histogram(binwidth = 0.05, fill = "darkgreen", color = "black") +
   geom_vline(aes(xintercept = beta_controls), color = "black", linetype = "dashed") +
   annotate("text", x = beta, y = Inf, vjust = 1.5, hjust = -.1, color = 'black',
            label = expression("Original " * hat(beta))) +
   labs(
      title = expression(
         "Distribution of |" * hat(beta) * "| (with Controls) in the Permutation Tests"
         ),
      x = expression("Permutations " * hat(beta)),
      y = "Frequency"
   ) +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(str_figure_directory, "permutation_beta_controls.pdf", sep = "/"), 
       height = 6, width = 9, dpi = 600)

#### t-stat --------------------------------------------------------------------
# Without Controls
ggplot(data.frame(tstat_permutations), aes(x = tstat_permutations)) +
   geom_histogram(binwidth = 0.05, fill = "aquamarine3", color = "black") +
   geom_vline(aes(xintercept = tstat), color = "black", linetype = "dashed") +
   annotate("text", x = tstat, y = Inf, vjust = 1.5, hjust = -.1, color = 'black',
            label = "Original t-stat") +
   labs(
      title = "Distribution of the t-statistics in the Permutation Tests",
      x = "Permutations of the t-statistic",
      y = "Frequency"
   ) +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(str_figure_directory, "permutation_tstat.pdf", sep = "/"), 
       height = 6, width = 9, dpi = 600)

# With controls
ggplot(data.frame(tstat_permutations_controls), 
       aes(x = tstat_permutations_controls)) +
   geom_histogram(binwidth = 0.05, fill = "darkolivegreen1", color = "black") +
   geom_vline(aes(xintercept = tstat_controls), color = "black", linetype = "dashed") +
   annotate("text", x = tstat_controls, y = Inf, vjust = 1.5, hjust = -.05, 
            color = 'black', label = "Original t-stat") +
   labs(
      title = "Distribution of the t-statistics (with Controls) in the Permutation Tests",
      x = "Permutations of the t-statistic",
      y = "Frequency"
   ) +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(str_figure_directory, "permutation_tstat_controls.pdf", sep = "/"), 
       height = 6, width = 9, dpi = 600)


## Wild Bootstrap with Null ----------------------------------------------------
# We will have the same number of observations, but will "mix" the errors

## Will have to create a separate dataset to avoid NAs in order to use predict()
data_controls <- drop_na(data)

# Estimating the regressions imposing the null of no treatment effect
## Without controls
reg_8th_grade_mt_wild <- lm(score_mt_8 ~ 1, data = data)

## With controls
reg_8th_grade_mt_controls_wild <- lm(
   score_mt_8 ~ factor(sex) + factor(race) + 
      factor(mother_schooling) + factor(father_schooling) + 
      factor(ever_failed_school) + score_lp_7 + score_mt_7, 
   data = data_controls)

# Getting the residuals from the regressions
residuals_wild <- reg_8th_grade_mt_wild$residuals
residuals_controls_wild <- reg_8th_grade_mt_controls_wild$residuals

# Running the bootstrap
## Numbers
n_bootstrap <- 1000
N <- nrow(data)
N_controls <- nrow(data_controls)

## Lists
beta_bootstrap <- rep(NA, n_bootstrap)
beta_bootstrap_controls <- rep(NA, n_bootstrap)

## Initializing columns
data$residual_wild_boot <- 0
data_controls$residual_wild_boot_controls <- 0

tic()
for (sample in 1:n_bootstrap){
   # Restarting dummy residual columns
   data$residual_wild_boot <- 0
   data_controls$residual_wild_boot_controls <- 0
   
   # Assigning errors (in a super inefficient way)
   ## Without controls
   for (i in 1:N){
      # Random integer from Uniform (1, N) to get which error will be sampled
      i_residual <- round(runif(1, min = 1, max = N), 0)

      # Random number from U(0, 1) to get if residual will be multiplied by -1
      random_number <- runif(1)
      
      # If random_number < .5, multiply by -1; else, by 1
      if (random_number < .5){
         data$residual_wild_boot[i] <- -1 * residuals_wild[i_residual]
      }
      else {
         data$residual_wild_boot[i] <- 1 * residuals_wild[i_residual]
      }
   }
   
   ## With controls
   for (i in 1:N_controls){
      # Random integer from Uniform (1, N) to get which error will be sampled
      i_residual_controls <- round(runif(1, min = 1, max = N_controls), 0)
      
      # Random number from Uniform(0, 1) to get if multiplied by (-1)
      random_number <- runif(1)
      
      # If random_number < .5, multiply by -1; else, by 1
      if (random_number < .5){
         data_controls$residual_wild_boot_controls[i] <- 
            -1 * residuals_controls_wild[i_residual_controls]
      }
      else {
         data_controls$residual_wild_boot_controls[i] <- 
            1 * residuals_controls_wild[i_residual_controls]
      }
   }
      
   # DGPs
   ## Without controls
   data$score_mt_8_wild_boot <- 
      predict(reg_8th_grade_mt_wild, data) + 
      data$residual_wild_boot
   
   ## With controls
   data_controls$score_mt_8_wild_boot_controls <- 
      predict(reg_8th_grade_mt_controls_wild, data_controls) +
      data_controls$residual_wild_boot_controls
   
   # Estimating regressions
   ## Without controls
   reg_8th_grade_mt_wild_boot <- lm(score_mt_8_wild_boot ~ treated8, 
                                    data = data)
   
   ## With controls
   reg_8th_grade_mt_controls_wild_boot <- lm(
      score_mt_8_wild_boot_controls ~ treated8 + factor(sex) + factor(race) + 
         factor(mother_schooling) + factor(father_schooling) + 
         factor(ever_failed_school) + score_lp_7 + score_mt_7, 
      data = data_controls)
   
   # Adding coefficients to lists
   beta_bootstrap[sample] <- 
      reg_8th_grade_mt_wild_boot$coefficients[[2]]
   
   beta_bootstrap_controls[sample] <- 
      reg_8th_grade_mt_controls_wild_boot$coefficients[[2]]
}
toc()  # 66 seconds

# Calculating p_values
p_value_beta_boot <- (1 / n_bootstrap) * sum((abs(beta_bootstrap) > abs(beta)))
p_value_beta_controls_boot <- (1 / n_bootstrap) * 
   sum((abs(beta_bootstrap_controls) > abs(beta_controls)))

### Graphs ---------------------------------------------------------------------
# Without Controls
ggplot(data.frame(beta_bootstrap), aes(x = beta_bootstrap)) +
   geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
   
   geom_vline(aes(xintercept = beta), color = "black", linetype = "dashed") +
   annotate("text", x = beta, y = Inf, vjust = 1.5, hjust = 1.1, color = 'black',
            label = expression("Original " * hat(beta))) +
   
   geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
   annotate("text", x = 0, y = Inf, vjust = 1.5, hjust = -.1, color = 'red',
            label = "Null") +
   
   labs(
      title = expression("Distribution of " * hat(beta) * " in Bootstrap Samples"),
      x = expression("Bootstrap " * hat(beta) * " (wild, with null imposed)"),
      y = "Frequency"
   ) +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(str_figure_directory, "bootstrap_beta.pdf", sep = "/"), 
       height = 6, width = 9, dpi = 600)

# With Controls
ggplot(data.frame(beta_bootstrap_controls), 
       aes(x = beta_bootstrap_controls)) +
   geom_histogram(binwidth = 0.05, fill = "darkgreen", color = "black") +
   
   geom_vline(aes(xintercept = beta_controls), color = "black", linetype = "dashed") +
   annotate("text", x = beta, y = Inf, vjust = 1.5, hjust = 1.2, color = 'black',
            label = expression("Original " * hat(beta))) +
   
   geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
   annotate("text", x = 0, y = Inf, vjust = 1.5, hjust = -.1, color = 'red',
            label = "Null") +
   
   labs(
      title = expression(
         "Distribution of " * hat(beta) * " (with Controls) in Bootstrap Samples"
      ),
      x = expression("Bootstrap " * hat(beta) * " (wild, with null imposed)"),
      y = "Frequency"
   ) +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(str_figure_directory, "bootstrap_beta_controls.pdf", sep = "/"), 
       height = 6, width = 9, dpi = 600)

## Yi(1, 1) - Yi(0, 0) ---------------------------------------------------------
# We want now the causal effect of being enrolled in both grades vs
# not being enrolled in any grade on math test scores.
# We showed how to identify and estimate this in PSET 01 Q7c.

# Implementing the estimator on the whole sample
# Using variable names from the Problem Set; see latex for details

## Won the first lottery
mean_Z1 <- data %>% 
   filter(won_lotteryA == 1) %>% 
   summarise(mean_Z1 = mean(score_mt_9)) %>% 
   pull()

## Lost the first lottery and didn't participate in the second
mean_Z0L0 <- data %>% 
   filter((won_lotteryA == 0) & (lotteryB == 0)) %>% 
   summarise(mean_Z0LO = mean(score_mt_9)) %>% 
   pull()

## Lost the first lottery, participated in the second and lost it
mean_Z0L1Z0 <- data %>% 
   filter((won_lotteryA == 0) & (lotteryB == 1) & (won_lotteryB == 0)) %>% 
   summarise(mean_Z0L1Z0 = mean(score_mt_9)) %>% 
   pull()

## Probability of not participating in the second lottery given that lost the 1st
prob_L0_given_ZO <- data %>% 
   filter(won_lotteryA == 0) %>% 
   group_by(lotteryB) %>% 
   summarise(n = n()) %>% 
   ungroup() %>% 
   mutate(prob = n / sum(n)) %>% 
   filter(lotteryB == 0) %>% 
   select(prob) %>% 
   pull()

# Estimator
beta_7c <- mean_Z1 - 
   prob_L0_given_ZO * mean_Z0L0 - 
   (1 - prob_L0_given_ZO) * mean_Z0L1Z0

### Boostrap -------------------------------------------------------------------
# Not easy to calculate variance and standard errors for this thing...
# bootstrap it is! We will use a standard non-parametric bootstrap

# List of estimators
## Number of bootstrap samples will be the same as the wild one
beta_bootstrap_7c <- rep(NA, n_bootstrap)

## Data (just for convenience)
data_bootstrap_7c <- data %>% 
   select(student_code, score_mt_9, won_lotteryA, lotteryB, won_lotteryB)

# Running the bootstrap
tic()
for (sample in 1:n_bootstrap){
   # Dummy data
   data_bootstrap_7c_sample <- data_bootstrap_7c %>% 
      mutate(student_code = 0, score_mt_9 = 0, won_lotteryA = 0, 
             lotteryB = 0, won_lotteryB = 0)
   
   # Creating bootstrap sample
   for (i in 1:N){
      # Random integer from Uniform (1, N) to get which unit will be sampled
      i_sample <- round(runif(1, min = 1, max = N), 0)
      
      # Bootstrap data
      data_bootstrap_7c_sample[i, ] <- data_bootstrap_7c[i_sample, ]
   }
   
   # Creating the estimator based on the bootstrap sample
   ## Won the first lottery
   mean_Z1_boot <- data_bootstrap_7c_sample %>% 
      filter(won_lotteryA == 1) %>% 
      summarise(mean_Z1 = mean(score_mt_9)) %>% 
      pull()
   
   ## Lost the first lottery and didn't participate in the second
   mean_Z0L0_boot <- data_bootstrap_7c_sample %>% 
      filter((won_lotteryA == 0) & (lotteryB == 0)) %>% 
      summarise(mean_Z0LO = mean(score_mt_9)) %>% 
      pull()
   
   ## Lost the first lottery, participated in the second and lost it
   mean_Z0L1Z0_boot <- data_bootstrap_7c_sample %>% 
      filter((won_lotteryA == 0) & (lotteryB == 1) & (won_lotteryB == 0)) %>% 
      summarise(mean_Z0L1Z0 = mean(score_mt_9)) %>% 
      pull()
   
   ## Probability of not participating in the second lottery given that lost the 1st
   prob_L0_given_ZO_boot <- data_bootstrap_7c_sample %>% 
      filter(won_lotteryA == 0) %>% 
      group_by(lotteryB) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      mutate(prob = n / sum(n)) %>% 
      filter(lotteryB == 0) %>% 
      select(prob) %>% 
      pull()
   
   # Estimator
   beta_7c_boot <- mean_Z1_boot - 
      prob_L0_given_ZO_boot * mean_Z0L0_boot - 
      (1 - prob_L0_given_ZO_boot) * mean_Z0L1Z0_boot
   
   # Adding coefficient to lists
   beta_bootstrap_7c[sample] <- beta_7c_boot
}
toc()  # 180s

# Graph
ggplot(data.frame(beta_bootstrap_7c), aes(x = beta_bootstrap_7c)) +
   geom_histogram(binwidth = 0.05, fill = "darkviolet", color = "black") +
   
   geom_vline(aes(xintercept = beta_7c), color = "black", linetype = "dashed") +
   annotate("text", x = beta_7c, y = Inf, vjust = 1.5, hjust = -.2, color = 'black',
            label = expression("Dashed line: original " * hat(tilde(beta)))) +
   labs(
      title = expression("Distribution of " * hat(tilde(beta)) * " in Bootstrap Samples"),
      x = expression("Bootstrap " * hat(tilde(beta)) * " (non-parametric)"),
      y = "Frequency"
   ) +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(str_figure_directory, "bootstrap_beta_7C.pdf", sep = "/"), 
       height = 6, width = 9, dpi = 600)

# Calculating standard deviation and standard error
beta_7c_sd_boot <- sd(beta_bootstrap_7c)  # already divides by n-1
beta_7c_se_boot <- sd(beta_bootstrap_7c) / sqrt(n_bootstrap)

# Confidence interval of 95%
quantile(beta_bootstrap_7c, probs = c(0.025, 0.975))


## Table Summary ---------------------------------------------------------------
# Summarising everything into one dataframe
results_q3 <- data.frame(
   Specification = c("Without Controls", "With Controls"),
   Coefficient = c(beta, beta_controls),
   Robust_SE = c(reg_8th_grade_mt_rob_se, reg_8th_grade_mt_controls_rob_se),
   # T_Statistic = c(tstat, tstat_controls),
   P_Value = c(p_value_beta, p_value_beta_controls),
   P_Value_Perm_Coef = c(p_value_beta_perm, p_value_beta_controls_perm),
   P_Value_Perm_TStat = c(p_value_tstat_perm, p_value_tstat_controls_perm),
   P_Value_Wild_Boot = c(p_value_beta_boot, p_value_beta_controls_boot)
)

# Transposing
results_q3 %<>% 
   mutate(across(is.numeric, round, digits = 4)) %>% 
   t %>%
   as.data.frame %>% 
   row_to_names(row_number = 1)

# Result
results_q3

# Stargazer
stargazer(results_q3, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Results_q3.tex",
          out = paste(str_table_directory, "results_q3.tex", sep = "/"))

### Estimator of 7c ------------------------------------------------------------
# Table
results_q3_7c <- data.frame(
   Coefficient = c(beta_7c),
   SE = c(beta_7c_sd_boot),
   P_Value = c(p_value_beta_7c_boot)
)

# Results
results_q3_7c

# Latex
stargazer(results_q3_7c, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Results_q3_e.tex",
          out = paste(str_table_directory, "results_q3_e.tex", sep = "/"))

# Question 4: Multiple Outcomes ================================================
## Language Test Scores --------------------------------------------------------
# Recalling math results
summary(reg_8th_grade_mt)
summary(reg_8th_grade_mt_controls)

# Models for language results
## Without controls
reg_8th_grade_lp <- lm(score_lp_8 ~ treated8, data = data)

## With controls
reg_8th_grade_lp_controls <- lm(
   score_lp_8 ~ treated8 + factor(sex) + factor(race) + 
      factor(mother_schooling) + factor(father_schooling) + 
      factor(ever_failed_school) + score_lp_7 + score_mt_7, 
   data = data)

# Summaries
summary(reg_8th_grade_lp)
summary(reg_8th_grade_lp_controls)

# Table
## List
model_list <- list(
   "Math - Without Controls" = reg_8th_grade_mt,
   "Math - With Controls" = reg_8th_grade_mt_controls,
   "Language - Without Controls" = reg_8th_grade_lp,
   "Language - With Controls" = reg_8th_grade_lp_controls
)

## Table
tab_summ <- modelsummary(
   model_list, 
   vcov = 'robust',
   output = 'latex',  # 'kableExtra', 'latex'
   fmt = fmt_decimal(3), 
   stars = FALSE,
   statistic = c("std.error", "p.value"),  # Show both SE and p-values
   fmt_statistic = list(
      "std.error" = "[{std.error}]",   # SE in brackets
      "p.value" = "[{p.value}]"  # p-value in parentheses
   ),
   coef_map = c('treated8' = 'Enrollment in 8th Grade'),
   gof_map = c('nobs', 'r.squared'),
   notes = list('HAC robust standard errors (HC3) in brackets; 
                p-values in parentheses.')
)
## Viewing / Saving
#tab_summ
save_kable(tab_summ, paste(str_table_directory, "table_q4.tex", sep = "/"))


## Standardized Variables ------------------------------------------------------
# Without controls
reg_8th_grade_mt_std <- lm(score_mt_8_std ~ treated8, data = data)
reg_8th_grade_lp_std <- lm(score_lp_8_std ~ treated8, data = data)

## With controls
reg_8th_grade_mt_std_controls <- lm(
   score_mt_8_std ~ treated8 + factor(sex) + factor(race) + 
      factor(mother_schooling) + factor(father_schooling) + 
      factor(ever_failed_school) + score_lp_7_std + score_mt_7_std, 
   data = data)

reg_8th_grade_lp_std_controls <- lm(
   score_lp_8_std ~ treated8 + factor(sex) + factor(race) + 
      factor(mother_schooling) + factor(father_schooling) + 
      factor(ever_failed_school) + score_lp_7_std + score_mt_7_std, 
   data = data)


# Table
## List
model_list_std <- list(
   "Math - Without Controls" = reg_8th_grade_mt_std,
   "Math - With Controls" = reg_8th_grade_mt_std_controls,
   "Language - Without Controls" = reg_8th_grade_lp_std,
   "Language - With Controls" = reg_8th_grade_lp_std_controls
)

## Table
tab_summ_std <- modelsummary(
   model_list_std, 
   vcov = 'robust',
   output = 'latex',  # 'kableExtra', 'latex'
   fmt = fmt_decimal(3), 
   stars = FALSE,
   statistic = c("std.error", "p.value"),  # Show both SE and p-values
   fmt_statistic = list(
      "std.error" = "[{std.error}]",   # SE in brackets
      "p.value" = "[{p.value}]"  # p-value in parentheses
   ),
   coef_map = c('treated8' = 'Enrollment in 8th Grade'),
   gof_map = c('nobs', 'r.squared'),
   notes = list('HAC robust standard errors (HC3) in brackets; 
                p-values in parentheses.')
)
## Viewing / Saving
#tab_summ_std
save_kable(tab_summ_std, paste(str_table_directory, "table_q4_std.tex", sep = "/"))


## Correction for Multiple Hypothesis ------------------------------------------
# Already did Young (2019), so will use Anderson (2008) to test significant of
# all outcomes. To test them individually and account for FWER, will use
# the simpler procedure of Bonferroni (when compared to Holm).

### Anderson (2008) ------------------------------------------------------------
# Both outcomes are already so that the positive direction is good
# We already constructed the standardized variables by the control statistics

# For the summary measure, we will just simple average
data$score_avg_8_std <- (data$score_mt_8_std + data$score_lp_8_std) / 2 

# Regressions
## Without controls
reg_8th_grade_std_avg <- lm(score_avg_8_std ~ treated8, data = data)

## With controls
reg_8th_grade_std_avg_controls <- lm(
   score_avg_8_std ~ treated8 + factor(sex) + factor(race) + 
      factor(mother_schooling) + factor(father_schooling) + 
      factor(ever_failed_school) + score_lp_7 + score_mt_7, 
   data = data)

# Table
## List
model_list <- list(
   "Math - Without Controls" = reg_8th_grade_mt,
   "Math - With Controls" = reg_8th_grade_mt_controls,
   "Language - Without Controls" = reg_8th_grade_lp,
   "Language - With Controls" = reg_8th_grade_lp_controls
)

# Table
tab_summ_avg <- modelsummary(
   list(
      "Without Controls" = reg_8th_grade_std_avg,
      "With Controls" = reg_8th_grade_std_avg_controls
   ), 
   vcov = 'robust',
   output = 'latex',  # 'kableExtra', 'latex'
   fmt = fmt_decimal(3), 
   stars = FALSE,
   statistic = c("std.error", "p.value"),  # Show both SE and p-values
   fmt_statistic = list(
      "std.error" = "[{std.error}]",   # SE in brackets
      "p.value" = "[{p.value}]"  # p-value in parentheses
   ),
   coef_map = c('treated8' = 'Enrollment in 8th Grade'),
   gof_map = c('nobs', 'r.squared')
)

# Viewing / Saving
#tab_summ
save_kable(tab_summ_avg, paste(str_table_directory, 
                               "table_q4_anderson.tex", sep = "/"))

### Bonferroni -----------------------------------------------------------------
# Just multiply the p-values from tab_summ by two :)

# Question 5: Attrition ========================================================
#' Suppose now at the end of grade 8 students can take an official exam that
#' would count for university admission once they finish high school. 
#' This exam is voluntary, so we should expect students that are more interested 
#' in going to the university would be more likely to take it. Information from 
#' this exam (which includes students who did not participate in the lottery) 
#' is avaiable in the dataset “voluntary exam.dta”.

## Data Wrangling --------------------------------------------------------------
# Cleaning environment
rm(list = ls())
cat("\014")

# Directories
# Directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
str_base_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
str_figure_directory <- paste(str_base_directory, "figures", sep = "/")
str_table_directory <- paste(str_base_directory, "tables", sep = "/")

# Reading datasets
data_attrition <- haven::read_dta("voluntary_exam.dta") %>% 
   arrange(student_code)
data_lotteries <- haven::read_dta("base_lotteries.dta") %>% 
   arrange(student_code)

# Seeing students that participated in the 8th grade lottery
data_lotteries <- left_join(data_lotteries, data_attrition, by = 'student_code')

# Creating dummy column
data_lotteries %<>% 
   mutate(took_voluntary_exam = as.numeric(!is.na(voluntary_exam_score))) %>% 
   relocate(took_voluntary_exam, .before = sex)

# How many took the test: 320 out of 483 (66.25%)
data_lotteries %>% 
   summarise(num_took_test = sum(took_voluntary_exam))

# By group
data_lotteries %>% 
   group_by(won_lotteryA) %>% 
   summarise(num_took_test = sum(took_voluntary_exam))

## Effect on Probability of Taking the Exam ------------------------------------
# For those on the original dataset, we can calculate the effect of 
# enrollment on the probability of taking the voluntary exam
# We will just use a very simple linear probability model: since the 8th grade
# lottery is fair and we have perfect compliance, 
# this recover the causal effect of interest

## Models
reg_prob_exam <- lm(took_voluntary_exam ~ won_lotteryA, data = data_lotteries)

reg_prob_exam_controls <- lm(
   took_voluntary_exam ~ won_lotteryA + factor(sex) + factor(race) + 
      factor(mother_schooling) + factor(father_schooling) + 
      factor(ever_failed_school) + score_lp_7 + score_mt_7, 
   data = data_lotteries)

## Summary
summary(reg_prob_exam)
summary(reg_prob_exam_controls)

# Coefficient
beta_prob <- reg_prob_exam$coefficients[[2]]
beta_prob_controls <- reg_prob_exam_controls$coefficients[[2]]

# Robust SEs of the treatment estimator
reg_prob_exam_rob_se <- 
   diag(vcovHC(reg_prob_exam, type = "HC3") ** (1 / 2))[2]
reg_prob_exam_controls_rob_se <- 
   diag(vcovHC(reg_prob_exam_controls, type = "HC3") ** (1 / 2))[2]

# t-statistics
tstat_prob_exam <- beta_prob / reg_prob_exam_rob_se
tstat_prob_exam_controls <- beta_prob_controls / reg_prob_exam_controls_rob_se

# p-values
p_value_prob_exam <- 2 * pt(tstat_prob_exam, 
                            df = reg_prob_exam$df.residual, lower.tail = F)

p_value_prob_exam_controls <- 2 * pt(
   tstat_prob_exam_controls,
   df = reg_prob_exam_controls$df.residual, lower.tail = F
)


## Balance Test: Lottery for Non-Attriters -------------------------------------
### Indicators -----------------------------------------------------------------
# Dataframe with only the covariates and treated columns conditional on
# non-attriters, that is, those that took the exam
data_covariates <- data_lotteries %>% 
   dplyr::select(student_code, sex, race, mother_schooling, 
                 father_schooling, ever_failed_school,
                 score_lp_7, score_mt_7,
                 won_lotteryA, won_lotteryB, lotteryB, took_voluntary_exam)

# Exploding the 'sex' column into 'sex_female' and 'sex_male'
data_covariates %<>% 
   mutate(
      sex_male = ifelse(sex == 1, 1, 0),
      sex_female = ifelse(sex == 2, 1, 0)
   )

# Exploding the 'race' column into indicator columns
data_covariates %<>% 
   mutate(
      race_white = ifelse(race == 1, 1, 0),
      race_brown = ifelse(race == 2, 1, 0),
      race_black = ifelse(race == 3, 1, 0),
      race_yellow = ifelse(race == 4, 1, 0),
      race_indigenous = ifelse(race == 5, 1, 0)
   )

# Exploding the 'mother_schooling' column into indicator columns
data_covariates %<>% 
   mutate(
      mother_schooling_less_5th = ifelse(mother_schooling == 1, 1, 0),
      mother_schooling_5th_9th = ifelse(mother_schooling == 2, 1, 0),
      mother_schooling_9th_HS = ifelse(mother_schooling == 3, 1, 0),
      mother_schooling_HS_College = ifelse(mother_schooling == 4, 1, 0),
      mother_schooling_College = ifelse(mother_schooling == 5, 1, 0),
      mother_schooling_above_College = ifelse(mother_schooling == 6, 1, 0),
      mother_schooling_unknown = ifelse(mother_schooling == 7, 1, 0)
   )

# Exploding the 'father_schooling' column into indicator columns
data_covariates %<>% 
   mutate(
      father_schooling_less_5th = ifelse(father_schooling == 1, 1, 0),
      father_schooling_5th_9th = ifelse(father_schooling == 2, 1, 0),
      father_schooling_9th_HS = ifelse(father_schooling == 3, 1, 0),
      father_schooling_HS_College = ifelse(father_schooling == 4, 1, 0),
      father_schooling_College = ifelse(father_schooling == 5, 1, 0),
      father_schooling_above_College = ifelse(father_schooling == 6, 1, 0),
      father_schooling_unknown = ifelse(father_schooling == 7, 1, 0)
   )

# Exploding the 'ever_failed' column into indicator columns
data_covariates %<>% 
   mutate(
      ever_failed_never = ifelse(ever_failed_school == 1, 1, 0),
      ever_failed_once = ifelse(ever_failed_school == 2, 1, 0),
      ever_failed_more_than_once = ifelse(ever_failed_school == 3, 1, 0)
   )

# Select only indicator columns and baseline scores
data_covariates %<>% 
   dplyr::select(-sex, -race, -mother_schooling, 
                 -father_schooling, -ever_failed_school)

### Tests ----------------------------------------------------------------------
# Function to calculate means and p-values for each variable
t_test_means <- function(df, group_var) {
   df %>%
      summarise(across(
         -all_of(group_var),
         list(
            mean_treated = ~ mean(.x[!!sym(group_var) == 1], na.rm = TRUE),
            se_treated = ~ sd(.x[!!sym(group_var) == 1], na.rm = TRUE) / 
               sqrt(sum(!is.na(.x[!!sym(group_var) == 1]))),
            
            mean_control = ~ mean(.x[!!sym(group_var) == 0], na.rm = TRUE),
            se_control = ~ sd(.x[!!sym(group_var) == 0], na.rm = TRUE) / 
               sqrt(sum(!is.na(.x[!!sym(group_var) == 0]))),
            
            difference = ~ mean(.x[!!sym(group_var) == 1], na.rm = TRUE) - 
               mean(.x[!!sym(group_var) == 0], na.rm = TRUE),
            
            se_difference = ~ t.test(.x ~ df[[group_var]], 
                                     alternative = 'two.sided',
                                     var.equal = FALSE,
                                     conf.level = .9)$stderr,
            p_value = ~ t.test(.x ~ df[[group_var]], 
                               alternative = 'two.sided',
                               var.equal = FALSE,
                               conf.level = .9)$p.value
         ),
         .names = "{col}__{fn}"
      )) %>%
      pivot_longer(cols = everything(),
                   names_to = c(".value", "variable"),
                   names_pattern = "(.*)__(.*)")
}

# Apply the function to get means and p-values
## 8th grade
data_balance_8th_grade <- t_test_means(
   data_covariates %>% 
      filter(took_voluntary_exam == 1) %>% 
      select(-student_code, -won_lotteryB, -lotteryB, -took_voluntary_exam),
   group_var = "won_lotteryA"
)

### Latex ----------------------------------------------------------------------
# Transposing, rounding and adding stars so it is pretty
data_balance_8th_grade %<>% 
   mutate(across(is.numeric, round, digits = 2)) %>% 
   t %>%
   as.data.frame %>% 
   row_to_names(row_number = 1) %>% 
   mutate(p_value = as.double(p_value),
          p_value = ifelse(p_value > .1, 
                           p_value,
                           ifelse(p_value > .05, 
                                  paste0(p_value, '*'),
                                  paste0(p_value, '**')
                           )
          )
   )


# Row names
rownames(data_balance_8th_grade) <- c(
   "Language Score (7th Grade)",
   "Math Score (7th Grade)",
   "Sex: Male",
   "Sex: Female",
   "Race: White",
   "Race: Brown",
   "Race: Black",
   "Race: Yellow",
   "Race: Indigenous",
   "Mother's Education: < 5th Grade",
   "Mother's Education: 5th-9th Grade",
   "Mother's Education: 9th Grade-HS",
   "Mother's Education: HS-College",
   "Mother's Education: College",
   "Mother's Education: > College",
   "Mother's Education: Unknown",
   "Father's Education: < 5th Grade",
   "Father's Education: 5th-9th Grade",
   "Father's Education: 9th Grade-HS",
   "Father's Education: HS-College",
   "Father's Education: College",
   "Father's Education: > College",
   "Father's Education: Unknown",
   "Never Failed",
   "Failed Once",
   "Failed More than Once"
)

# Stargazer
stargazer(data_balance_8th_grade, type = "latex", 
          summary = FALSE, rownames = TRUE, 
          title = "Balance Test Results",
          out = paste(str_table_directory, "balance_test_attrition.tex", sep = "/"))

### Multiple Outcomes ----------------------------------------------------------
# Implementing Young (2019) Randomized Inference (RI) procedure to account 
# for multiple hypothesis testing

# Selecting relevant columns (that have the differences)
## Using the notation from the slides, each column will be one B(Te)
BTe <- data_balance_8th_grade %>% 
   dplyr::select(starts_with("difference")) %>% 
   mutate(across(everything(), as.numeric))

# We will also create a copy of our dataset for convenience
## Will need to deselect linearly dependent columns for the inversion
## of the variance-covariance matrix
data_covariates_RI <- data_covariates %>% 
   filter(took_voluntary_exam == 1) %>% 
   dplyr::select(-sex_male, -race_indigenous,
                 -mother_schooling_less_5th, 
                 -father_schooling_less_5th, -ever_failed_more_than_once)

## Excluding this rows from BTe
BTe <- BTe[-c(3, 9, 10, 17, 26), ]

# Relevant numbers
num_won_lotteryA <- sum(data_covariates_RI$won_lotteryA)

# Doing 250 permutations
n_permutations <- 250

## Matrices to store results
matrix_BTn_won_lotteryA <- matrix(0, nrow = n_permutations, 
                                  ncol = length(BTe))

## Looping
tic()
for (permutation in 1:n_permutations){
   # Restarting dummy 'treatment' columns
   data_covariates_RI$won_lotteryA_permutation <- 0
   
   # Allocating 'treatments'
   shuffled_indices_all <- sample(1:nrow(data_covariates_RI))
   
   # Assign treatment based on the shuffled indices
   data_covariates_RI$won_lotteryA_permutation[
      shuffled_indices_all[1:num_won_lotteryA]
   ] <- 1
   
   # Calculating differences
   data_balance_8th_grade_RI <- t_test_means(
      data_covariates_RI %>% 
         select(-student_code, -won_lotteryA, -took_voluntary_exam,
                -won_lotteryB, -lotteryB),
      group_var = "won_lotteryA_permutation"
   ) %>% 
      t %>% 
      as.data.frame %>% 
      row_to_names(row_number = 1) %>% 
      select(difference)
   
   # Appending to matrices
   matrix_BTn_won_lotteryA[permutation, ] <-
      as.numeric(data_balance_8th_grade_RI$difference)
}
toc()  # 50s with 250 permutations

# Calculating inverse covariance matrices
cov_matrix_inv_BTn_won_lotteryA <- solve(cov(matrix_BTn_won_lotteryA))

# Quadratic forms of actual 'treatment'
## 8th grade lottery
quadratic_8th_grade <- (BTe %*% cov_matrix_inv_BTn_won_lotteryA %*% BTe)[1, 1]

# Calculating probability that placebo 'treatments' have higher quadratic forms
## Lists
quadratic_8th_grade_perm <- rep(NA, n_permutations)

## Quadratic forms of permutations
for (i in 1:n_permutations){
   quadratic_8th_grade_perm[i] <- (matrix_BTn_won_lotteryA[i, ] %*% 
      cov_matrix_inv_BTn_won_lotteryA %*% matrix_BTn_won_lotteryA[i, ])[1, 1]
   }

# Probabilities
(1 / n_permutations) * sum((quadratic_8th_grade_perm > quadratic_8th_grade))


## Balance Test: Attrition as Treatment ----------------------------------------
# Apply the function to get means and p-values
data_balance_attrition <- t_test_means(
   data_covariates %>% 
      select(-student_code, -won_lotteryB, -lotteryB, -won_lotteryA),
   group_var = "took_voluntary_exam"
)

# Transposing, rounding and adding stars so it is pretty
data_balance_attrition %<>% 
   mutate(across(is.numeric, round, digits = 2)) %>% 
   t %>%
   as.data.frame %>% 
   row_to_names(row_number = 1) %>% 
   mutate(p_value = as.double(p_value),
          p_value = ifelse(p_value > .1, 
                           p_value,
                           ifelse(p_value > .05, 
                                  paste0(p_value, '*'),
                                  paste0(p_value, '**')
                           )
          )
   )


# Row names
rownames(data_balance_attrition) <- c(
   "Language Score (7th Grade)",
   "Math Score (7th Grade)",
   "Sex: Male",
   "Sex: Female",
   "Race: White",
   "Race: Brown",
   "Race: Black",
   "Race: Yellow",
   "Race: Indigenous",
   "Mother's Education: < 5th Grade",
   "Mother's Education: 5th-9th Grade",
   "Mother's Education: 9th Grade-HS",
   "Mother's Education: HS-College",
   "Mother's Education: College",
   "Mother's Education: > College",
   "Mother's Education: Unknown",
   "Father's Education: < 5th Grade",
   "Father's Education: 5th-9th Grade",
   "Father's Education: 9th Grade-HS",
   "Father's Education: HS-College",
   "Father's Education: College",
   "Father's Education: > College",
   "Father's Education: Unknown",
   "Never Failed",
   "Failed Once",
   "Failed More than Once"
)

# Stargazer
stargazer(data_balance_attrition, type = "latex", 
          summary = FALSE, rownames = TRUE, 
          title = "Balance Test Results",
          out = paste(str_table_directory, 
                      "balance_test_attrition2.tex", sep = "/"))


## Exam Score ------------------------------------------------------------------
### OLS ------------------------------------------------------------------------
# We have attrition: not all students take the exam and enrolled students are 
# more likely to take. Exam takers have "better" baseline characteristics
# OLS is then likely to be bias, the direction of which depends on whether
# going to the school increases the difference.

# Naive OLS
## Models
reg_score_exam <- lm(voluntary_exam_score ~ won_lotteryA, data = data_lotteries)

reg_score_exam_controls <- lm(
   voluntary_exam_score ~ won_lotteryA + factor(sex) + factor(race) + 
      factor(mother_schooling) + factor(father_schooling) + 
      factor(ever_failed_school) + score_lp_7 + score_mt_7, 
   data = data_lotteries)

## Summary
summary(reg_score_exam)
summary(reg_score_exam_controls)

# Coefficient
beta_score <- reg_score_exam$coefficients[[2]]
beta_score_controls <- reg_score_exam_controls$coefficients[[2]]

# Robust SEs of the treatment estimator
reg_score_exam_rob_se <- 
   diag(vcovHC(reg_score_exam, type = "HC3") ** (1 / 2))[2]
reg_score_exam_controls_rob_se <- 
   diag(vcovHC(reg_score_exam_controls, type = "HC3") ** (1 / 2))[2]

# t-statistics
tstat_score_exam <- beta_score / reg_score_exam_rob_se
tstat_score_exam_controls <- beta_score_controls / reg_score_exam_controls_rob_se

# p-values
p_value_score_exam <- 2 * pt(tstat_score_exam, 
                            df = reg_score_exam$df.residual, lower.tail = F)

p_value_score_exam_controls <- 2 * pt(
   tstat_score_exam_controls,
   df = reg_score_exam_controls$df.residual, lower.tail = F
)

### Lee Bounds  ----------------------------------------------------------------
# Creating dataframe
data_lotteries_lee <- data.frame(
   treat = data_lotteries$won_lotteryA,
   selection = data_lotteries$took_voluntary_exam,
   outcome = data_lotteries$voluntary_exam_score
)

# Getting bounds
GetBounds(leebounds(data_lotteries_lee))
