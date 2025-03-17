#' Microeconometrics II - Referee Report & Replication & Extension
#' Student: Vinícius de Almeida Nery Ferreira
#' Professor: Bruno Ferman
#' TA: Arthur Botinha
#' FGV - EESP 2024
#' 
#' GOAL: replicate and extend the difference-in-differences (DiD) results of
#' Chen and Hoekstra (Journal of Human Resources 2013)
#' 
#' Inspiration: Causal Inference: The Mixtape (Cunningham, 2017, Chap. 9 (DiD))

# Setup ========================================================================
## Libraries -------------------------------------------------------------------
# Files
library(haven)
library(xtable)

# Data
library(tidyverse)
library(magrittr)
library(glue)
library(tictoc)

# Tables
library(modelsummary)
library(kableExtra)

# Standard errors and tests
library(car)
library(sandwich)
library(lmtest)

# TWFE and DiD
## TWFE
library(fixest)

## Binary DiD
library(did)
library(bacondecomp)
library(did2s)

## Sensitivity
library(HonestDiD) # devtools::install_github("asheshrambachan/HonestDiD")
library(pretrends) # devtools::install_github("jonathandroth/pretrends")

## Environment -----------------------------------------------------------------
# Cleaning
rm(list = ls())
cat("\014")

# Directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
str_base_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

str_data_directory <- paste(str_base_directory, "data", sep = "/")
str_table_directory <- paste(str_base_directory, "tables", sep = "/")
str_figure_directory <- paste(str_base_directory, "figures", sep = "/")

my_theme <- function() {
   theme_bw() +
   theme(
      plot.title = element_blank(),
      legend.position = 'none',
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
   )
}

# Data =========================================================================
# Reading file
# Source: https://github.com/scunning1975/mixtape/blob/master/castle.dta
df <- read_dta(paste0(str_data_directory, "/castle.dta"))

# Seeing number of unique states: all 50 states, including those with laws
# passed before the initial sample period (2000)
length(unique(df$state))

# Collapsing regions-years dummies into one column
## Columns
df_fixed_effects <- df %>% 
   dplyr::select(starts_with("r20"))

## Grouping
df$region_year <- as.factor(as.numeric(interaction(
   df_fixed_effects,
   drop = TRUE
)))

# Replication of Main Results ==================================================
# Only their preferred specification: region-time FE and time-varying controls

# Variables
c_alt_dep_variables <- c(
   "jhcitizen_c", "jhpolice_c", 
   "murder", "homicide", 
   "robbery", "assault", "burglary",
   "larceny", "motor", "robbery_gun_r"
)

c_controls_repl <- c(
   "blackm_15_24", "whitem_15_24", "blackm_25_44", "whitem_25_44",
   "l_exp_subsidy", "l_exp_pubwelfare",
   "l_police", "unemployrt", "poverty", 
   "l_income", "l_prisoner", "l_lagprisoner"
)

treat_var_repl <- "cdl"

# Formula
dd_formula_repl <- function(dep_variable, treat_var_repl = "cdl"){
   formula_obj <- as.formula(
      paste(
         dep_variable, "~",
         paste(
            treat_var_repl,
            paste(c_controls_repl, collapse = " + "),
            sep = " + "
         ),
         "| year + sid + region_year"
      )
   )
   
   return(formula_obj)
}


## Homicides -------------------------------------------------------------------
# OLS Unweighted
repl_homicide <- fixest::feols(
   dd_formula_repl("l_homicide"),
   data = df,
   cluster = c("sid")
)

# OLS Weighted
repl_homicide_wt <- fixest::feols(
   dd_formula_repl("l_homicide"),
   data = df,
   cluster = c("sid"),
   weights = df$popwt
)

## Larceny ---------------------------------------------------------------------
# OLS Unweighted
repl_larceny <- fixest::feols(
   dd_formula_repl("l_larceny"),
   data = df,
   cluster = c("sid")
)

# OLS Weighted
repl_larceny_wt <- fixest::feols(
   dd_formula_repl("l_larceny"),
   data = df,
   cluster = c("sid"),
   weights = df$popwt
)

## Motor Vehicle Theft ---------------------------------------------------------
# OLS Unweighted
repl_motor <- fixest::feols(
   dd_formula_repl("l_motor"),
   data = df,
   cluster = c("sid")
)

# OLS Weighted
repl_motor_wt <- fixest::feols(
   dd_formula_repl("l_motor"),
   data = df,
   cluster = c("sid"),
   weights = df$popwt
)

## Burglary --------------------------------------------------------------------
# OLS Unweighted
repl_burglary <- fixest::feols(
   dd_formula_repl("l_burglary"),
   data = df,
   cluster = c("sid")
)

# OLS Weighted
repl_burglary_wt <- fixest::feols(
   dd_formula_repl("l_burglary"),
   data = df,
   cluster = c("sid"),
   weights = df$popwt
)

## Robbery ---------------------------------------------------------------------
# OLS Unweighted
repl_robbery <- fixest::feols(
   dd_formula_repl("l_robbery"),
   data = df,
   cluster = c("sid")
)

# OLS Weighted
repl_robbery_wt <- fixest::feols(
   dd_formula_repl("l_robbery"),
   data = df,
   cluster = c("sid"),
   weights = df$popwt
)

## Assault ---------------------------------------------------------------------
# OLS Unweighted
repl_assault <- fixest::feols(
   dd_formula_repl("l_assault"),
   data = df,
   cluster = c("sid")
)

# OLS Weighted
repl_assault_wt <- fixest::feols(
   dd_formula_repl("l_assault"),
   data = df,
   cluster = c("sid"),
   weights = df$popwt
)


## Table -----------------------------------------------------------------------
fixest::etable(
   repl_homicide, repl_larceny, repl_motor, 
   repl_burglary, repl_robbery, repl_assault,
   repl_homicide_wt, repl_larceny_wt, repl_motor_wt, 
   repl_burglary_wt, repl_robbery_wt, repl_assault_wt,
   title = "Main Results of Cheng and Hoekstra (JHR 2013)",
   tex = TRUE, 
   digits = 4,
   digits.stats = 4,
   keep = c('cdl'),
   file = paste0(str_table_directory, "/1_replication_table.tex")
)

# Extensions: Binary Treatment =================================================
## Binary Replication ----------------------------------------------------------
#' Cheng and Hoekstra use as the treatment variable the _fraction_ of the year
#' the state had Castle laws. This makes for a continuous treatment.
#' They show that results are similar excluding the first year of implementa-
#' tion. We follow this approach, stating as the treatment the first *full*
#' year of laws. We also compare results with treatment being 1 if the state
#' had most of the year treated (`cdl` > .5)

# Selecting variables we actually using and dropping multicol. fixed effects
df_main <- df %>% 
   dplyr::select(
      state, sid, year, region_year, cdl, post, pre2_cdl, popwt,
      l_homicide, l_larceny, l_motor, l_burglary, l_robbery, l_assault,
      any_of(c_controls_repl)
   )

# Creating alternative treatments
df_main %<>% 
   mutate(cdl_1st = ifelse((cdl > 0) & (cdl < 1), 0, cdl),
          cdl_most = ifelse(cdl >= .5, 1, 0)) %>% 
   relocate(any_of(c("cdl_1st", "cdl_most")), .after = cdl)

# Homicides (will just do this for brevity) (only weighted)
repl_homicide_bin_1st_wt <- fixest::feols(
   dd_formula_repl("l_homicide", treat_var_repl = "cdl_1st"),
   data = df_main,
   cluster = c("sid"),
   weights = df_main$popwt
)

repl_homicide_bin_most_wt <- fixest::feols(
   dd_formula_repl("l_homicide", treat_var_repl = "cdl_most"),
   data = df_main,
   cluster = c("sid"),
   weights = df_main$popwt
)

fixest::etable(
   repl_homicide_wt, repl_homicide_bin_1st_wt, repl_homicide_bin_most_wt, 
   title = "Continuous versus Binary Treatment Alternatives",
   tex = TRUE, 
   digits = 4,
   digits.stats = 4,
   keep = c('cdl', 'cdl_1st', 'cdl_most'),
   agg = '(cdl)',
   file = paste0(str_table_directory, "/2_binary_comparison.tex")
)

# Results very similar, although smaller for binary treatment
# 9.37 --> 8.21 or 8.08; all still significant.
# Will follow them (footnote 21) and Cunningham (2017) and use cdl_1st.

## Other Specifications --------------------------------------------------------
#' An issue is that the time-varying controls they use could be bad controls
#' (specially police, prisoner and lagprisoner).
#' No issue at first in including time-varying covariates, but they can't be
#' affected by treatment. In the police and prisioner case,
#' `Treatment -> More Homicide -> More Police / More Prisoners`

#' As discussed in Roth et al. (2024), controlling for lagged homicides
#' can be a good thing in this case, as maybe these laws where passed in
#' response to a violence trend captured by more homicides: 
#' "Including the lagged outcome in the conditioning variable thus makes sense 
#' if one is confident in the conditional unconfoundedness assumption: i.e., 
#' if treatment is as good as randomly assigned conditional on the lagged 
#' outcome and other elements of Xi. This may be sensible in settings where 
#' treatment takeup decisions are made on the basis of lagged outcomes"

#' Thus, the best specification, in my view, controls for unemployment,
#' poverty, demographics and public welfare in a time-varying fashion.
#' Also, controls for police and prisoner at baseline AND lags of outcomes.
#' Also, region by year fixed effects, making comparison only between states
#' in that region.
#' However, fixed effects and baseline values are collinear, so we omit 
#' the baseline values... Don't know how to control for them properly.

### Wrangling ------------------------------------------------------------------

# Creating lagged outcomes and pre-treatment variables
df_main %<>% 
   group_by(state) %>% 
   arrange(year) %>% 
   mutate(
      l_homicide_lag1 = lag(l_homicide),
      l_larceny_lag1 = lag(l_larceny),
      l_motor_lag1 = lag(l_motor),
      l_burglary_lag1 = lag(l_burglary),
      l_robbery_lag1 = lag(l_robbery),
      l_assault_lag1 = lag(l_assault),
      
      last_l_police = l_police[max(which(cdl_1st == 0))],
      last_l_prisoner = l_prisoner[max(which(cdl_1st == 0))]
   ) %>% 
   ungroup()

# Creating variable of the first period a unit was treated
df_main %<>% 
   group_by(sid) %>% 
   arrange(year) %>% 
   mutate(first_treat = year[min(which(cdl_1st == 1))]) %>% 
   ungroup() %>% 
   relocate(first_treat, .after = cdl_1st) %>% 
   mutate(first_treat = ifelse(is.na(first_treat), 0, first_treat))

# Summary of number of states per year they were treated
df_treated_by_year <- df_main %>% 
   filter(year == 2010) %>% 
   group_by(first_treat) %>% 
   summarise(Number_States = n())

df_treated_by_year

### Formulas -------------------------------------------------------------------
# Creating some formulas
# 0. Only treatment

# 1. No controls, only the fixed effects

# 2. Sociodemographic, time-varying controls
spec_sociodemo <- paste(
   "cdl_1st",
   "blackm_15_24 + whitem_15_24 + blackm_25_44 + whitem_25_44",
   "unemployrt + poverty + l_income",
   "l_exp_subsidy + l_exp_pubwelfare",
   sep = "+"

)

# 3. Sociodemographic + lagged homicides
spec_sociodemo_lagged_outcome <- paste(
   spec_sociodemo, 
   "l_homicide_lag1",
   sep = "+"
)

# 4. Sociodemographic + baseline police and prisoner
spec_sociodemo_baseline <- paste(
   spec_sociodemo, 
   "last_l_police",
   "last_l_prisoner",
   sep = "+"
)

# 5. Everything
spec_full <- paste(
   spec_sociodemo, 
   "l_homicide_lag1",
   #"last_l_police",
   #"last_l_prisoner",
   sep = "+"
)

### TWFE -----------------------------------------------------------------------
#### Homicides -----------------------------------------------------------------
twfe_homicide0_no_weights <- fixest::feols(
   as.formula("l_homicide ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year"),
   cluster = c("sid")
)

twfe_homicide0 <- fixest::feols(
   as.formula("l_homicide ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year"),
   cluster = c("sid"),
   weights = as.matrix(df_main['popwt'])
)

twfe_homicide1 <- fixest::feols(
   as.formula("l_homicide ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_homicide2 <- fixest::feols(
   as.formula(paste("l_homicide", spec_sociodemo, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_homicide3 <- fixest::feols(
   as.formula(paste("l_homicide", spec_sociodemo_lagged_outcome, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

#### Larceny -----------------------------------------------------------------
twfe_larceny0 <- fixest::feols(
   as.formula("l_larceny ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_larceny1 <- fixest::feols(
   as.formula("l_larceny ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_larceny2 <- fixest::feols(
   as.formula(paste("l_larceny", spec_sociodemo, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_larceny3 <- fixest::feols(
   as.formula(paste("l_larceny", spec_sociodemo_lagged_outcome, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

#### Motor -----------------------------------------------------------------
twfe_motor0 <- fixest::feols(
   as.formula("l_motor ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_motor1 <- fixest::feols(
   as.formula("l_motor ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_motor2 <- fixest::feols(
   as.formula(paste("l_motor", spec_sociodemo, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_motor3 <- fixest::feols(
   as.formula(paste("l_motor", spec_sociodemo_lagged_outcome, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)


#### Burglary -----------------------------------------------------------------
twfe_burglary0 <- fixest::feols(
   as.formula("l_burglary ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_burglary1 <- fixest::feols(
   as.formula("l_burglary ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_burglary2 <- fixest::feols(
   as.formula(paste("l_burglary", spec_sociodemo, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_burglary3 <- fixest::feols(
   as.formula(paste("l_burglary", spec_sociodemo_lagged_outcome, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)


#### Robbery -----------------------------------------------------------------
twfe_robbery0 <- fixest::feols(
   as.formula("l_robbery ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_robbery1 <- fixest::feols(
   as.formula("l_robbery ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_robbery2 <- fixest::feols(
   as.formula(paste("l_robbery", spec_sociodemo, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_robbery3 <- fixest::feols(
   as.formula(paste("l_robbery", spec_sociodemo_lagged_outcome, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)


#### Assault -----------------------------------------------------------------
twfe_assault0 <- fixest::feols(
   as.formula("l_assault ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_assault1 <- fixest::feols(
   as.formula("l_assault ~ cdl_1st"),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_assault2 <- fixest::feols(
   as.formula(paste("l_assault", spec_sociodemo, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)

twfe_assault3 <- fixest::feols(
   as.formula(paste("l_assault", spec_sociodemo_lagged_outcome, sep = "~")),
   data = df_main,
   fixef = c("sid", "year", "region_year"),
   cluster = c("sid"),
   weights = df_main$popwt
)



## CS (2021) -------------------------------------------------------------------
#' As we've seen, TWFE has problem in staggered setting with treatment
#' effect heterogeneity across treatment cohorts

#' We have some problems here: since we have a few years with only one treated
#' (2006 and 2010), we can't use ANY covariates.
#' In all cases with TWFE (except robbery), including covariates does not change
#' the significance nor the magnitude of the result, so we will heuristically
#' use that to justify not controlling for covariates here.
#' In this paragraph, 'covariates' includes the region-year fixed effects.

df_treated_by_year

### Homicides ------------------------------------------------------------------
#### No Controls ---------------------------------------------------------------
cs_homicide0 <- did::att_gt(
   yname = "l_homicide",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = NULL,
   data = df_main,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = FALSE,
   base_period = 'universal'
)

(cs_agg_dynamic_homicide0 <- did::aggte(cs_homicide0, 
                                        type = 'dynamic', min_e = -8))
(cs_agg_group_homicide0 <- did::aggte(cs_homicide0, type = 'group'))
(cs_agg_calendar_homicide0 <- did::aggte(cs_homicide0, type = 'calendar'))

cs_agg_dynamic_homicide0_trunc <- did::aggte(cs_homicide0, type = 'dynamic',
                                             min_e = -8, max_e = 3)

ggdid(cs_agg_dynamic_homicide0_trunc) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, "/cs_homicides_es.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_homicide0) + my_theme()
ggsave(paste0(str_figure_directory, "/cs_homicides_group.pdf"), 
       width = 9, height = 6, dpi = 600)

#' The positive results we got in the original papers seem to be driven entirely
#' by Florida (2006) and mainly due to 2010 (4th year after).
#' Furthermore, from -8 on, unconditional pre-trends holds. No reason to believe
#' that a time-varying confounder would act only when states take-up treatment
#' (at least none that I think of)

#' _NOTE_: for a single summary parameter, CS (2021) recommend the group method,
#' as it first computes the ATT (g) for each g (average across all time periods)
#' and then is aggregated using the proportion of G = g among those that are
#' eventually treated. This does not suffer from compositional changes.
#' In this case, it is 0.0632 (marginally insignificant).
#' Weaker evidence of increasing homicide, but definetely still something.

#### Region-by-Year Fixed Effects ----------------------------------------------
cs_homicide_region_year_fixed_effects <- did::att_gt(
   yname = "l_homicide",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = ~region_year,
   data = df_main,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = FALSE,  # important: TRUE throws error
   base_period = 'universal'
)

(cs_agg_dynamic_homicide_region_year_fixed_effects <- 
      did::aggte(cs_homicide_region_year_fixed_effects, 
                 type = 'dynamic', min_e = -8))

(cs_agg_dynamic_homicide_region_year_fixed_effects_trunc <- 
      did::aggte(cs_homicide_region_year_fixed_effects, 
                 type = 'dynamic', min_e = -8, max_e = 3))

(cs_agg_group_homicide_region_year_fixed_effects <- 
      did::aggte(cs_homicide_region_year_fixed_effects, type = 'group'))

ggdid(cs_agg_dynamic_homicide_region_year_fixed_effects_trunc) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, "/cs_homicides_es_region_year.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_homicide_region_year_fixed_effects) + my_theme()
ggsave(paste0(str_figure_directory, "/cs_homicides_group_region_year.pdf"), 
       width = 9, height = 6, dpi = 600)

#### No Florida nor Montana ----------------------------------------------------
# Data
df_main_no_florida_montana <- df_main %>% 
   dplyr::filter(state != "Montana", state != "Florida")

# Unconditional Parallel Trends
cs_homicide0_no_florida_montana <- did::att_gt(
   yname = "l_homicide",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = NULL,
   data = df_main_no_florida_montana,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = FALSE,
   base_period = 'universal'
)

(cs_agg_dynamic_homicide0_no_florida_montana <- 
      did::aggte(cs_homicide0_no_florida_montana, type = 'dynamic', min_e = -8))
(cs_agg_group_homicide0_no_florida_montana <- 
      did::aggte(cs_homicide0_no_florida_montana, type = 'group'))

ggdid(cs_agg_dynamic_homicide0_no_florida_montana) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, 
              "/cs_homicides_es_no_florida_montana.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_homicide0_no_florida_montana) + my_theme()
ggsave(paste0(str_figure_directory, 
              "/cs_homicides_group_no_florida_montana.pdf"), 
       width = 9, height = 6, dpi = 600)

# Using region-by-year fixed effects
cs_homicide0_no_florida_montana_region_year <- did::att_gt(
   yname = "l_homicide",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = ~region_year,
   data = df_main_no_florida_montana,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = FALSE,
   base_period = 'universal'
)

(cs_agg_dynamic_homicide0_no_florida_montana_region_year <- 
      did::aggte(cs_homicide0_no_florida_montana_region_year, 
                 type = 'dynamic', min_e = -8))
(cs_agg_group_homicide0_no_florida_montana_region_year <- 
      did::aggte(cs_homicide0_no_florida_montana_region_year, type = 'group'))

ggdid(cs_agg_dynamic_homicide0_no_florida_montana_region_year) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, 
              "/cs_homicides_es_no_florida_montana_region_year.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_homicide0_no_florida_montana_region_year) + my_theme()
ggsave(paste0(str_figure_directory, 
              "/cs_homicides_group_no_florida_montana_region_year.pdf"), 
       width = 9, height = 6, dpi = 600)

### Larceny --------------------------------------------------------------------
cs_larceny0 <- did::att_gt(
   yname = "l_larceny",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = NULL,
   data = df_main,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = TRUE,
   base_period = 'universal'
)

(cs_agg_dynamic_larceny0 <- did::aggte(cs_larceny0, type = 'dynamic',
                                       min_e = -8, max_e = 3))
(cs_agg_group_larceny0 <- did::aggte(cs_larceny0, type = 'group'))

ggdid(cs_agg_dynamic_larceny0) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, "/cs_larceny_es.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_larceny0) + my_theme()
ggsave(paste0(str_figure_directory, "/cs_larceny_group.pdf"), 
       width = 9, height = 6, dpi = 600)

### Motor ----------------------------------------------------------------------
cs_motor0 <- did::att_gt(
   yname = "l_motor",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = NULL,
   data = df_main,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = TRUE,
   base_period = 'universal'
)

(cs_agg_dynamic_motor0 <- did::aggte(cs_motor0, type = 'dynamic',
                                     min_e = -8, max_e = 3))
(cs_agg_group_motor0 <- did::aggte(cs_motor0, type = 'group'))

#' Outside of Florida & Montana, placebos still valid (no effect)
#' Furthermore, pre-trends holds! (almost for motor)
#' Can't even blame this on multiple hypothesis testing, as the bands are
#' simultaneous (not pointwise) and account for that.

ggdid(cs_agg_dynamic_motor0) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, "/cs_motor_es.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_motor0) + my_theme()
ggsave(paste0(str_figure_directory, "/cs_motor_group.pdf"), 
       width = 9, height = 6, dpi = 600)

### Burglary -------------------------------------------------------------------
cs_burglary0 <- did::att_gt(
   yname = "l_burglary",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = NULL,
   data = df_main,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = TRUE,
   base_period = 'universal'
)

(cs_agg_dynamic_burglary0 <- did::aggte(cs_burglary0, type = 'dynamic',
                                        min_e = -8, max_e = 3))
(cs_agg_group_burglary0 <- did::aggte(cs_burglary0, type = 'group'))

#' Opposite of deterrence: burglary goes up after castle laws, but violating
#' pre-trends at -8. Less solid evidence overall.

ggdid(cs_agg_dynamic_burglary0) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, "/cs_burglary_es.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_burglary0) + my_theme()
ggsave(paste0(str_figure_directory, "/cs_burglary_group.pdf"), 
       width = 9, height = 6, dpi = 600)

#' Cheng and Hoekstra also find a positive effects when not including 
#' region-year fixed effects. We will see if our results are robust to this.

df_treated_by_year

cs_burglary_region_year_fixed_effects <- did::att_gt(
   yname = "l_burglary",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = ~region_year,
   data = df_main,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = TRUE,
   base_period = 'universal'
)

(cs_agg_dynamic_burglary_region_year_fixed_effects <- 
      did::aggte(cs_burglary_region_year_fixed_effects, 
                 type = 'dynamic', min_e = -8, max_e = 3))

(cs_agg_group_burglary_region_year_fixed_effects <- 
      did::aggte(cs_burglary_region_year_fixed_effects, type = 'group'))

ggdid(cs_agg_dynamic_burglary_region_year_fixed_effects) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, "/cs_burglary_es_region_year.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_burglary_region_year_fixed_effects) + my_theme()
ggsave(paste0(str_figure_directory, "/cs_burglary_group_region_year.pdf"), 
       width = 9, height = 6, dpi = 600)


### Robbery --------------------------------------------------------------------
cs_robbery0 <- did::att_gt(
   yname = "l_robbery",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = NULL,
   data = df_main,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = TRUE,
   base_period = 'universal'
)

(cs_agg_dynamic_robbery0 <- did::aggte(cs_robbery0, type = 'dynamic',
                                       min_e = -8, max_e = 3))
(cs_agg_group_robbery0 <- did::aggte(cs_robbery0, type = 'group'))

#' Opposite of deterrence: some evidence that robbery goes up after castle laws
#' However, fucking Montana (2010) is again giving trouble.

ggdid(cs_agg_dynamic_robbery0) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, "/cs_robbery_es.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_robbery0) + my_theme()
ggsave(paste0(str_figure_directory, "/cs_robbery_group.pdf"), 
       width = 9, height = 6, dpi = 600)

### Assault --------------------------------------------------------------------
cs_assault0 <- did::att_gt(
   yname = "l_assault",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weightsname = "popwt",
   clustervars = c("sid"),
   xformla = NULL,
   data = df_main,
   control_group = "nevertreated",
   bstrap = TRUE,
   cband = TRUE,
   allow_unbalanced_panel = TRUE,
   base_period = 'universal'
)

(cs_agg_dynamic_assault0 <- did::aggte(cs_assault0, type = 'dynamic',
                                       min_e = -8, max_e = 3))
(cs_agg_group_assault0 <- did::aggte(cs_assault0, type = 'group'))

#' No deterrence.

ggdid(cs_agg_dynamic_assault0) + my_theme() + 
   labs(title = "", x = "Relative Time to Treatment", y = "ATT")
ggsave(paste0(str_figure_directory, "/cs_assault_es.pdf"), 
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_group_assault0) + my_theme()
ggsave(paste0(str_figure_directory, "/cs_assault_group.pdf"), 
       width = 9, height = 6, dpi = 600)


## Bacon Decomposition ---------------------------------------------------------
#' Will implement the Bacon decomposition of 2 x 2 DiDs.
#' The region-by-time fixed effects have to be included in the formula.

# Object
bacon_decomp <- bacondecomp::bacon(
   formula = l_homicide ~ cdl_1st,
   data = df_main,
   id_var = "sid",
   time_var = "year"
)

# Checking sums
(dd_estimate <- sum(bacon_decomp$estimate * bacon_decomp$weight))
twfe_homicide0_no_weights


#' Even though there is a later to early 2×2 in the mix, as there always will
#' be with any differential timing, it is small in terms of influence and 
#' ultimately pulls down the estimate (CUNNINGHAM, 2017, p. 501).

#' Negative weights in TWFE is not much of a problem; main issue is 
#' results relying much on Florida.
#' Highest weight is given to 2007 x untreated

# Plot
bacon_decomp %>% 
   ggplot(aes(x = weight, y = estimate, color = type, shape = type)) +
   geom_point(size = 2) + 
   geom_hline(yintercept = dd_estimate, color = 'black', linetype = 'dashed') +
   xlab("Weight") +
   ylab("2x2 DiD Estimate") +
   theme_classic() + 
   theme(
      legend.title = element_blank(),
      legend.position = 'top',
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
   )
ggsave(paste0(str_figure_directory, "/bacon_decomp.pdf"),
       width = 6, height = 4, dpi = 600)


## Dynamic Specifications and All Estimators -----------------------------------
#' Going to overkill this and use many estimators.
#' My main concern is efficiency: although CS is 'right', it can have high SDs.
#' Using a modified function from https://github.com/kylebutts/did2s to allow
#' for weights and unbalanced panels in Callaway and Sant'Anna (2021)

funcs <- new.env()
source("event_study.R", local = funcs, encoding = 'UTF-8')

### Homicides ------------------------------------------------------------------
event_study_homicide = funcs$event_study(
   yname = "l_homicide",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weights = "popwt",
   xformla = NULL,
   data = df_main,
   estimator = "all"
)

funcs$plot_event_study(event_study_homicide, 
                       separate = FALSE, horizon = c(-8, 3))

ggsave(paste0(str_figure_directory, "/es_homicide_no_controls.pdf"),
       width = 9, height = 6, dpi = 600)

ggdid(cs_agg_dynamic_homicide0)

#' CS (2021) use simultaneous confidence bands, while `event_study` uses 
#' pairwise. With this exception, all else is the same with `weights` change.

event_study_homicide_region_year = funcs$event_study(
   yname = "l_homicide",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weights = "popwt",
   xformla = ~region_year,
   data = df_main,
   estimator = "all"
)

funcs$plot_event_study(event_study_homicide_region_year, 
                       separate = FALSE, horizon = c(-8, 3))

ggsave(paste0(str_figure_directory, "/es_homicide_region_year.pdf"),
       width = 9, height = 6, dpi = 600)

### Larceny --------------------------------------------------------------------
event_study_larceny <- funcs$event_study(
   yname = "l_larceny",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weights = "popwt",
   xformla = NULL,
   data = df_main,
   estimator = "all"
)

funcs$plot_event_study(event_study_larceny, 
                       separate = FALSE, horizon = c(-8, 3))

ggsave(paste0(str_figure_directory, "/es_larceny_no_controls.pdf"),
       width = 9, height = 6, dpi = 600)

### Motor ----------------------------------------------------------------------
event_study_motor <- funcs$event_study(
   yname = "l_motor",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weights = "popwt",
   xformla = NULL,
   data = df_main,
   estimator = "all"
)

funcs$plot_event_study(event_study_motor, 
                       separate = FALSE, horizon = c(-8, 3))

ggsave(paste0(str_figure_directory, "/es_motor_no_controls.pdf"),
       width = 9, height = 6, dpi = 600)

### Burglary -------------------------------------------------------------------
event_study_burglary <- funcs$event_study(
   yname = "l_burglary",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weights = "popwt",
   xformla = NULL,
   data = df_main,
   estimator = "all"
)

funcs$plot_event_study(event_study_burglary, 
                       separate = FALSE, horizon = c(-8, 3))

ggsave(paste0(str_figure_directory, "/es_burglary_no_controls.pdf"),
       width = 9, height = 6, dpi = 600)

### Robbery --------------------------------------------------------------------
event_study_robbery <- funcs$event_study(
   yname = "l_robbery",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weights = "popwt",
   xformla = NULL,
   data = df_main,
   estimator = "all"
)

funcs$plot_event_study(event_study_robbery, 
                       separate = FALSE, horizon = c(-8, 3))

ggsave(paste0(str_figure_directory, "/es_robbery_no_controls.pdf"),
       width = 9, height = 6, dpi = 600)

### Assault --------------------------------------------------------------------
event_study_assault <- funcs$event_study(
   yname = "l_assault",
   tname = "year",
   idname = "sid",
   gname = "first_treat",
   weights = "popwt",
   xformla = NULL,
   data = df_main,
   estimator = "all"
)

funcs$plot_event_study(event_study_assault, 
                       separate = FALSE, horizon = c(-8, 3))

ggsave(paste0(str_figure_directory, "/es_assault_no_controls.pdf"),
       width = 9, height = 6, dpi = 600)

## Sensitivity Analysis --------------------------------------------------------
#' Will employ the sensitivity analysis of Rambachan and Roth (2023), 
#' which uses bounds based on pre-trends deviations. Focusing only on CS (2021).

funcs_sens <- new.env()
source("sensitivity_CS_2021_RR2023.R", local = funcs_sens, encoding = 'UTF-8')

### No Controls ----------------------------------------------------------------
tic()
# Running honest_did 
cs_homicide0_honest <- funcs_sens$honest_did.AGGTEobj(
   es = cs_agg_dynamic_homicide0, e = 0, type = "relative_magnitude"
)
cs_homicide0_honest

# Drop 0 as that is not really allowed
cs_homicide0_honest$robust_ci <- cs_homicide0_honest$robust_ci[-1,]

# Plot
HonestDiD::createSensitivityPlot_relativeMagnitudes(
   cs_homicide0_honest$robust_ci,
   cs_homicide0_honest$orig_ci
) + 
   labs(y = "Point Estimate and 95% Confidence Intervals") + 
   theme_classic() + 
   theme(
      legend.position = 'none',
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
   )
ggsave(paste0(str_figure_directory, "/sens_homicide_no_controls_cs.pdf"),
       width = 9, height = 6, dpi = 600)
toc()  # 2min

### Region-Year ----------------------------------------------------------------
tic()
# Running honest_did 
cs_homicide0_region_year_honest <- funcs_sens$honest_did.AGGTEobj(
   es = cs_agg_dynamic_homicide_region_year_fixed_effects, 
   e = 0, type = "relative_magnitude"
)

# Drop 0 as that is not really allowed
cs_homicide0_region_year_honest$robust_ci <- 
   cs_homicide0_region_year_honest$robust_ci[-1,]

# Plot
HonestDiD::createSensitivityPlot_relativeMagnitudes(
   cs_homicide0_region_year_honest$robust_ci,
   cs_homicide0_region_year_honest$orig_ci
) + 
   labs(y = "Point Estimate and 95% Confidence Intervals") + 
   theme_classic() + 
   theme(
      legend.position = 'none',
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
   )
ggsave(paste0(str_figure_directory, "/sens_homicide_region_year_cs.pdf"),
       width = 9, height = 6, dpi = 600)
toc()  # 2min

## Alternative Inference -------------------------------------------------------
#' ~20 treated and 30 controls may be too few to rely on asymptotics.
#' This is corroborated by the fact that they find that in the permutation
#' tests that they are overrjecting. 

#' Furthermore, the overlap assumption is particularly important when using
#' standard inference procedures; given the continuous and time-varying
#' controls they use, this probably fails.

#' _NOTE_: when overlap holds (or when not using covariates), Supp. Appendix C 
#' of Callaway and Sant'Anna (2021) shows that their estimator has relatively
#' good finite sample properties in a setting similar to ours (n = 50, T = 20, 
#' G = 4 in their case; the only difference is that we have T = 11).
#' The main problem is when we have few observations in a treatment group, 
#' which can lead to overrjection problems in their bootstrap procedure.

### IID Normal Simulations -----------------------------------------------------
#' Ferman (2019) proposes substituting the outcomes with iid standard normals.
#' In this case, the null of no treatment is satisfied, and so, over B -> \infty
#' repetitions of the dataset, we expect a rejection rate close to \alpha if we
#' have valid inference.
#' This is a "minimum requirement test", in the sense that passing it does not
#' imply valid inference, but having size distortions even in the best case
#' of outcomes being iid normal should raise red flags.
#' Importantly, it does not detect problems associated with spatial correlation.
#' Reference: https://arxiv.org/abs/1912.08772
#' (They also have a nice discussion on using block bootstraps!)

# Number of simulations, length of dataframe and size of test
B_TWFE <- 5000
n <- nrow(df)
alpha <- .05

#### TWFE ----------------------------------------------------------------------
# Will run simulations with and without weights
twfe_normal_cdl_p <- rep(NA, B_TWFE)
twfe_wt_normal_cdl_p <- rep(NA, B_TWFE)

tic()
for (b in 1:B_TWFE){
   # Simulating normals
   df$iid_normal <- rnorm(n)
   
   # Running TWFE with sid-clustered standard errors and their preferred spec.
   iid_homice_twfe <- fixest::feols(
      dd_formula_repl("iid_normal"),
      data = df,
      cluster = c("sid")
   )
   
   # P-values
   twfe_normal_cdl_p[b] <- fixest::pvalue(iid_homice_twfe)['cdl']
   
   # Simulating normals (doing it again to avoid any dependence)
   df$iid_normal <- rnorm(n)
   
   # Running TWFE with sid-clustered standard errors and populational weights
   iid_homice_twfe_wt <- fixest::feols(
      dd_formula_repl("iid_normal"),
      data = df,
      weights = df$popwt,
      cluster = c("sid")
   )
   
   # P-values
   twfe_wt_normal_cdl_p[b] <- fixest::pvalue(iid_homice_twfe_wt)['cdl']
}
toc() # 6min with 5000 simulations

# Rejection rates (5000 simulations)
(rej_rate_twfe_iid = (1/B_TWFE) * sum(twfe_normal_cdl_p < alpha)) # 6.22%
(rej_rate_twfe_wt_iid = (1/B_TWFE) * sum(twfe_wt_normal_cdl_p < alpha)) # 8.98%

#' More overrjection with weights, as said by Ferman (2022).

# Plotting CDF of p-values
## Dataframe
df_pvalues_twfe <- rbind(
   data.frame(
      p_value = sort(twfe_normal_cdl_p),
      ecdf = ecdf(twfe_normal_cdl_p)(sort(twfe_normal_cdl_p)),
      spec = 'Unweighted'
   ),
   data.frame(
      p_value = sort(twfe_wt_normal_cdl_p),
      ecdf = ecdf(twfe_wt_normal_cdl_p)(sort(twfe_wt_normal_cdl_p)),
      spec = 'Weighted'
   )
)

## Plotting
df_pvalues_twfe %>% 
   ggplot(aes(x = p_value, y = ecdf, color = spec)) + 
   geom_line(size = 1) + 
   geom_abline(slope = 1, intercept = 0, 
               linetype = 'dashed', linewidth = .5, color = 'black') +
   labs(x = "P-Value", y = "CDF") +
   theme_bw() + 
   theme(
      legend.title = element_blank(),
      legend.position = 'top',
      legend.text = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
   )

ggsave(paste0(str_figure_directory, "/pvalue_cdf_sim_iid_normal_twfe.pdf"), 
       width = 9, height = 6, dpi = 600)

#### CS (2021) -----------------------------------------------------------------
#' For better comparability with TWFE, will focus only on summary measures of
#' the group and dynamic effects (which align more with our target parameter, 
#' which is the aggregate effect rather than for a particular group), 
#' and not on the effect for a particular group.
#' Since this is the case where we have few treated clusters and we are using
#' populations as weights, Ferman and Pinto (2019) seems like a natural way to
#' go if we were interested in these coefficients (and willing to make
#' parametric assumption on the functional form of errors).
#' Source: https://doi.org/10.1162/rest_a_00759

# A function will be useful
estimate_cs_compute_significance <- function(y, data, weights = NULL, 
                                             xformla = NULL, alpha = .05){
   # Estimating
   ## Formula
   if (!is.null(xformla)){
      xformla = as.formula(xformla)
   }
   
   ## Object
   cs_object <- did::att_gt(
      yname = y,
      tname = "year",
      idname = "sid",
      gname = "first_treat",
      weightsname = weights,
      clustervars = c("sid"),
      xformla = xformla,
      data = data,
      control_group = "nevertreated",
      bstrap = TRUE,
      cband = TRUE,
      allow_unbalanced_panel = FALSE,
      base_period = 'universal',
      pl = TRUE, cores = 3
   )
   
   # Assessing significance
   ## Objects
   cs_object_agg_dynamic <- did::aggte(cs_object, type = 'dynamic')
   cs_object_agg_group <- did::aggte(cs_object, type = 'group')
   
   ## T-statistic
   t <- qnorm(1 - alpha / 2)
   
   ## Upper and lower bounds of confidence intervals for aggregate parameters
   ub_dynamic <- cs_object_agg_dynamic$overall.att + 
      t*cs_object_agg_dynamic$overall.se
   lb_dynamic <- cs_object_agg_dynamic$overall.att - 
      t*cs_object_agg_dynamic$overall.se
   
   ub_group <- cs_object_agg_group$overall.att + 
      t*cs_object_agg_dynamic$overall.se
   lb_group <- cs_object_agg_group$overall.att - 
      t*cs_object_agg_dynamic$overall.se
   
   ## Assessing sign chances: different signs = not significant
   signif_dynamic <- ifelse(sign(ub_dynamic) == sign(lb_dynamic), 1, 0)
   signif_group <- ifelse(sign(ub_group) == sign(lb_group), 1, 0)
   
   return(c(signif_dynamic, signif_group))
}

# Objects
## This will be matrices to store dynamic and group effect
B <- 1000
skeleton <- matrix(rep(NA, 2*B), nrow = B, ncol = 2)

cs_wt_normal_cdl_sig <- skeleton
cs_wt_normal_cdl_sig_region_year <- skeleton

cs_normal_cdl_sig <- skeleton
cs_normal_cdl_sig_region_year <- skeleton

cs_normal_wt_normal_cdl_sig_nfm <- skeleton
cs_normal_wt_normal_cdl_sig_region_year_nfm <- skeleton

# Simulations
tic()
suppressWarnings({
   for (b in 1:B){
      
      # Printing progress
      if (b %% 10 == 0){
         print(glue("Simulation {b} / {B} ..."))
      }
      
      # Simulating iid normals
      df_main$iid_normal <- rnorm(n)
      
      # Estimating without controls and with weights
      cs_wt_normal_cdl_sig[b, ] <- estimate_cs_compute_significance(
         y = "iid_normal", data = df_main, weights = "popwt", 
         xformla = NULL, alpha = alpha
      )
      
      # Simulating normals again to avoid any dependence
      df_main$iid_normal <- rnorm(n)
      
      # Estimating with region_year fixed effects and with weights
      cs_wt_normal_cdl_sig_region_year[b, ] <- estimate_cs_compute_significance(
         y = "iid_normal", data = df_main, weights = "popwt", 
         xformla = "~region_year", alpha = alpha
      )
      
      # Simulating normals again to avoid any dependencee
      df_main$iid_normal <- rnorm(n)
      
      # Estimating without controls and without weights
      cs_normal_cdl_sig[b, ] <- estimate_cs_compute_significance(
         y = "iid_normal", data = df_main, weights = NULL, 
         xformla = NULL, alpha = alpha
      )
      
      # Simulating normals again to avoid any dependence
      df_main$iid_normal <- rnorm(n)
      
      # Estimating with region_year fixed effects and without weights
      cs_normal_cdl_sig_region_year[b, ] <- estimate_cs_compute_significance(
         y = "iid_normal", data = df_main, weights = NULL, 
         xformla = "~region_year", alpha = alpha
      )
      
      # Simulating normals again to avoid any dependence
      df_main_no_florida_montana$iid_normal <- rnorm(n - 22)
      
      # Estimating weighted model without Florida and Montana
      cs_normal_wt_normal_cdl_sig_nfm[b, ] <- estimate_cs_compute_significance(
         y = "iid_normal", data = df_main_no_florida_montana, 
         weights = NULL, xformla = NULL, alpha = alpha
      )
      
      # Simulating normals again to avoid any dependence
      df_main_no_florida_montana$iid_normal <- rnorm(n - 22)
      
      # Estimating weighted model without Florida and Montana
      cs_normal_wt_normal_cdl_sig_region_year_nfm[b, ] <- 
         estimate_cs_compute_significance(
            y = "iid_normal", data = df_main_no_florida_montana, 
            weights = NULL, xformla = "~region_year", alpha = alpha
         )
   }
})
toc()  # 1h4min with 1000 simulations

# Rejection rates (1000 simulations)
## Without weights and without region-year fixed effects
(rej_rate_cs_dynamic_iid <- 1 / B * sum(cs_normal_cdl_sig[, 1])) # 20.2% (!)
(rej_rate_cs_group_iid <- 1 / B * sum(cs_normal_cdl_sig[, 2])) # 7.7%

## Without weights and with region-year fixed effects
## 18% (!) and 7.6%, respectively
(rej_rate_cs_ry_dynamic_iid <- 1 / B * sum(cs_normal_cdl_sig_region_year[, 1]))
(rej_rate_cs_ry_group_iid <- 1 / B * sum(cs_normal_cdl_sig_region_year[, 2]))

## With weights and without region-year fixed effects
(rej_rate_cs_wt_dynamic_iid <- 1 / B * sum(cs_wt_normal_cdl_sig[, 1])) # 21.3%
(rej_rate_cs_wt_group_iid <- 1 / B * sum(cs_wt_normal_cdl_sig[, 2])) # 11.4%

## With weights and with region-year fixed effects
(rej_rate_cs_wt_ry_dynamic_iid <- 1 / B * 
      sum(cs_wt_normal_cdl_sig_region_year[, 1])) # 19% (!)

(rej_rate_cs_wt_ry_group_iid <- 1 / B * 
      sum(cs_wt_normal_cdl_sig_region_year[, 2])) # 10.6% (!)

# Without Florida and Montana
## Without region-year fixed effects
(rej_rate_cs_wt_nfm_dynamic_iid <- 1 / B * 
      sum(cs_normal_wt_normal_cdl_sig_nfm[, 1])) # 6.2%

(rej_rate_cs_wt_nfm_group_iid <- 1 / B * 
      sum(cs_normal_wt_normal_cdl_sig_nfm[, 2])) # 5.3%

## With region-year fixed effects
(rej_rate_cs_wt_ry_nfm_dynamic_iid <- 1 / B * 
      sum(cs_normal_wt_normal_cdl_sig_region_year_nfm[, 1])) # 5.9%

(rej_rate_cs_wt_ry_nfm_group_iid <- 1 / B * 
      sum(cs_normal_wt_normal_cdl_sig_region_year_nfm[, 2])) # 5.0%

#' Interestingly, even when using iid normals, removing groups with only one
#' treated improves the validity A LOT.

# Table 
# Create the dataframe
rejection_rates <- 100 * data.frame(
   no_wt = c(
      rej_rate_cs_dynamic_iid, 
      rej_rate_cs_ry_dynamic_iid, 
      rej_rate_cs_group_iid, 
      rej_rate_cs_ry_group_iid
   ),
   wt = c(
      rej_rate_cs_wt_dynamic_iid, 
      rej_rate_cs_wt_ry_dynamic_iid, 
      rej_rate_cs_wt_group_iid, 
      rej_rate_cs_wt_ry_group_iid
   ),
   wt_nfm = c(
      rej_rate_cs_wt_nfm_dynamic_iid, 
      rej_rate_cs_wt_ry_nfm_dynamic_iid, 
      rej_rate_cs_wt_nfm_group_iid,
      rej_rate_cs_wt_ry_nfm_group_iid
   )
)

# Add row labels for clarity
rownames(rejection_rates) <- c(
   "Dynamic IID (no region-year effects)",
   "Dynamic IID (region-year effects)",
   "Group IID (no region-year effects)",
   "Group IID (region-year effects)"
)

# Print the dataframe
print(rejection_rates)

# Export as a LaTeX table
latex_table_rej_rate_cs <- xtable(t(rejection_rates), na.string = "-", digits = 1)

print(latex_table_rej_rate_cs, type = "latex", 
      file = paste0(str_table_directory, "/rejection_rates_table.tex"))
