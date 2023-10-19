## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries----------------------------------------------------------------
library(mgcv)  # for datasets and for gam function
library(dplyr)  # for data manipulation
library(ale)

## ----data setup---------------------------------------------------------------
# Create and prepare the data

# Specific seed chosen to illustrate the spuriousness of the random variable
set.seed(6)  

math <- 
  # Start with math achievement scores per student
  MathAchieve |> 
  as_tibble() |> 
  mutate(
    school = School |> as.character() |>  as.integer(),
    minority = Minority == 'Yes',
    female = Sex == 'Female'
  ) |> 
  # summarize the scores to give per-school values
  summarize(
    .by = school,
    minority_ratio = mean(minority),
    female_ratio = mean(female),
    math_avg = mean(MathAch),
  ) |> 
  # merge the summarized student data with the school data
  inner_join(
    MathAchSchool |> 
      mutate(school = School |> as.character() |>  as.integer()),
    by = c('school' = 'school')
  ) |> 
  mutate(
    public = Sector == 'Public',
    high_minority = HIMINTY == 1,
  ) |> 
  select(-School, -Sector, -HIMINTY) |> 
  rename(
    size = Size,
    academic_ratio = PRACAD,
    discrim = DISCLIM,
    mean_ses = MEANSES,
  ) |> 
  # Remove ID column for analysis
  select(-school) |> 
  select(
    math_avg, size, public, academic_ratio,
    female_ratio, mean_ses, minority_ratio, high_minority, discrim,
    everything()
  ) |> 
  mutate(
    rand_norm = rnorm(nrow(MathAchSchool)) 
  )

glimpse(math)

## ----y summary----------------------------------------------------------------
summary(math$math_avg)

## ----model bootstrap----------------------------------------------------------
mb_gam <- model_bootstrap(
  math, 
  'gam(
     math_avg ~ public + high_minority +
     s(size) + s(academic_ratio) + s(female_ratio) + s(mean_ses) + 
     s(minority_ratio) + s(discrim) + s(rand_norm)
   )',
  # For the GAM model coefficients, show details of all variables, parametric or not
  tidy_options = list(parametric = TRUE),
  # tidy_options = list(parametric = NULL),
  boot_it = 40,  # 100 by default but reduced here for a faster demonstration
  silent = TRUE  # progress bars disabled for the vignette
)

## ----model_stats--------------------------------------------------------------
mb_gam$model_stats

## ----model_coefs--------------------------------------------------------------
mb_gam$model_coefs

## ----model_coefs stat sig variables-------------------------------------------
mb_gam$model_coefs |> 
  # filter is TRUE if conf.low and conf.high are both positive or both negative because
  # multiplying two numbers of the same sign results in a positive number.
  filter((conf.low * conf.high) > 0)

## ----all ALE plots, fig.width=7, fig.height=10--------------------------------
gridExtra::grid.arrange(grobs = mb_gam$ale$plots, ncol = 2)

## ----ALE effects plot, fig.width=8, fig.height=6------------------------------
mb_gam$ale$stats$effects_plot

## ----ALE stats for public-----------------------------------------------------
mb_gam$ale$stats$by_term$public

## ----ALE stats for academic_ratio---------------------------------------------
mb_gam$ale$stats$by_term$academic_ratio

## ----ALE stats for rand_norm--------------------------------------------------
mb_gam$ale$stats$by_term$rand_norm

## ----ale data for public------------------------------------------------------
mb_gam$ale$data$public

## ----ale data for academic_ratio----------------------------------------------
mb_gam$ale$data$academic_ratio

## ----ALE plot for academic_ratio----------------------------------------------
mb_gam$ale$plots$academic_ratio

## ----conf_regions for academic_ratio------------------------------------------
mb_gam$ale$conf_regions$academic_ratio

## ----text conf_regions for academic_ratio-------------------------------------
ale:::summarize_conf_regions_in_words(mb_gam$ale$conf_regions$academic_ratio)

## ----ALE plot for public------------------------------------------------------
mb_gam$ale$plots$public

## ----conf_regions for public--------------------------------------------------
mb_gam$ale$conf_regions$public

## ----ALE plot for rand_norm---------------------------------------------------
mb_gam$ale$plots$rand_norm

## ----conf_regions for rand_norm-----------------------------------------------
mb_gam$ale$conf_regions$rand_norm

