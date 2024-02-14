## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load libraries-----------------------------------------------------------
library(mgcv)   # for datasets and the gam function
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

## ----enable progressr, eval = FALSE-------------------------------------------
#  # Run this in an R console; it will not work directly within an R Markdown or Quarto block
#  progressr::handlers(global = TRUE)
#  progressr::handlers('cli')

## ----train gam model----------------------------------------------------------
gam_math <- gam(
     math_avg ~ public + high_minority +
     s(size) + s(academic_ratio) + s(female_ratio) + s(mean_ses) + 
     s(minority_ratio) + s(discrim) + s(rand_norm),
     data = math
   )

gam_math

## ----p_funs-------------------------------------------------------------------
# # To generate the code, uncomment the following lines.
# # But it is slow because it retrains the model 1000 times,
# # so this vignette loads a pre-created p-values object.
# gam_math_p_funs <- create_p_funs(
#   math,
#   gam_math
# )
# saveRDS(gam_math_p_funs, file.choose())
gam_math_p_funs <- url('https://github.com/tripartio/ale/raw/main/download/gam_math_p_funs.rds') |> 
  readRDS()

## ----model bootstrap----------------------------------------------------------
mb_gam_math <- model_bootstrap(
  math, 
  gam_math,
  # Pass the p_funs object so that p-values will be generated
  ale_options = list(p_values = gam_math_p_funs),
  # For the GAM model coefficients, show details of all variables, parametric or not
  tidy_options = list(parametric = TRUE),
  # tidy_options = list(parametric = NULL),
  boot_it = 40,  # 100 by default but reduced here for a faster demonstration
  parallel = 2  # CRAN limit (delete this line on your own computer for faster speed)
)

## ----model_stats--------------------------------------------------------------
mb_gam_math$model_stats

## ----model_coefs--------------------------------------------------------------
mb_gam_math$model_coefs

## ----model_coefs stat sig variables-------------------------------------------
mb_gam_math$model_coefs |> 
  # filter is TRUE if conf.low and conf.high are both positive or both negative because
  # multiplying two numbers of the same sign results in a positive number.
  filter((conf.low * conf.high) > 0)

## ----all-ALE-plots, fig.width=7, fig.height=10--------------------------------
mb_gam_math$ale$plots |> 
  patchwork::wrap_plots(ncol = 2)

## ----model-bootstrap-without-p_funs, fig.width=7, fig.height=10---------------
mb_gam_no_p <- model_bootstrap(
  math, 
  gam_math,
  # For the GAM model coefficients, show details of all variables, parametric or not
  tidy_options = list(parametric = TRUE),
  # tidy_options = list(parametric = NULL),
  boot_it = 40,  # 100 by default but reduced here for a faster demonstration
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

mb_gam_no_p$ale$plots |> 
  patchwork::wrap_plots(ncol = 2)

## ----ALE-effects-plot, fig.width=8, fig.height=6------------------------------
mb_gam_math$ale$stats$effects_plot

## ----ALE-plot-for-public-1----------------------------------------------------
mb_gam_math$ale$plots$public

## ----ALE stats for public-----------------------------------------------------
mb_gam_math$ale$stats$by_term$public

## ----ALE-plot-forl-academic_ratio-1-------------------------------------------
mb_gam_math$ale$plots$academic_ratio

## ----ALE stats for academic_ratio---------------------------------------------
mb_gam_math$ale$stats$by_term$academic_ratio

## ----math-rand_norm-ALE-plot, fig.width=3.5, fig.height=3---------------------
mb_gam_math$ale$plots$rand_norm

## ----ALE stats for rand_norm--------------------------------------------------
mb_gam_math$ale$stats$by_term$rand_norm

## ----ale data for public------------------------------------------------------
mb_gam_math$ale$data$public

## ----ale data for academic_ratio----------------------------------------------
mb_gam_math$ale$data$academic_ratio

## ----ALE-plot-for-mean_ses----------------------------------------------------
mb_gam_math$ale$plots$mean_ses

## ----conf_regions for mean_ses------------------------------------------------
mb_gam_math$ale$conf_regions$by_term$mean_ses

## ----text conf_regions for mean_ses-------------------------------------------
ale:::summarize_conf_regions_in_words(mb_gam_math$ale$conf_regions$by_term$mean_ses)

## ----ALE-plot-for-public-2----------------------------------------------------
mb_gam_math$ale$plots$public

## ----conf_regions for public--------------------------------------------------
mb_gam_math$ale$conf_regions$by_term$public

## ----ALE-plot-for-rand_norm---------------------------------------------------
mb_gam_math$ale$plots$rand_norm

## ----conf_regions for rand_norm-----------------------------------------------
mb_gam_math$ale$conf_regions$by_term$rand_norm

## ----significant conf_regions-------------------------------------------------
mb_gam_math$ale$conf_regions$significant

## ----variables with significant conf_regions----------------------------------
mb_gam_math$ale$conf_regions$significant$term |> 
  unique()

