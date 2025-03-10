## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

 
## ----load libraries-----------------------------------------------------------
library(ale)

## ----attitude_str-------------------------------------------------------------
str(attitude)

## ----attitude_summary---------------------------------------------------------
summary(attitude)

## ----lm_summary---------------------------------------------------------------
lm_attitude <- lm(rating ~ ., data = attitude)

summary(lm_attitude)

## ----enable progressr, eval = FALSE-------------------------------------------
#  # Run this in an R console; it will not work directly within an R Markdown or Quarto block
#  progressr::handlers(global = TRUE)
#  progressr::handlers('cli')

## ----lm_simple, fig.width=7, fig.height=7-------------------------------------
ale_lm_attitude_simple <- ale(
  attitude, lm_attitude,
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

# Print all plots
ale_lm_attitude_simple$plots |> 
  patchwork::wrap_plots(ncol = 2)

## ----lm_ixn, fig.width=7, fig.height=7----------------------------------------
ale_lm_attitude_ixn <- ale_ixn(
  attitude, lm_attitude,
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

# Print plots
ale_lm_attitude_ixn$plots |>
  # extract list of x1 ALE outputs
  purrr::walk(\(.x1) {  
    # plot all x2 plots in each .x1 element
    patchwork::wrap_plots(.x1, ncol = 2) |> 
      print()
  })

## ----lm_full_call-------------------------------------------------------------
mb_lm <- model_bootstrap(
  attitude, 
  lm_attitude,
  boot_it = 10,  # 100 by default but reduced here for a faster demonstration
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

## ----lm_full_stats------------------------------------------------------------
mb_lm$model_stats

## ----lm_full_coefs------------------------------------------------------------
mb_lm$model_coefs

## ----lm_full_ale, fig.width=7, fig.height=7-----------------------------------
mb_lm$ale$plots |> 
  patchwork::wrap_plots(ncol = 2)

## ----gam_summary--------------------------------------------------------------
gam_attitude <- mgcv::gam(rating ~ complaints + privileges + s(learning) +
                            raises + s(critical) + advance,
                          data = attitude)
summary(gam_attitude)

## ----gam_simple, fig.width=7, fig.height=7------------------------------------
ale_gam_attitude_simple <- ale(
  attitude, gam_attitude,
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

ale_gam_attitude_simple$plots |> 
  patchwork::wrap_plots(ncol = 2)

## ----gam_full_stats-----------------------------------------------------------
mb_gam <- model_bootstrap(
  attitude, 
  gam_attitude, 
  boot_it = 10,  # 100 by default but reduced here for a faster demonstration
  parallel = 2  # CRAN limit (delete this line on your own computer)
  )
mb_gam$model_stats

## ----gam_full_coefs-----------------------------------------------------------
mb_gam$model_coefs

## ----gam_full_ale, fig.width=7, fig.height=7----------------------------------
mb_gam$ale$plots |> 
  patchwork::wrap_plots(ncol = 2)

## ----gam_summary_repeat-------------------------------------------------------
gam_attitude_again <- mgcv::gam(rating ~ complaints + privileges + s(learning) +
                                  raises + s(critical) + advance,
                                data = attitude)
summary(gam_attitude_again)

## ----model_call_string--------------------------------------------------------
mb_gam_non_standard <- model_bootstrap(
  attitude,
  gam_attitude_again,
  model_call_string = 'mgcv::gam(rating ~ complaints + privileges + s(learning) +
                                  raises + s(critical) + advance,
                                data = boot_data)', 
  boot_it = 10,  # 100 by default but reduced here for a faster demonstration
  parallel = 2  # CRAN limit (delete this line on your own computer)
  )
mb_gam_non_standard$model_stats

