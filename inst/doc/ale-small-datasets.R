## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ale)

## ----attitude_str-------------------------------------------------------------
str(attitude)

## ----attitude_summary---------------------------------------------------------
summary(attitude)

## ----lm_summary---------------------------------------------------------------
lm_attitude <- lm(rating ~ ., data = attitude)

summary(lm_attitude)

## ----lm_simple, fig.width=7, fig.height=7-------------------------------------
ale_lm_attitude_simple <- ale(attitude, lm_attitude)

# Print all plots
gridExtra::grid.arrange(grobs = ale_lm_attitude_simple$plots, ncol = 2)

## ----lm_ixn, fig.width=7, fig.height=7----------------------------------------
ale_lm_attitude_ixn <- ale_ixn(attitude, lm_attitude)

# Print plots
ale_lm_attitude_ixn$plots |>
  purrr::walk(\(.x1) {  # extract list of x1 ALE outputs
    gridExtra::grid.arrange(grobs = .x1, ncol = 2)  # plot all x1 plots
  })

## ----lm_full_call-------------------------------------------------------------
mb_lm <- model_bootstrap(
  attitude, 
  'lm(rating ~ .)',
  boot_it = 10,  # 100 by default but reduced here for a faster demonstration
  silent = TRUE  # progress bars disabled for the vignette
)

## ----lm_full_stats------------------------------------------------------------
mb_lm$model_stats

## ----lm_full_coefs------------------------------------------------------------
mb_lm$model_coefs

## ----lm_full_ale, fig.width=7, fig.height=7-----------------------------------
gridExtra::grid.arrange(grobs = mb_lm$ale$plots, ncol = 2)

## ----gam_summary--------------------------------------------------------------
gam_attitude <- mgcv::gam(rating ~ complaints + privileges + s(learning) +
                            raises + s(critical) + advance,
                          data = attitude)
summary(gam_attitude)

## ----gam_simple, fig.width=7, fig.height=7------------------------------------
ale_gam_attitude_simple <- ale(attitude, gam_attitude)

gridExtra::grid.arrange(grobs = ale_gam_attitude_simple$plots, ncol = 2)

## ----gam_full_stats-----------------------------------------------------------
mb_gam <- model_bootstrap(
  attitude, 
  'mgcv::gam(rating ~ complaints + privileges + s(learning) +
               raises + s(critical) + advance)', 
  boot_it = 10,  # 100 by default but reduced here for a faster demonstration
  silent = TRUE  # progress bars disabled for the vignette
  )
mb_gam$model_stats

## ----gam_full_coefs-----------------------------------------------------------
mb_gam$model_coefs

## ----gam_full_ale, fig.width=7, fig.height=7----------------------------------
gridExtra::grid.arrange(grobs = mb_gam$ale$plots, ncol = 2)

