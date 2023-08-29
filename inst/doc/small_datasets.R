## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggplot2)
library(ale)

## ----attitude_help------------------------------------------------------------
help(attitude)

## ----attitude_str-------------------------------------------------------------
str(attitude)

## ----attitude_summary---------------------------------------------------------
summary(attitude)

## ----lm_summary---------------------------------------------------------------
lm_attitude <- lm(rating ~ ., data = attitude)

summary(lm_attitude)

## ----lm_simple----------------------------------------------------------------
ale_lm_attitude_simple <- ale(attitude, lm_attitude, boot_it = 0)

# Print all plots
# Skip .common_data when iterating through the data for plotting
ale_lm_attitude_simple[setdiff(names(ale_lm_attitude_simple), '.common_data')] |> 
  purrr::map(\(.x) .x$plot) |>  # extract plots as a list
  gridExtra::grid.arrange(grobs = _, ncol = 2)

## ----lm_ixn-------------------------------------------------------------------
ale_lm_attitude_ixn <- ale_ixn(attitude, lm_attitude, boot_it = 0)

# Skip .common_data when iterating through the data for plotting
ale_lm_attitude_ixn[setdiff(names(ale_lm_attitude_ixn), '.common_data')] |> 
  purrr::walk(\(x1) {  # extract list of x1 ALE outputs
    purrr::map(x1, \(.x) .x$plot) |>  # for each x1, extract list of x2 ALE outputs
      gridExtra::grid.arrange(grobs = _, ncol = 2)  # plot all x1 plots
  })

## ----lm_full_call-------------------------------------------------------------
mb_lm <- model_bootstrap(attitude, 'lm(rating ~ .)')

## ----full_help----------------------------------------------------------------
# help(ale::model_bootstrap)
help(model_bootstrap)

## ----lm_full_stats------------------------------------------------------------
mb_lm$model_stats

## ----lm_full_coefs------------------------------------------------------------
mb_lm$model_coefs

## ----lm_full_ale--------------------------------------------------------------
mb_lm$ale_data[setdiff(names(mb_lm$ale_data), '.common_data')] |> 
  purrr::map(\(.x) .x$plot) |>  # extract plots as a list
  gridExtra::grid.arrange(grobs = _, ncol = 2)

## ----gam_summary--------------------------------------------------------------
gam_attitude <- mgcv::gam(rating ~ s(complaints) + s(privileges) + s(learning) +
                            s(raises) + s(critical) + s(advance),
                data = attitude)
summary(gam_attitude)

## ----gam_simple---------------------------------------------------------------
ale_gam_attitude_simple <- ale(attitude, gam_attitude, boot_it = 0)

ale_gam_attitude_simple[setdiff(names(ale_gam_attitude_simple), '.common_data')] |> 
  purrr::map(\(.x) .x$plot) |>  # extract plots as a list
  gridExtra::grid.arrange(grobs = _, ncol = 2)

## ----gam_full_stats-----------------------------------------------------------
mb_gam <- model_bootstrap(
  attitude, 
  'mgcv::gam(rating ~ s(complaints) + s(privileges) + s(learning) +
                      s(raises) + s(critical) + s(advance))'
  )
mb_gam$model_stats

## ----gam_full_coefs-----------------------------------------------------------
mb_gam$model_coefs

## ----gam_full_ale-------------------------------------------------------------
mb_gam$ale_data[setdiff(names(mb_gam$ale_data), '.common_data')] |> 
  purrr::map(\(.x) .x$plot) |>  # extract plots as a list
  gridExtra::grid.arrange(grobs = _, ncol = 2)

