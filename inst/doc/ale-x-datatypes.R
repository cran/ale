## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ale)
library(dplyr)

## ----print var_cars-----------------------------------------------------------
print(var_cars)

## ----var_cars summary---------------------------------------------------------
summary(var_cars)

## ----cars_gam-----------------------------------------------------------------
cm <- mgcv::gam(mpg ~ cyl + disp + hp + drat + wt + s(qsec) +
                  vs + am + gear + carb + country,
                data = var_cars)
summary(cm)

## ----cars_ale, fig.width=7, fig.height=14-------------------------------------
cars_ale <- ale(var_cars, cm)

# Print all plots
gridExtra::grid.arrange(grobs = cars_ale$plots, ncol = 2)

## ----cars_ale_ixn, fig.width=7, fig.height=7----------------------------------
cars_ale_ixn <- ale_ixn(var_cars, cm)

# Print plots
cars_ale_ixn$plots |>
  purrr::walk(\(.x1) {  # extract list of x1 ALE outputs
    gridExtra::grid.arrange(grobs = .x1, ncol = 2)  # plot all x1 plots
  })

## ----cars_full, fig.width=7, fig.height=14------------------------------------
mb <- model_bootstrap(
  var_cars, 
  'mgcv::gam(mpg ~ cyl + disp + hp + drat + wt + s(qsec) +
               vs + am + gear + carb + country)',
  boot_it = 10,  # 100 by default but reduced here for a faster demonstration
  silent = TRUE  # progress bars disabled for the vignette
)

gridExtra::grid.arrange(grobs = mb$ale$plots, ncol = 2)

