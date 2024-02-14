## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load libraries-----------------------------------------------------------
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

## ----enable progressr, eval = FALSE-------------------------------------------
#  # Run this in an R console; it will not work directly within an R Markdown or Quarto block
#  progressr::handlers(global = TRUE)
#  progressr::handlers('cli')

## ----cars_ale, fig.width=7, fig.height=14-------------------------------------
cars_ale <- ale(
  var_cars, cm,
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

# Print all plots
cars_ale$plots |> 
  patchwork::wrap_plots(ncol = 2)

## ----cars_ale_ixn, fig.width=7, fig.height=7----------------------------------
cars_ale_ixn <- ale_ixn(
  var_cars, cm,
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

# Print plots
cars_ale_ixn$plots |>
  # extract list of x1 ALE outputs
  purrr::walk(\(.x1) {  
    # plot all x2 plots in each .x1 element
    patchwork::wrap_plots(.x1, ncol = 2) |> 
      print()
  })

## ----cars_full, fig.width=7, fig.height=14------------------------------------
mb <- model_bootstrap(
  var_cars, 
  cm,
  boot_it = 10,  # 100 by default but reduced here for a faster demonstration
  parallel = 2,  # CRAN limit (delete this line on your own computer)
  seed = 2  # workaround to avoid random error on such a small dataset
)

mb$ale$plots |> 
  patchwork::wrap_plots(ncol = 2)

