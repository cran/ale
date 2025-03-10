## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
 

## ----load libraries-----------------------------------------------------------
library(ale)
library(dplyr)

## ----diamonds_print-----------------------------------------------------------
# Clean up some invalid entries
diamonds <- ggplot2::diamonds |> 
  filter(!(x == 0 | y == 0 | z == 0)) |> 
  # https://lorentzen.ch/index.php/2021/04/16/a-curious-fact-on-the-diamonds-dataset/
  distinct(
    price, carat, cut, color, clarity,
    .keep_all = TRUE
  ) |> 
  rename(
    x_length = x,
    y_width = y,
    z_depth = z,
    depth_pct = depth
  )

summary(diamonds)

## ----diamonds_str-------------------------------------------------------------
str(diamonds)

## ----diamonds_price-----------------------------------------------------------
summary(diamonds$price)

## ----train_gam----------------------------------------------------------------
# Create a GAM model with flexible curves to predict diamond prices.
# (In testing, mgcv::gam actually performed better than nnet.)
# Smooth all numeric variables and include all other variables.
gam_diamonds <- mgcv::gam(
  price ~ s(carat) + s(depth_pct) + s(table) + s(x_length) + s(y_width) + s(z_depth) +
    cut + color + clarity,
  data = diamonds
  )
summary(gam_diamonds)

## ----enable progressr, eval = FALSE-------------------------------------------
#  # Run this in an R console; it will not work directly within an R Markdown or Quarto block
#  progressr::handlers(global = TRUE)
#  progressr::handlers('cli')

## ----ale_simple---------------------------------------------------------------
# Simple ALE without bootstrapping
ale_gam_diamonds <- ale(
  diamonds, gam_diamonds,
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

## ----print-carat, fig.width=3.5, fig.width=4----------------------------------
# Print a plot by entering its reference
ale_gam_diamonds$plots$carat

## ----print-ale_simple, fig.width=7, fig.height=11-----------------------------
# Print all plots
patchwork::wrap_plots(ale_gam_diamonds$plots, ncol = 2)

## ----diamonds_new-------------------------------------------------------------
# Bootstraping is rather slow, so create a smaller subset of new data for demonstration
set.seed(0)
new_rows <- sample(nrow(diamonds), 200, replace = FALSE)
diamonds_small_test <- diamonds[new_rows, ]

## ----ale_boot, fig.width=7, fig.height=11-------------------------------------

ale_gam_diamonds_boot <- ale(
  diamonds_small_test, gam_diamonds, 
  # Normally boot_it should be set to 100, but just 10 here for a faster demonstration
  boot_it = 10,
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

# Bootstrapping produces confidence intervals
patchwork::wrap_plots(ale_gam_diamonds_boot$plots, ncol = 2)

## ----ale_ixn------------------------------------------------------------------
# ALE two-way interactions
ale_ixn_gam_diamonds <- ale_ixn(
  diamonds, gam_diamonds,
  parallel = 2  # CRAN limit (delete this line on your own computer)
)


## ----print-all-ale_ixn, fig.width=7, fig.height=7-----------------------------
# Print all interaction plots
ale_ixn_gam_diamonds$plots |>
  # extract list of x1 ALE outputs
  purrr::walk(\(.x1) {
    # plot all x2 plots in each .x1 element
    patchwork::wrap_plots(.x1, ncol = 2) |>
      print()
  })


## ----print-specific-ixn, fig.width=5, fig.height=3----------------------------
ale_ixn_gam_diamonds$plots$carat$depth

