## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggplot2)
library(ale)

## ----diamonds_help------------------------------------------------------------
help(diamonds)

## ----diamonds_print-----------------------------------------------------------
diamonds

## ----diamonds_str-------------------------------------------------------------
str(diamonds)

## ----diamonds_split-----------------------------------------------------------
# Split the dataset into training and test sets
# https://stackoverflow.com/a/54892459/2449926
set.seed(0)
train_test_split <- sample(c(TRUE, FALSE), nrow(diamonds), replace = TRUE, prob = c(0.8, 0.2))
diamonds_train <- diamonds[train_test_split, ]
diamonds_test <- diamonds[!train_test_split, ]

## ----train_gam----------------------------------------------------------------
# Create a GAM model with flexible curves to predict diamond price
# Smooth all numeric variables and include all other variables
# Build model on training data, not on the full dataset.
gam_diamonds <- mgcv::gam(
  price ~ s(carat) + s(depth) + s(table) + s(x) + s(y) + s(z) + 
    cut + color + clarity,
  data = diamonds_train
  )
summary(gam_diamonds)

## ----ale_help-----------------------------------------------------------------
# help(ale::ale)
help(ale)

## ----ale_simple---------------------------------------------------------------
# Simple ALE without bootstrapping
ale_gam_diamonds <- ale(diamonds_test, gam_diamonds)

ale_gam_diamonds[setdiff(names(ale_gam_diamonds), '.common_data')] |> 
  purrr::map(\(.x) .x$plot) |>  # extract plots as a list
  gridExtra::grid.arrange(grobs = _, ncol = 2)

## ----diamonds_new-------------------------------------------------------------
# Bootstraping is rather slow, so create a smaller subset of new data for demonstration
new_rows <- sample(nrow(diamonds_test), 200, replace = FALSE)
diamonds_new <- diamonds_test[new_rows, ]

## ----ale_boot-----------------------------------------------------------------
ale_gam_diamonds_boot <- ale(diamonds_new, gam_diamonds, boot_it = 100)

# Bootstrapping produces confidence intervals
ale_gam_diamonds_boot[setdiff(names(ale_gam_diamonds_boot), '.common_data')] |> 
  purrr::map(\(.x) .x$plot) |>  # extract plots as a list
  gridExtra::grid.arrange(grobs = _, ncol = 2)


## ----ale_ixn_help-------------------------------------------------------------
help(ale_ixn)
# help(ale::ale_ixn)

## ----ale_ixn------------------------------------------------------------------
# ALE two-way interactions
ale_ixn_gam_diamonds <- ale_ixn(diamonds_test, gam_diamonds)

# Skip .common_data when iterating through the data for plotting
ale_ixn_gam_diamonds[setdiff(names(ale_ixn_gam_diamonds), '.common_data')] |> 
  purrr::walk(\(x1) {  # extract list of x1 ALE outputs
    purrr::map(x1, \(.x) .x$plot) |>  # for each x1, extract list of x2 ALE outputs
      gridExtra::grid.arrange(grobs = _, ncol = 2)  # plot all x1 plots
  })

