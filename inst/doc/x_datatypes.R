## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ale)
library(dplyr)
library(ggplot2)

## ----mtcars, echo=TRUE--------------------------------------------------------
help(mtcars)

## ----mtcars_to_cars-----------------------------------------------------------
# Create a function to determine the country of origin of a car based on its make
car_country <- function(make) {
  american_makes <- c("AMC", "Cadillac", "Camaro", "Chrysler", "Dodge", "Duster", "Ford", "Hornet", "Lincoln", "Pontiac", "Valiant")
  japanese_makes <- c("Datsun", "Honda", "Mazda", "Toyota")
  italian_makes <- c("Ferrari", "Fiat", "Maserati")
  british_makes <- c("Lotus")
  swedish_makes <- c("Volvo")
  german_makes <- c("Merc", "Porsche")
  
  case_when(
    make %in% american_makes ~ 'USA',
    make %in% japanese_makes ~ 'Japan',
    make %in% italian_makes ~ 'Italy',
    make %in% british_makes ~ 'UK',
    make %in% swedish_makes ~ 'Sweden',
    make %in% german_makes ~ 'Germany',
  )
}

cars <- 
  mtcars |> 
  as_tibble(rownames = 'make') |> 
  # retain only first word as the make without the car model
  mutate(
    make = stringr::str_extract(make, "^\\S+") |> factor(),
    country = car_country(make) |> factor()
    ) |> 
  select(-make) |> 
  mutate(across(c(vs, am), as.logical)) |> 
  mutate(gear = as.ordered(gear)) |> 
  mutate(across(c(cyl, carb), as.integer))

cars |> 
  print(n = 50)

## ----cars---------------------------------------------------------------------
summary(cars)

## ----cars_gam-----------------------------------------------------------------
cm <- mgcv::gam(mpg ~ cyl + s(disp) + s(hp) + drat + wt + s(qsec) +
            + vs + am + gear + carb + country,
          data = cars)

summary(cm)

## ----cars_ale-----------------------------------------------------------------
cars_ale <- ale(cars, cm)

# Print all plots
# Skip .common_data when iterating through the data for plotting
cars_ale[setdiff(names(cars_ale), '.common_data')] |> 
purrr::map(\(.x) .x$plot) |>  # extract plots as a list
  gridExtra::grid.arrange(grobs = _, ncol = 2)

## ----cars_ale_ixn-------------------------------------------------------------
cars_ale_ixn <- ale_ixn(cars, cm)

# Skip .common_data when iterating through the data for plotting
cars_ale_ixn[setdiff(names(cars_ale_ixn), '.common_data')] |> 
  purrr::walk(\(x1) {  # extract list of x1 ALE outputs
    purrr::map(x1, \(.x) .x$plot) |>  # for each x1, extract list of x2 ALE outputs
      gridExtra::grid.arrange(grobs = _, ncol = 2)  # plot all x1 plots
  })

## ----cars_full----------------------------------------------------------------
mb <- model_bootstrap(
  cars, 
  'mgcv::gam(mpg ~ cyl + s(disp) + s(hp) + drat + wt + s(qsec) +
            + vs + am + gear + carb + country)'
)

# Skip .common_data when iterating through the data for plotting
mb$ale_data[setdiff(names(mb$ale_data), '.common_data')] |> 
purrr::map(\(.x) .x$plot) |>  # extract plots as a list
  gridExtra::grid.arrange(grobs = _, ncol = 2)

