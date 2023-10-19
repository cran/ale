## ----knitr setup, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----general setup------------------------------------------------------------
library(dplyr)

## ----nnet setup, fig.width=10, fig.height=8-----------------------------------
## R code for Example 2
## Load relevant packages
library(ALEPlot)
library(nnet)

## Generate some data and fit a neural network supervised learning model
set.seed(0)  # not in the original, but added for reproducibility
n = 5000
x1 <- runif(n, min = 0, max = 1)
x2 <- runif(n, min = 0, max = 1)
x3 <- runif(n, min = 0, max = 1)
x4 <- runif(n, min = 0, max = 1)
y = 4*x1 + 3.87*x2^2 + 2.97*exp(-5+10*x3)/(1+exp(-5+10*x3))+
13.86*(x1-0.5)*(x2-0.5)+ rnorm(n, 0, 1)
DAT <- data.frame(y, x1, x2, x3, x4)
nnet.DAT <- nnet(y~., data = DAT, linout = T, skip = F, size = 6,
decay = 0.1, maxit = 1000, trace = F)

## ----ALEPlot nnet yhat--------------------------------------------------------
## Define the predictive function
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata,
type = "raw"))


## ----ALEPlot nnet one-way-----------------------------------------------------
## Calculate and plot the ALE main effects of x1, x2, x3, and x4
ALE.1 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = 1, K = 500,
NA.plot = TRUE)
ALE.2 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = 2, K = 500,
NA.plot = TRUE)
ALE.3 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = 3, K = 500,
NA.plot = TRUE)
ALE.4 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = 4, K = 500,
NA.plot = TRUE)

## ----ALEPlot nnet ixn---------------------------------------------------------
## Calculate and plot the ALE second-order effects of {x1, x2} and {x1, x4}
ALE.12 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = c(1,2), K = 100,
NA.plot = TRUE)
ALE.14 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = c(1,4), K = 100,
NA.plot = TRUE)

## ----ALEPlot nnet organized plots, fig.width=7, fig.height=10-----------------
## Manually plot the ALE main effects on the same scale for easier comparison
## of the relative importance of the four predictor variables
par(mfrow = c(3,2))
plot(ALE.1$x.values, ALE.1$f.values, type="l", xlab="x1",
ylab="ALE_main_x1", xlim = c(0,1), ylim = c(-2,2), main = "(a)")
plot(ALE.2$x.values, ALE.2$f.values, type="l", xlab="x2",
ylab="ALE_main_x2", xlim = c(0,1), ylim = c(-2,2), main = "(b)")
plot(ALE.3$x.values, ALE.3$f.values, type="l", xlab="x3",
ylab="ALE_main_x3", xlim = c(0,1), ylim = c(-2,2), main = "(c)")
plot(ALE.4$x.values, ALE.4$f.values, type="l", xlab="x4",
ylab="ALE_main_x4", xlim = c(0,1), ylim = c(-2,2), main = "(d)")
## Manually plot the ALE second-order effects of {x1, x2} and {x1, x4}
image(ALE.12$x.values[[1]], ALE.12$x.values[[2]], ALE.12$f.values, xlab = "x1",
ylab = "x2", main = "(e)")
contour(ALE.12$x.values[[1]], ALE.12$x.values[[2]], ALE.12$f.values, add=TRUE,
drawlabels=TRUE)
image(ALE.14$x.values[[1]], ALE.14$x.values[[2]], ALE.14$f.values, xlab = "x1",
ylab = "x4", main = "(f)")
contour(ALE.14$x.values[[1]], ALE.14$x.values[[2]], ALE.14$f.values, add=TRUE,
drawlabels=TRUE)


## ----ale nnet one-way creation------------------------------------------------
library(ale)

nn_ale <- ale(DAT, nnet.DAT, pred_type = "raw")

## ----ale nnet one-way plots, fig.width=3.5, fig.height=10---------------------
# , fig.asp=3 is OK
# Print plots
gridExtra::grid.arrange(grobs = nn_ale$plots, ncol = 1)

## ----ale nnet one-way zeroed, fig.width=7, fig.height=5-----------------------
# Zero-centred ALE
nn_ale <- ale(DAT, nnet.DAT, pred_type = "raw", relative_y = 'zero')

gridExtra::grid.arrange(grobs = nn_ale$plots, ncol = 2)

## ----ale nnet ixn, fig.width=7, fig.height=10---------------------------------
# Create and plot interactions
nn_ale_ixn <- ale_ixn(DAT, nnet.DAT, pred_type = "raw")

# Print plots
nn_ale_ixn$plots |>
  purrr::walk(\(.x1) {  # extract list of x1 ALE outputs
    gridExtra::grid.arrange(grobs = .x1, ncol = 1)  # plot all x1 plots
  })

## ----gbm data setup-----------------------------------------------------------
## R code for Example 3
## Load relevant packages
library(ALEPlot)
library(gbm)

## Read data and fit a boosted tree supervised learning model
data(census, package = 'ale')  # load ale package version of the data
data <-  
  census |> 
  as.data.frame() |>   # ALEPlot is not compatible with the tibble format
  select(age:native_country, higher_income) |>  # Rearrange columns to match ALEPlot order
  na.omit(data)

## ----gbm model----------------------------------------------------------------
# To generate the code, uncomment the following lines.
# But it is slow, so this vignette loads a pre-created model object.
# set.seed(0)
# gbm.data <- gbm(higher_income ~ ., data= data[,-c(3,4)],
#                 distribution = "bernoulli", n.trees=6000, shrinkage=0.02,
#                 interaction.depth=3)
# saveRDS(gbm.data, file.choose())
gbm.data <- url('https://github.com/Tripartio/ale/raw/main/download/gbm.data_model.rds') |> 
  readRDS()

gbm.data

## ----ALEPlot gbm, fig.width=7, fig.height=5-----------------------------------
## Define the predictive function; note the additional arguments for the
## predict function in gbm
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata,
n.trees = 6000, type="link"))

## Calculate and plot the ALE main and interaction effects for x_1, x_3,
## x_11, and {x_1, x_11}
par(mfrow = c(2,2), mar = c(4,4,2,2)+ 0.1)
ALE.1=ALEPlot(data[,-c(3,4,15)], gbm.data, pred.fun=yhat, J=1, K=500,
NA.plot = TRUE)
ALE.3=ALEPlot(data[,-c(3,4,15)], gbm.data, pred.fun=yhat, J=3, K=500,
NA.plot = TRUE)
ALE.11=ALEPlot(data[,-c(3,4,15)], gbm.data, pred.fun=yhat, J=11, K=500,
NA.plot = TRUE)
ALE.1and11=ALEPlot(data[,-c(3,4,15)], gbm.data, pred.fun=yhat, J=c(1,11),
K=50, NA.plot = FALSE)

## ----ale one-way link, fig.width=7, fig.height=20-----------------------------
# Custom predict function that returns log odds
yhat <- function(object, newdata) {
  as.numeric(
    predict(object, newdata,  n.trees = 6000,
            type="link")  # return log odds
  )
}

# Generate ALE data for all variables

# # To generate the code, uncomment the following lines.
# # But it is slow, so this vignette loads a pre-created model object.
# gbm_ale_link <- ale(
#   data[,-c(3,4)], gbm.data,
#   pred_fun = yhat,
#   x_intervals = 500,
#   rug_sample_size = 600,  # technical issue: rug_sample_size must be > x_intervals + 1
#   relative_y = 'zero'  # compatibility with ALEPlot
# )
# saveRDS(gbm_ale_link, file.choose())
gbm_ale_link <- url('https://github.com/Tripartio/ale/raw/main/download/gbm_ale_link.rds') |> 
  readRDS()

# Print plots
gridExtra::grid.arrange(grobs = gbm_ale_link$plots, ncol = 2)

## ----ale ixn link, fig.width=7, fig.height=5----------------------------------
# # To generate the code, uncomment the following lines.
# # But it is slow, so this vignette loads a pre-created model object.
# gbm_ale_ixn_link <- ale_ixn(
#   data[,-c(3,4)], gbm.data,
#   pred_fun = yhat,
#   x_intervals = 500,
#   rug_sample_size = 600,  # technical issue: rug_sample_size must be > x_intervals + 1
#   relative_y = 'zero'  # compatibility with ALEPlot
# )
# saveRDS(gbm_ale_ixn_link, file.choose())
gbm_ale_ixn_link <- url('https://github.com/Tripartio/ale/raw/main/download/gbm_ale_ixn_link.rds') |> 
  readRDS()

# Print plots
gbm_ale_ixn_link$plots |>
  purrr::walk(\(.x1) {  # extract list of x1 ALE outputs
    gridExtra::grid.arrange(grobs = .x1, ncol = 2)  # plot all x1 interaction plots
  })

## ----ale one-way prob, fig.width=7, fig.height=20-----------------------------
# Custom predict function that returns predicted probabilities
yhat <- function(object, newdata) {
  as.numeric(
    predict(object, newdata,  n.trees = 6000,
            type="response")  # return predicted probabilities
  )
}

# Generate ALE data for all variables

# # To generate the code, uncomment the following lines.
# # But it is slow, so this vignette loads a pre-created model object.
# gbm_ale_prob <- ale(
#   data[,-c(3,4)], gbm.data,
#   pred_fun = yhat,
#   x_intervals = 500,
#   rug_sample_size = 600  # technical issue: rug_sample_size must be > x_intervals + 1
# )
# saveRDS(gbm_ale_prob, file.choose())
gbm_ale_prob <- url('https://github.com/Tripartio/ale/raw/main/download/gbm_ale_prob.rds') |> 
  readRDS()

# Print plots
gridExtra::grid.arrange(grobs = gbm_ale_prob$plots, ncol = 2)

## ----ale ixn prob, fig.width=7, fig.height=5----------------------------------
# # To generate the code, uncomment the following lines.
# # But it is slow, so this vignette loads a pre-created model object.
# gbm_ale_ixn_prob <- ale_ixn(
#   data[,-c(3,4)], gbm.data,
#   pred_fun = yhat,
#   x_intervals = 500,
#   rug_sample_size = 600  # technical issue: rug_sample_size must be > x_intervals + 1
# )
# saveRDS(gbm_ale_ixn_prob, file.choose())
gbm_ale_ixn_prob <- url('https://github.com/Tripartio/ale/raw/main/download/gbm_ale_ixn_prob.rds') |> 
  readRDS()

# Print plots
gbm_ale_ixn_prob$plots |>
  purrr::walk(\(.x1) {  # extract list of x1 ALE outputs
    gridExtra::grid.arrange(grobs = .x1, ncol = 2)  # plot all x1 plots
  })

