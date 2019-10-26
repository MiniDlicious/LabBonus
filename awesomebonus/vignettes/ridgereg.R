## ----include=FALSE-------------------------------------------------------
library(awesomebonus)
library(caret)
library(mlbench)

## ------------------------------------------------------------------------
data("BostonHousing")

set.seed(107)
inTrain <- createDataPartition(
  y = BostonHousing$medv,
  ## the outcome data are needed
  p = .8,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

training <- BostonHousing[ inTrain,] 
testing  <- BostonHousing[-inTrain,]

## ------------------------------------------------------------------------
linregFit <- train(medv ~ .,
                   data= training,
                   method = 'leapForward', # to fit linear regression with forward selection
                   preProc = c("center", "scale") # center and scale the predictors
                   )
linregFit

## ------------------------------------------------------------------------
pred <- predict(linregFit, testing)
postResample(pred = pred, obs = testing$medv)

## ------------------------------------------------------------------------
rr <- list(type = "Regression",
              library = "awesomebonus",
              loop = NULL)

prm <- data.frame(parameter = "lambda",
                  class = "numeric",
                  label = "Lambda")

rr$parameters <- prm

rrGrid <- function(x, y, len = NULL, search = grid) {
  if(search == grid) {
    out <- expand.grid(lambda = c(0.01, 100))
  } else {
    stop('random search not yet implemented')
  }
  return(out)
}

rr$grid <- rrGrid

# Probably needs to use formula??
rrFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  library(awesomebonus)
  print(head(data))
  ridgereg(y ~ x, data = as.data.frame(data), lambda = param$lambda, ...)
}

rr$fit <- rrFit

rr$levels <- function(x) x@levels

rrPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata)
}

rr$predict <- rrPred

rrProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata, type = "prob")
}

rr$prob <- rrProb

rrSort <- function(x) x[order(x$lambda),]

rr$sort <- rrSort

fitControl <- trainControl(method = "repeatedcv",
                           number = 1,
                           repeats = 1)

rGrid <- expand.grid(lambda = c(0.1, 0.3))

set.seed(825)
# rrTune <- train(medv ~ .,
#                 data = training,
#                   method = rr,
#                   trControl = fitControl,
#                   tuneGrid = rGrid)
# 
# linregFit_ridgereg <- train(medv ~ ., 
#                             data = training, 
#                             method = "ridgereg", 
#                             preProc = c("center", "scale")
#                             )
# 


# Setting "formula", "data" and "lambda" arguments
#ridge <- ridgereg$new(formula=air_time~dep_delay+arr_delay, data=flights, lambda=2)

## ------------------------------------------------------------------------
# fitControl <- trainControl(method = "none",
#                            number = 10,
#                            repeats = 3,
#                            summaryFunction = defaultSummary,
#                            search = "random")
# 
# set.seed(825)
# rda_fit <- train(Class ~ ., data = training, 
#                   method = "rda",
#                   metric = "ROC",
#                   tuneLength = 30,
#                   trControl = fitControl)
# rda_fit

