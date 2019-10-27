library(awesomebonus)
library(caret)
library(dplyr)
library(mlbench)
data("BostonHousing")

set.seed(107)

dataSet <- dplyr::select(BostonHousing, -c(chas))

inTrain <- createDataPartition(
  y = dataSet$medv,
  ## the outcome data are needed
  p = .8,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

training <- dataSet[ inTrain,] 
testing  <- dataSet[-inTrain,]



# rr <- list(type = "Regression",
#            library = "awesomebonus",
#            loop = NULL)
# 
# prm <- data.frame(parameter = "lambda",
#                   class = "numeric",
#                   label = "Lambda")
# 
# rr$parameters <- prm
# 
# rrGrid <- function(x, y, len = NULL, search = "grid") {
#   if(search == "grid") {
#     out <- expand.grid(lambda = c(1, 100))
#   } else {
#     stop('random search not yet implemented')
#   }
#   return(out)
# }
# 
# rr$grid <- rrGrid
# 
# 
# rrFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#   library(awesomebonus)
#   dat <- if(is.data.frame(x)) x else as.data.frame(x)
#   dat$.outcome <- y
#   ridgereg(.outcome ~ ., data = dat, lambda = param$lambda, ...)
# }
# 
# rr$fit <- rrFit
# 
# rr$levels <- function(x) x@levels
# 
# rrPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
#   modelFit$predict(newdata)
# }
# 
# rr$predict <- rrPred
# 
# rrProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
#   modelFit$predict(newdata, type = "prob")
#   warning("Function not applicable for ridgereg!")
# }
# 
# rr$prob <- rrProb
# 
# rrSort <- function(x) x[order(x$lambda),]
# 
# rr$sort <- rrSort

rr <- ridgereg_model()

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5)

rGrid <- expand.grid(lambda = 1*10**(c(1:10)))

set.seed(825)
rrTune <- train(form = medv ~ .,
                data = training,
                method = rr,
                trControl = fitControl, 
                tuneGrid = rGrid)
print(rrTune$results)


# PLOT
library(ggplot2)
ggplot(rrTune$results) + 
  geom_point(aes(x= log(lambda), y = RMSE, colour="#2c3e50", size=2)) + 
  geom_line(aes(x= log(lambda), y = RMSE, colour="#2980b9", size=1)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks=seq(0,24,2))

my_ridge <- ridgereg$new(medv~., data=training, lambda = 10**6)
X_test <- dplyr::select(testing, -medv)
pred <- my_ridge$predict(X_test)

rmse(pred - testing$medv)

pred2 <- my_ridge$predict(dplyr::select(training, -medv))
rmse(pred2 - training$medv)
