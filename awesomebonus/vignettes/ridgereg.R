## ----include=FALSE-------------------------------------------------------
library(awesomebonus)
library(caret)
library(mlbench)

## ------------------------------------------------------------------------
data("BostonHousing")

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

## ------------------------------------------------------------------------
linregFit <- train(medv ~ .,
                   data= training,
                   method = 'lm', # to fit linear regression with forward selection
                   preProc = c("center", "scale") # center and scale the predictors
                   )
linregFit

## ------------------------------------------------------------------------
linforFit <- train(medv ~ .,
                   data= training,
                   method = 'leapForward', # to fit linear regression with forward selection
                   preProc = c("center", "scale") # center and scale the predictors
                   )
linforFit

## ------------------------------------------------------------------------
ridge_model <- ridgereg_model()

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5)

rGrid <- expand.grid(lambda = 1*10**(c(1:10)))

set.seed(825)
rrTune <- train(form = medv ~ .,
                data = training,
                method = ridge_model,
                trControl = fitControl, 
                tuneGrid = rGrid)



## ----echo = FALSE--------------------------------------------------------
library(knitr)
kable(rrTune$results, caption = "Evaluation of the Hyper parameters")


## ----echo = FALSE--------------------------------------------------------

library(ggplot2)
ggplot(rrTune$results) + 
  geom_point(aes(x= log10(lambda), y = RMSE, colour="#2c3e50", size=2)) + 
  geom_line(aes(x= log10(lambda), y = RMSE, colour="#2980b9", size=1)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks=seq(0,24,2))


## ------------------------------------------------------------------------
pred <- predict(linregFit, testing)
postResample(pred = pred, obs = testing$medv)

## ------------------------------------------------------------------------
forpred <- predict(linforFit, testing)
postResample(pred = forpred, obs = testing$medv)

## ------------------------------------------------------------------------
my_ridge <- ridgereg$new(medv~., data=training, lambda = 10**6)
X_test <- dplyr::select(testing, -medv)
ridge_pred <- my_ridge$predict(X_test)
postResample(pred = ridge_pred, obs = testing$medv)

