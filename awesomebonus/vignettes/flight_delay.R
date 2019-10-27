## ----include=FALSE-------------------------------------------------------
library(dplyr)
library(nycflights13)
library(awesomebonus)

## ------------------------------------------------------------------------
# Load the datasets
flights_data <- nycflights13::flights
airport_data <- nycflights13::airports
weather_data <- nycflights13::weather

# Drop columns not used in prediction
flights_data <- select(flights_data, -c(year, month, day, arr_time, dep_time, sched_dep_time, sched_arr_time, 
                        carrier, flight, tailnum, hour, minute, air_time))
airport_data <- select(airport_data, -c(name, tz, dst, tzone))
weather_data <- select(weather_data, -c(day, wind_gust))

# Join data sets
flights_data <- dplyr::rename(flights_data, faa = dest)
joined_data <- left_join(flights_data, weather_data, by=c("origin", "time_hour"))
joined_data <- left_join(joined_data, airport_data, by="faa")

# Remove additional columns
joined_data <- select(joined_data, -c(faa, origin, time_hour, year))

# Clean data from NA
joined_data <- na.omit(joined_data)

# Add arrival and departure delay
joined_data$total_delay <- joined_data$arr_delay + joined_data$dep_delay


## ------------------------------------------------------------------------
# Use Caret to divide the dataset into test (5%), train (80%) and validation (15%).
library(caret)
set.seed(42)
trainIndex <- createDataPartition(joined_data$total_delay, p = .8, 
                                  list = FALSE)
trainSet <- joined_data[trainIndex, ]
nontrainSet <- joined_data[-trainIndex, ]
validationIndex <- createDataPartition(nontrainSet$total_delay, p = .75, 
                                  list = FALSE, 
                                  times = 1)
validationSet <- joined_data[validationIndex, ]
testSet <- joined_data[-validationIndex, ]


## ------------------------------------------------------------------------
# Use Caret to divide the dataset into test (5%), train (80%) and validation (15%).
library(corrplot)
corrplot.mixed(cor(trainSet), order="hclust", tl.col="black")


## ------------------------------------------------------------------------
# Make predictions and evaluate
ridge_model <- ridgereg_model()

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 1)

rGrid <- expand.grid(lambda = 1*10**(c(1:10)))

set.seed(825)
rrTune <- train(form = dep_delay ~ .,
                data = trainSet,
                method = ridge_model,
                trControl = fitControl, 
                tuneGrid = rGrid)


## ----echo = FALSE--------------------------------------------------------
library(knitr)
kable(rrTune$results, caption = "Evaluation of the Hyper parameters")


## ------------------------------------------------------------------------
my_ridge <- ridgereg$new(dep_delay~., data=trainSet, lambda = 10**9)
X_test <- dplyr::select(testSet, -dep_delay)
ridge_pred <- my_ridge$predict(X_test)
postResample(pred = ridge_pred, obs = testSet$dep_delay)

