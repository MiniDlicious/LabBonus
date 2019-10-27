#' Function for generating caret custom model.
#'
#' \code{ridgereg_model} generates a custom model for use with the caret package.
#'  
#'
#' @return  \code{ridgereg_model} returns a custom model object for the caret package.
#'
#' @examples
#' rr <- ridgereg_model()
#' 
#' @references \url{https://topepo.github.io/caret/using-your-own-model-in-train.html}
#'
#' @export
#'

ridgereg_model <- function(){
  rr <- list(type = "Regression",
             library = "awesomebonus",
             loop = NULL)
  
  prm <- data.frame(parameter = "lambda",
                    class = "numeric",
                    label = "Lambda")
  
  rr$parameters <- prm
  
  rrGrid <- function(x, y, len = NULL, search = "grid") {
    if(search == "grid") {
      out <- expand.grid(lambda = 1*10**(c(1:10)))
    } else {
      stop('random search not yet implemented')
    }
    return(out)
  }
  
  rr$grid <- rrGrid
  
  
  rrFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    library(awesomebonus)
    dat <- if(is.data.frame(x)) x else as.data.frame(x)
    dat$.outcome <- y
    ridgereg(.outcome ~ ., data = dat, lambda = param$lambda, ...)
  }
  
  rr$fit <- rrFit
  
  rr$levels <- function(x) x@levels
  
  rrPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    modelFit$predict(newdata)
  }
  
  rr$predict <- rrPred
  
  rrProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    modelFit$predict(newdata, type = "prob")
    warning("Function not applicable for ridgereg!")
  }
  
  rr$prob <- rrProb
  
  rrSort <- function(x) x[order(x$lambda),]
  
  rr$sort <- rrSort
  
  return(rr)
}
