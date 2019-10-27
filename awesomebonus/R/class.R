#' Ridge Regression Class.
#'
#' \code{ridgereg()} calculates a number of statistics (see section \code{"Fields"}) by default given both arguments: formula, data and lambda. 
#' It also stores diverse functions implemented using RC object oriented programming. Find more information regarding these functions on section \code{"Methods"}.
#' 
#'
#' @field formula an object of class "formula".
#' @field data a data frame.
#' @field regression_coefficients the regression coefficients of the model calculated using the QR decomposition.
#' @field fitted_values the fitted values.
#' @field lambda a integer.
#' @field arguments a vector containing the function call.
#' 
#' @examples
#' data("flights")
#' example <- ridgereg$new(formula = air_time~dep_delay + arr_delay, data = flights, lambda = 10**6)
#' 
#' @references Reference Classes: \url{http://adv-r.had.co.nz/R5.html}
#' 
#' 
#' Vignettes: \url{http://r-pkgs.had.co.nz/vignettes.html}
#' 
#' @importFrom methods new
#' @importFrom ggplot2 ggplot
#' @importFrom cowplot plot_grid
#'
#' @export ridgereg
#' @exportClass ridgereg
#'

ridgereg <- setRefClass ("ridgereg",
  fields = c (
    formula = "formula",
    data = "data.frame",
    lambda = "ANY",
    regression_coefficients = "ANY", 
    fitted_values = "ANY",
    arguments = "ANY"),
  methods = c (
    initialize = function(formula, data, lambda){
      "Initialize function calculates all values needed from formula, data and lambda."
      ## 0. Check that the class of the formula argument is correct:
      stopifnot(class(formula) == "formula",
                is.data.frame(data),
                length(lambda) == 1)

      
      # Extract formula
      term <- terms.formula(x=formula, data = data, keep.order = TRUE, simplify = TRUE)
      form <- deparse(term)
      formula_call <- paste0(form, collapse = "")
      
      # Save arguments
      arguments <<- c(formula_call, lambda)
      
      # 1. Normalizating the covariates in the dataset.
      df <- as.data.frame(na.omit(data)) # removing NA's
      #name_indep <- all.vars(formula)[-1]
      name_indep <- labels(term)
      vect <- vector()
      
      
      
      # if(name_indep == "."){
      #   name_indep <- labels(term)
      # }
      
      for(j in 1:length(name_indep)){
        mean_j <- mean(df[[name_indep[j]]])
        sd_j <- sd(df[[name_indep[j]]])
        
        vect <- (df[[name_indep[j]]] - mean_j) / sd_j
        df[[name_indep[j]]] <- vect
      }
       
      ## 2. Calculations using least squares. 
      x <- model.matrix(formula, df) # X matrix (independent variables)
      name_dep <- all.vars(formula)[1] # Dependent variable/s name/s
      y <- df[, name_dep] # y (dependent variable/s)
      I <- diag(ncol(x))
      
      ## 3. Estimations
      # Regression coefficients:
      xtx_lambda <- t(x) %*% x + lambda*I
      regression_coefficients <<- as.vector(solve(xtx_lambda) %*% t(x) %*% y)
      names(regression_coefficients) <<- row.names(solve(xtx_lambda) %*% t(x) %*% y)
      
      # The fitted values:
      fitted_values <<- x %*% regression_coefficients
      
      
      return(NULL)
    },
    show = function() {
      "Modifies the print() function for class."
      base::print("Use ...$print()")
    },
    print = function() {
      "Same as show, but allows for ridgereg$print(). Will break print(ridgereg) that show allows."
      
      model <- list()
      model$Call <- paste("ridgereg(formula = ", arguments[1], ", data = ", arguments[2], ", lambda = ", arguments[2], ")", sep="")
      model$Coefficients <- regression_coefficients
      base::print(model)
    },
    predict = function(newdata){
      "Calculate predictions based on fitted values and new x data."
      predicted_y <- rowSums(regression_coefficients[1] + newdata * regression_coefficients[-1])
      
      return(predicted_y)
    },
    coef = function(){
      "Returns the coefficients."
      return(regression_coefficients[-1])
    }
  )
)
