install.packages("nycflights13")
library(nycflights13)
library(MASS)

# Example to compare results with:
data(flights)
lm.ridge(formula=air_time~dep_delay+arr_delay, data=flights)

# 1. Normalizating the covariates in the dataset.
data <- as.data.frame(na.omit(data)) # removing NA's
name_indep <- all.vars(formula)[-1]
vect <- vector()

for(j in 1:length(name_indep)){
  mean_j <- mean(data[[name_indep[j]]])
  sd_j <- sd(data[[name_indep[j]]])
  
  vect <- (data[[name_indep[j]]] - mean_j) / sd_j
  data[[name_indep[j]]] <- vect
}

## 2. Calculations using least squares. 
x <- model.matrix(formula, data) # X matrix (independent variables)
name_dep <- all.vars(formula)[1] # Dependent variable/s name/s
Y <- data[, name_dep] # y (dependent variable/s)
I <- matrix(0, ncol=ncol(x), nrow=nrow(x))
diag(I) <- 1

# Regression coefficients:
xtx <- t(x) %*% x
xtx_lambda <- xtx + (lambda*I)
beta_hat <- solve(xtx) %*% t(x) %*% y

# The fitted values:
y_hat <- x %*% beta_hat


lambda=2

XXl = crossprod(X) + diag(nrow(X)*lambda,ncol(X),ncol(X))
QR = qr(XXl)
XY = crossprod(X,Y)
Ahat = qr.coef(QR,XY) # predicted activities
Yhat = X %*% Ahat # predicted expression values
resid = Y-Yhat
Chi2 = colSums(resid^2)   
Yvar = Chi2/nrow(X) # sigma_sample; un-explained variance


