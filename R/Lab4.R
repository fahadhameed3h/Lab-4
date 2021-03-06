#' require(ggplot2)
#' data <- iris
#' formula <- Petal.Length ~ Species
#' 
#' #' @title Ordinary Linear Algebra Calculations
#' #' @name  linreg
#' #' @param formula formula
#' #' @param data numeric
#' #' @return RC including some linear algebra calculations
#' #' @description The function takes formula and data, and calculates linear algebra terms.
#' 
#' linreg <- function(formula, data){
#' 
#'   mx <- model.matrix(formula, data=data)
#'   y <- all.vars(formula)[1]
#' 
#'   ta <- t(mx)
#' 
#'   # Regressions coefficients
#'   betas <- solve((ta %*% mx)) %*% ta %*% data[,y[1]==names(data)]
#' 
#'   # The fitted values
#'   y_hat <- mx %*% betas
#' 
#'   # The residuals
#'   e_hat <- data[,y[1]==names(data)] - y_hat
#' 
#'   # The degrees of freedom
#'   n <- nrow(mx)
#'   p <- ncol(mx)
#'   df <- n - p
#' 
#'   # The residual variance
#'   rvariance <- t(e_hat) %*% e_hat / df
#' 
#'   # The variance of the regression coefficients:
#'   right_side <- 1/(t(mx)%*%mx)
#'   var_betas <- rvariance[1,1] * right_side
#' 
#'   # The t-values for each coefficient
#'   tB <- betas / sqrt(diag(var_betas))
#' 
#'   linregRC <- setRefClass("linreg",
#'                               fields = list(Coefficients = "numeric", Fits = "numeric",
#'                                             Residual = "numeric", degreesFreedom = "numeric",
#'                                             rvariance = "numeric",
#'                                             var_betas="numeric",tB="numeric"),
#' 
#'                               methods = list(
#'                                 plot = function(result){
#' 
#'                                 }
#'                               ))
#'   result <- linregRC(Coefficients = c(betas), Fits = c(y_hat), Residual = c(e_hat),degreesFreedom=c(df),rvariance=c(rvariance),
#' 
#'                      var_betas=c(var_betas),tB=c(tB))
#' }
#' test <- linreg(formula, data)
#' 
#' QR_decompostion <- function(formula, data)
#' {
#'   y <- data$Sepal.Length
#'   x <- data$Sepal.Width
#'   
#'   # Matrix Intercept
#'   X <- cbind(1,x)
#'   
#'   # Regressions coefficients of model matrix By QR
#'   Beta_QR <- qr(X)
#'   
#'   # Estimated Coefficient
#'   b <- qr.qty(Beta_QR, y)
#'   beta <- as.vector(backsolve(Beta_QR$qr, b))
#'   
#'   # Computeing Residuals
#'   res <- as.vector(y - X %*% beta)
#'   
#'   # Residual Standard Error
#'   se2 <- sum(res ^ 2) / (nrow(X) - Beta_QR$rank)
#'   
#'   ## Full variance-covariance matrix By QR
#'   Reg_coff <- chol2inv(Beta_QR$qr) * se2
#'   
#'   QR_res <- setRefClass("QR_decompostion",
#'                         fields = list(Beta_QR = "list",
#'                                       Reg_coff="numeric"),
#'                         methods = list())
#'   
#'   result <- QR_res(Beta_QR = c(Beta_QR),
#'                    Reg_coff=c(Reg_coff))
#' }
#' 
#' data(iris)
#' plot(lm(Petal.Length ~ Species, data=iris))
#' 
#' print()
#' pred()
#' resid()
#' coef()
#' summary(data=iris)
#' # print <- function()
#' # {
#' #   linreg_mod <- lm(Petal.Length ~ Species, data=iris)
#' #   return(linreg_mod)
#' # }
#' # pred <- function()
#' # {
#' #  linreg_mod <- lm(formula = Petal.Length~Species, data=iris)
#' #  return(predict(linreg_mod))
#' # }
#' # 
#' # resid <- function()
#' # {
#' #   linreg_mod <- lm(Petal.Length~Species, data = iris)
#' #   return(residuals(linreg_mod))
#' # }
#' # 
#' # coef <- function()
#' # {
#' #   mod_object <- lm(Petal.Length~Species, data = iris)
#' #   return(coefficients(mod_object))
#' #}
#' #print()
#' # pred()
#' # resid()
#' # coef()
