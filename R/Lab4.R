# student <- setRefClass("student",
#                        fields = list(name="character",age="numeric",GPA="numeric"),
#                        methods = list(
#                          inc_age = function(x){
#                            age <<- age + x
#                          }
#                        ))
#
# stu <- student(name = "John",age=12,GPA=11)
#
# stu$name <- "Ugurcan"
# stu$inc_age(5)
#
#
# print(stu)
#


require(ggplot2)
data <- iris
formula <- Sepal.Length ~  Sepal.Width + Petal.Length + Petal.Width


#' @title Ordinary Linear Algebra Calculations
#' @name  linreg
#' @param formula formula
#' @param data numeric
#' @return RC including some linear algebra calculations
#' @description The function takes formula and data, and calculates linear algebra terms.

linreg <- function(formula, data){

  mx <- model.matrix(formula, data=data)
  y <- all.vars(formula)[1]

  ta <- t(mx)

  # Regressions coefficients
  betas <- solve((ta %*% mx)) %*% ta %*% data[,y[1]==names(data)]

  # The fitted values
  y_hat <- mx %*% betas

  # The residuals
  e_hat <- data[,y[1]==names(data)] - y_hat


  # The degrees of freedom
  n <- nrow(mx)
  p <- ncol(mx)
  df <- n - p

  # The residual variance
  rvariance <- t(e_hat) %*% e_hat / df

  # The variance of the regression coefficients:
  right_side <- 1/(t(mx)%*%mx)
  var_betas <- rvariance[1,1] * right_side

  # The t-values for each coefficient
  tB <- betas / sqrt(diag(var_betas))

  linregRC <- setRefClass("linreg",
                              fields = list(Coefficients = "numeric", Fits = "numeric",
                                            Residual = "numeric", degreesFreedom = "numeric",
                                            rvariance = "numeric",
                                            var_betas="numeric",tB="numeric"),
                              methods = list())
  result <- linregRC(Coefficients = c(betas), Fits = c(y_hat), Residual = c(e_hat),
                     degreesFreedom=c(df),rvariance=c(rvariance),
                     var_betas=c(var_betas),tB=c(tB))
}

test <- linreg(formula, data)


QR_decompostion <- function(formula, data)
{
  y <- data$Sepal.Length
  x <- data$Sepal.Width

  # Matrix Intercept
  X <- cbind(1,x)

  # Regressions coefficients of model matrix By QR
  Beta_QR <- qr(X)

  # Estimated Coefficient
  b <- qr.qty(Beta_QR, y)
  beta <- as.vector(backsolve(Beta_QR$qr, b))

  # Computeing Residuals
  res <- as.vector(y - X %*% beta)

  # Residual Standard Error
  se2 <- sum(res ^ 2) / (nrow(X) - Beta_QR$rank)

  ## Full variance-covariance matrix By QR
  Reg_coff <- chol2inv(Beta_QR$qr) * se2

  QR_res <- setRefClass("QR_decompostion",
                          fields = list(Beta_QR = "list",
                                        Reg_coff="numeric"),
                          methods = list())

  result <- QR_res(Beta_QR = c(Beta_QR),
                   Reg_coff=c(Reg_coff))
}
print(QR_decompostion(formula, data))
