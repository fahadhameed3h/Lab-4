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



data <- iris
formula <- Sepal.Length ~  Sepal.Width + Petal.Length + Petal.Width
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


linreg <- setRefClass("linreg",
                       fields = list(),
                       methods = list(
                         
                       ))
