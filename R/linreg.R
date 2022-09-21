linreg <- setRefClass("linreg",
                      fields = list(
                        formula = "character",
                        data = "character",
                        coefficients = "matrix",
                        fittedvalues = "matrix",
                        residuals = "matrix",
                        df = "numeric",
                        residualvariance = "numeric",
                        var_regcoef_matrix = "matrix",
                        t_values = "matrix",
                        p_value = "matrix"
                      ))
linreg$methods(initialize = function(formula, data){
  .self$formula <- deparse(formula) # converts formula to character
  .self$data <- deparse(substitute(data)) # deparse gives the names of the data set
  X <- model.matrix(formula, data)
  y <- all.vars(formula)[1]
  Y <- data[,y]
  n <- nrow(data)
  p <- ncol(data)
  
  .self$coefficients <- solve(t(X) %*% X) %*% t(X) %*% Y
  .self$fittedvalues <- X %*% coefficients
  .self$residuals <- Y - fittedvalues
  .self$df <- n - p
  .self$residualvariance <- as.numeric((t(residuals) %*% residuals) / df)
  
  variance_matrix <- residualvariance * solve(t(X) %*% X)
  
  #Getting the diagonal elements, then convert them to a matrix
  .self$var_regcoef_matrix <- matrix(diag(variance_matrix), nrow(coefficients))
  rownames(var_regcoef_matrix) <<- rownames(variance_matrix)
  
  .self$t_values <- coefficients / sqrt(var_regcoef_matrix)
  .self$p_value <- pt(coefficients, df)
},
show = function(){
  cat("call:\n")
  cat("linreg(formula =", formula, ", data = ", data, ")\n\n")
  cat(rownames(coefficients), "\n")
  cat(as.vector(coefficients), "\n")
},
resid = function(){
  return(as.vector(residuals))
},
pred = function(){
  return(fittedvalues)
},
coef = function(){
  coef <- as.vector(coefficients)
  names(coef) <- rownames(coefficients)
  return(coef)
},
summary = function(){
  
}
)

data("iris")
test <- linreg(Petal.Length~Species, iris)
print(test)
test$resid()
test$pred()
test$coef()
  