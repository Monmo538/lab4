#' @title A Reference Class to represent a linear model.
#'
#' @field formula Presents description of model to be fitted.
#' @field data Shows observations including dependent (Y) and independent variables (X).
#' @field coefficients The estimates for the model parameters to describe the relationship between X and Y.
#' @field fittedvalues The predicted value of Y.
#' @field residuals The difference between observed value of the Y and the value of Y variable predicted from the regression line.
#' @field df Difference between number of observations and parameters.
#' @field residualvariance The variance of the values that are calculated by finding the distance between regression line and the actual points.
#' @field var_regcoef_matrix The variance of the parameters that are calculated as coefficients.
#' @field t_values A matrix with the t-values of coefficient.
#' @field p_value The probability of finding the given t-statistic if the null hypothesis of no relationship between X and Y were true.
#'
#' @return An object of class " linreg "
#' @export linreg
#' @exportClass linreg
#' @import ggplot2

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
linreg$methods(
  initialize = function(formula, data){
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
    .self$residualvariance <- as.numeric((t(residuals) %*% residuals) / df) #as.numeric

    variance_matrix <- residualvariance * solve(t(X) %*% X)

    #Getting the diagonal elements, then convert them to a matrix
    .self$var_regcoef_matrix <- matrix(diag(variance_matrix), nrow(coefficients))
    rownames(var_regcoef_matrix) <<- rownames(variance_matrix)

    .self$t_values <- coefficients / sqrt(var_regcoef_matrix)
    .self$p_value <- pt(coefficients, df)
  },
  print = function(){
    cat(paste("linreg(formula = ", format(.self$formula), ", data = ", .self$data , ")\n\n ", sep = ""))

    cat(paste(rownames(coefficients), sep = ""), "\n")
    cat(as.vector(coefficients))
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
  plot = function(){
    plot1 <- ggplot2::ggplot(mapping = ggplot2::aes(x = fittedvalues, y = residuals)) +
      ggplot2::geom_point(shape = 1, size = 3) +
      ggplot2::stat_summary(fun = "median", color = "red", geom = "line") +
      ggplot2::labs(title = "Residuals vs Fitted")

    standardized_residuals <- abs(residuals / sqrt(residualvariance))
    plot2 <- ggplot2::ggplot(mapping = ggplot2::aes(x = fittedvalues, y = sqrt(standardized_residuals))) +
      ggplot2::geom_point(shape = 1, size = 3) +
      ggplot2::stat_summary(fun = "mean", color = "red", geom = "line") +
      ggplot2::labs(title = "Sqrt Standardized residuals vs Fitted")

    plots <- list(plot1, plot2)
    plots
  },
  summary = function(){
    cat(paste("(Intercept)", as.vector(coefficients)[1], sep = "\t"), "\n")
    cat(paste("(Intercept)", as.vector(coefficients)[1], sep = "\t"), "\n")
    se <- sqrt(abs(.self$residualvariance)) / sqrt(nrow(.self$data))

  }
)

linreg_mod <- linreg$new(Petal.Length ~ Species, data=iris)
linreg_mod$print()
linreg_mod$summary()


data(iris)
mod_object <- lm(Petal.Length~Species, data = iris)
summary(mod_object)
