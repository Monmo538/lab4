<!-- badges: start -->
  [![R-CMD-check](https://github.com/Monmo538/lab4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Monmo538/lab4/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
# lab4


## About the Package
This package has implemented a class based solution for linear regression. It uses a ordinary linear algebra and calculate:

* Regressions coefficients
* The fitted values
* The residuals
* The degrees of freedom
* The residual variance
* The variance of the regression coefficients
* The t-values for each coefficient
* The p-values for each regression coefficient
* The standard error of the estimate
* The residual standard error


### Setup of Package 
```{r setup}
library(lab4)
linreg_mod <- linreg$new(Petal.Length ~ Species, data=iris)
```

### Print function
Function to print the formula and intercept for the linear regression model
```{r}
 linreg_mod$print()
```

### resid function 
Function to compute residuals, which calculate the prediction errors for identified model
```{r}

head(linreg_mod$resid())
length(linreg_mod$resid())
```
### pred function 
Function to return the fitted value which is the predicted value of Y.
```{r}
head(linreg_mod$pred())
length(linreg_mod$pred())
```

### coef function 
Function to return the coefficient as a name vector. 
```{r}
linreg_mod$coef()
```

### plot function 

Function to demonstrates plots by using ggplot2. This package includes 2 plots, the first one is Residuals vs Fitted Values, and the second plot demonstrates the root of standardized residuals vs Fitted Value   
```{r}
linreg_mod$plot()
```
### summary function 
Function to produce result summaries of the results of various model fitting functions, print the coefficients with their standard error, t-value and p-value as well as the estimate of ˆσ and the degrees of freedom in the model
```{r}
linreg_mod$summary()
```

