# Adv_lab4
[![Build Status](https://travis-ci.org/lmxblur/Adv_lab4.svg?branch=master)](https://travis-ci.org/lmxblur/Adv_lab4)


## The 4th assignment of the course Advanced Programming in R.  
**Creating a Reference Class for the linear regrassion algorithm. 
A collaborative work of Group 19.**
* Dimitriadis Spyridon - spydi472
* Mengxin Liu - menli358


This package performs the linear regression with two argument: formula and data. formula is the formula object indicating the dependent and independent variables.  

## Installation

```{r eval=FALSE}
# Install the library:
devtools::install_github("lmxblur/Adv_lab4", subdir="lab4")
```


## Usage

```{r eval=FALSE}

# Import a dataset
data(iris)

# Create an object
lr_model <- linreg$new(formula = Petal.Length~Species, data = iris)
```

## Methods

### print(): Print out the coefficient and  name of the coefficient of the linear regression.

```{r eval=FALSE}
lr_model$print()
```

### plot(): Return two plots in a list. First the Residuals vs Fitted Values and second the Square of Standartized residuals vs Fitted values. .

```{r eval=FALSE}
lr_model$plot()
```

### resid(): Return the vector of residual.

```{r eval=FALSE}
lr_model$resid()
```

### pred(): Return the predicted values.

```{r eval=FALSE}
lr_model$pred()
```

### coef(): Return the coefficients as a named vector.

```{r eval=FALSE}
lr_model$coef()
```

### summary(): Returns the coefficients with their standard error, t-value and p-value as well as the estimate of residual variance and the degrees of freedom of the model.

```{r eval=FALSE}
lr_model$summary()
```

