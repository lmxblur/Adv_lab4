#' A reference class to perform linear regression and return parameters and plots
#' 
#' @import ggplot2
#' @import methods
#' @field formula Object dependent and indepedent variables
#' @field data The data in data.frame format 
#' @field dname Data name
#' @field X Independent variable
#' @field y Dependent variable
#' @field b_hat Regression coefficient
#' @field y_hat Fitted values
#' @field e_hat Residuals
linreg <- setRefClass("linreg", 
                      
                         fields =list(formula="formula", 
                                      data="data.frame",
                                      dname="vector",
                                      X="matrix",
                                      y="matrix",
                                      b_hat="matrix",
                                      y_hat="matrix",
                                      e_hat="matrix"),
                      
                         methods = list(
                           initialize = function(formula, data) {
                             'Initialize the values'
                             formula <<- formula
                             data <<- data
                             dname <<- deparse(substitute(data))
                             
                             X <<- model.matrix(formula,data)
                             y <<- as.matrix(data[all.vars(formula,data)[1]])
                             
                             #Regressions coefficients:
                             b_hat <<- solve(t(X)%*%X) %*% t(X) %*% y
                             names(b_hat) <<- colnames(X)
                             
                             #The fitted values:
                             y_hat <<- X%*%b_hat
                             
                             #The residuals:
                             e_hat <<- y-y_hat

                             
                           },
                           
                           print = function(){
                             'Print the coeff that needed'
                             cat("Call:", "\n")
                             cat("linreg(formula = ", all.vars(formula)[1], " ~ ", 
                                 paste(all.vars(formula)[-1], collapse = " + "),
                                 ", data = ", dname,")", "\n\n", sep="")
                             cat("Coefficients:","\n")
                             cat(format(labels(b_hat[,1]), width=10, justify="right"), "\n")
                             cat(format(round(b_hat,2), width=17, justify="right"))
                             
                           },
                           plot = function(){
                             'Plot the linear regression'
                             titlfi <- paste0('linreg(',all.vars(formula)[1], " ~ ", 
                                              paste(all.vars(formula)[-1], collapse = " + "),")")
                           titl <- paste('Fitted values',titlfi,sep = '\n')
                           std <- sd(e_hat)
                           yax_2 <- sqrt(abs(e_hat/std))
                           ph <-'e'
                           
                           p1<-ggplot2::qplot(x = y_hat,y = e_hat,main='Residual vs Fitted'
                                              ,ylab = 'Rediduals',xlab =titl)+ggplot2::geom_smooth(method = "lm")
                           p2 <- ggplot2::qplot(x = y_hat,y = yax_2,main='Residual vs Fitted'
                                                ,ylab = expression(sqrt(abs(paste(Standardized,phantom(ph),residuals))
                                                )),xlab =titl)+ggplot2::geom_smooth(method = "lm")
                           return(list(p1,p2))},
                           
                           resid = function(){
                             'Return the vector of residuals'
                             as.vector(e_hat)
                           },
                           
                           pred = function(){
                             'Return the predicted values'
                             as.vector(y_hat)
                           },
                           
                           coef = function(){
                             'Return the coefficient'
                             c <- as.vector(b_hat)
                             names(c) <- colnames(X)
                             return(c)
                           },
                           
                           summary = function(){
                             'Summary of the linear regression'
                             #The degrees of freedom:
                             df <- dim(X)[1]-length(b_hat)
                             
                             #The residual variance:
                             s2_hat <- as.numeric((t(e_hat)%*%e_hat)/df)
                             
                             #The variance of the regression coefficients:
                             var_hat <- as.vector(s2_hat)*solve(t(X)%*%X)
                             
                             #The t-values for each coefficient:
                             tv <- b_hat/sqrt(diag(var_hat))
                             
                             #p-values for each regression coefficient.
                             pv <- 2 * pt(abs(tv), df, lower.tail = FALSE)
                             
                             cat("Coefficients:","\n")
                             
                             cmat <- cbind(round(as.numeric(b_hat),5), round(sqrt(diag(var_hat)),5))
                             cmat <- cbind(cmat,round(tv,2))
                             cmat <- cbind(cmat, pv)
                             colnames(cmat) <- c("Estimate", "Std.Err", "t value", "Pr(>|t|)")
                             rownames(cmat)<- colnames(X)
                             printCoefmat(cmat)
                             cat("\n")
                             cat("Residual standard error:", round(sqrt(s2_hat),4),
                                 "on", df, "degrees of freedom.")
                           }
                         )
                         
)
