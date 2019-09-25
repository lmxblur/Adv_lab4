linreg <- function(formula,data){
  linreg1 <- setRefClass("linreg", 
                         fields =list(formula = 'formula',
                                      data = 'data.frame'
                         ),
                         methods = list(
                           initialize = function(formula,data){
                             X <<- data[all.vars(formula)[1]]
                             y <<- data[all.vars(formula)[2]]
                             beta <<- solve(t(X)%*%X)%*%t(X)%*%y
                             y_fit <<- X%*%beta
                             e <<- y-X%*%beta
                             df <<- length(X)-length(beta)
                             sigma <<- (t(e)%*%e)/df
                             var <<- sigma%*%solve(t(X)%*%X)
                             t <<- beta/sqrt(var)},
                           print = function(){},
                           plot = function(){},
                           resid = function(){},
                           pred = function(){},
                           coef = function(){},
                           summary = function(){}
                         )
                         
  )
  return(linreg1)
}