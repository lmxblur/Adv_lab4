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
                                 all.vars(formula)[-1],
                                 ", data = ", dname,")", "\n\n", sep="")
                             cat("Coefficients:","\n")
                             cat(format(labels(b_hat[,1]), width=10, justify="right"), "\n")
                             cat(format(round(b_hat,2), width=17, justify="right"))
                             
                           },
                           plot = function(){titlfi <- paste0('linreg(',all.vars(formula)[1], " ~ ", 
                                                              all.vars(formula)[-1],")")
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
                             as.vector(e_hat)
                           },
                           
                           pred = function(){
                             as.vector(y_hat)
                           },
                           
                           coef = function(){
                             c <- as.vector(b_hat)
                             names(c) <- colnames(X)
                             return(c)
                           },
                           
                           summary = function(){
                             #The degrees of freedom:
                             df <- dim(X)[1]-length(b_hat)
                             
                             #The residual variance:
                             s2_hat <- as.numeric((t(e_hat)%*%e_hat)/df)
                             
                             #The variance of the regression coefficients:
                             var_hat <- as.vector(s2_hat)*solve(t(X)%*%X)
                             
                             #The t-values for each coefficient:
                             tv <- b_hat/sqrt(diag(var_hat))
                             
                             #p-values for each regression coefficient.
                             pv <- 2 * pt(tv, df, lower.tail = FALSE)
                             
                             cat("Coefficients:","\n")
                             
                             cmat <- cbind(round(as.numeric(b_hat),5), round(sqrt(diag(var_hat)),5))
                             cmat <- cbind(cmat,round(tv,2))
                             cmat <- cbind(cmat, pv)
                             colnames(cmat) <- c("Estimate", "Std.Err", "t value", "Pr(>|t|)")
                             rownames(cmat)<- colnames(X)
                             printCoefmat(cmat)
                             
                             cat("\n", "Residuals standard error: ", round(sqrt(s2_hat),4),
                                 " on ", df, " degrees of freedom.")
                           }
                         )
                         
)
