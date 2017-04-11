##############################################################################
    #This code requires package XLConnect which need supports of JAVA
    #So you may need to install JAVA before running this code
    #RevoUtils and RevoUtilsMath are highly recommended but they are just optional
##############################################################################

library(XLConnect)

##############################################################################

if (sum(c("RevoUtils", "RevoUtilsMath") %in% rownames(installed.packages())) == 2){ #Maximize the computation performance
                                                                                    #based on intel MKL library
    require(RevoUtils)                                                              #In this case, you may need packages, RevoUtils and RevoUtilsMath
    require(RevoUtilsMath)                                                          #but they are just optional libraries
    setMKLthreads(getMKLthreads())

}

##############################################################################

wb = loadWorkbook("C:/Users/bolus/Desktop/st610/logistic/ST610_GLMPractice_Data.xlsx")

APD = readWorksheet(wb, sheet = "Auto Purchase Data", header = TRUE, startRow = 2)
COD = readWorksheet(wb, sheet = "Ceriodaphnia Organiism Data", header = TRUE, startRow = 3)

##############################################################################

logistic.mean <- function(X, b){

    exp(X %*% b.new) / (1 + exp(X %*% b.new))

}

logistic.var <- function(n, mean){

    pi * (1 - pi)


}

poisson.mean <- function(X, b){

    exp(X %*% b.new)

}

poisson.var <- function(X, b){

    exp(X %*% b.new)

}


GLM.model <- function(Y, X, intercept = TRUE, family = c('logistic'), alpha = 0.05, maxiter = 1000, tol = 1e-6){

    Y <- as.matrix(Y)

    if (intercept == TRUE){
    
        X <- as.matrix(cbind(1, X))
        colnames(X) <- c('intercept', colnames(X)[-1])
    
    }
    
    p <- dim(X)[2]
    n <- dim(X)[1]
    
    b.old <- rep(Inf, p)
    b.new <- solve(t(X) %*% X) %*% t(X) %*% Y

    iter <- 0
    while(sum(abs(b.old - b.new)) > tol){
        iter <- iter + 1
        if (iter > maxiter) break
     
        eta <- X %*% b.new
        
        mu <- logistic.mean(X, b.new)

        V <- logistic.var(n, mu)
    
        eta.star <- eta + (Y - mu) / V
        
        V.matrix <- diag(n)
        diag(V.matrix) <- V
        
        b.old <- b.new
        b.new <- solve(t(X) %*% V.matrix %*% X) %*% t(X) %*% V.matrix %*% eta.star
    
    }

    var.b <- diag(solve(t(X) %*% V.matrix %*% X))
    
    wald.chisq <- 1 - pchisq((b.new) ^ 2 / var.b, 1)
    
    b.upper.bound <- qnorm(1 - alpha / 2, mean = b.new, sd = sqrt(var.b))
    b.lower.bound <- qnorm(alpha / 2, mean = b.new, sd = sqrt(var.b))
    
    return(b.new)

}

Y <- APD[, 3]
X <- APD[, 1:2]

GLM.model(Y, X)

glm(APD[,3] ~ APD[, 1] + APD[,2 ], family = binomial)

##############################################################################


##############################################################################


##############################################################################


##############################################################################


##############################################################################


##############################################################################


##############################################################################


##############################################################################


##############################################################################


##############################################################################


##############################################################################
