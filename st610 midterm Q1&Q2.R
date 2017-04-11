source(file = 'C:/Users/ggg/Desktop/st610/midterm/code/st610 midterm head.R')

##############################################################################

wb = loadWorkbook("C:/Users/ggg/Desktop/st610/midterm/250data.xlsx")

RunHH1 = readWorksheet(wb, sheet = "RunHH1", header = FALSE)
RunHL1 = readWorksheet(wb, sheet = "RunHL1", header = FALSE)
RunLH1 = readWorksheet(wb, sheet = "RunLH1", header = FALSE)
RunLL1 = readWorksheet(wb, sheet = "RunLL1", header = FALSE)

RunHH2 = readWorksheet(wb, sheet = "RunHH2", header = FALSE)
RunHL2 = readWorksheet(wb, sheet = "RunHL2", header = FALSE)
RunLH2 = readWorksheet(wb, sheet = "RunLH2", header = FALSE)
RunLL2 = readWorksheet(wb, sheet = "RunLL2", header = FALSE)

RunHH1 <- cbind(RunHH1, 1, 1, 1, 1)
RunHL1 <- cbind(RunHL1, 2, 1, -1, -1)
RunLH1 <- cbind(RunLH1, 3, -1, 1, -1)
RunLL1 <- cbind(RunLL1, 4, -1, -1, 1)
                      
RunHH2 <- cbind(RunHH2, 5, 1, 1, 1)
RunHL2 <- cbind(RunHL2, 6, 1, -1, -1)
RunLH2 <- cbind(RunLH2, 7, -1, 1, -1)
RunLL2 <- cbind(RunLL2, 8, -1, -1, 1)

names(RunHH1) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')
names(RunHL1) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')
names(RunLH1) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')
names(RunLL1) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')
names(RunHH2) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')
names(RunHL2) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')
names(RunLH2) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')
names(RunLL2) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')

data <- rbind(
			RunHH1,
            RunHL1,
            RunLH1,
            RunLL1,
            RunHH2,
            RunHL2,
            RunLH2,
            RunLL2
)

subplot <- data[, 2]
X <- data[, -c(1, 2)]
Y <- data[, 1]

##############################################################################
    #get eta and theta based on manly
##############################################################################

est.manly <- optim(c(1, -0.5), LR.func.manly, control=list(fnscale=-1), 
				gr=NULL, lower=c(0, -Inf), Y = Y, X = X, subplot = subplot)

#save(est.manly, file = 'C:/Users/ggg/Desktop/st610/midterm/est.manly.Rdata')
				
##############################################################################
    #get beta, sigma2e and sigma2b based on manly 
##############################################################################

ests.manly <- estimates.manly(est.manly$par, Y, X, subplot)

#save(ests.manly, file = 'C:/Users/ggg/Desktop/st610/midterm/ests.manly.Rdata')

##############################################################################
    #calculate the mean model
##############################################################################

RunHH1.mean <- cbind(mean(RunHH1[, 1]), 1, 1, 1, 1)
RunHL1.mean <- cbind(mean(RunHL1[, 1]), 2, 1, -1, -1)
RunLH1.mean <- cbind(mean(RunLH1[, 1]), 3, -1, 1, -1)
RunLL1.mean <- cbind(mean(RunLL1[, 1]), 4, -1, -1, 1)
                                
RunHH2.mean <- cbind(mean(RunHH2[, 1]), 5, 1, 1, 1)
RunHL2.mean <- cbind(mean(RunHL2[, 1]), 6, 1, -1, -1)
RunLH2.mean <- cbind(mean(RunLH2[, 1]), 7, -1, 1, -1)
RunLL2.mean <- cbind(mean(RunLL2[, 1]), 8, -1, -1, 1)

data.mean <- rbind(
			RunHH1.mean,
            RunHL1.mean,
            RunLH1.mean,
            RunLL1.mean,
            RunHH2.mean,
            RunHL2.mean,
            RunLH2.mean,
            RunLL2.mean
)

data.mean <- as.data.frame(data.mean)

names(data.mean) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')

standard.OLS <- lm(Y ~ RakeAngle + CuttingSpeed + Interaction, data = data.mean)

sum.standard.OLS <- summary(standard.OLS)

##############################################################################
    #get eta and lambda based on boxcox
##############################################################################

est.boxcox <- optim(c(0.5, 0.5), LR.func.boxcox, control=list(fnscale=-1), 
				gr=NULL, lower=c(0, -Inf), Y = Y, X = X, subplot = subplot)
#save(est.boxcox, file = 'C:/Users/ggg/Desktop/st610/midterm/est.boxcox.Rdata')
	
##############################################################################
    #get beta, sigma2e and sigma2b based on boxcox
##############################################################################

ests.boxcox <- estimates.boxcox(est.boxcox$par, Y, X, subplot)

#save(ests.boxcox, file = 'C:/Users/ggg/Desktop/st610/midterm/ests.boxcox.Rdata')


##############################################################################

