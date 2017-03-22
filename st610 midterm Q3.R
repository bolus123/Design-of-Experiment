source(file = 'C:/Users/ggg/Desktop/st610/midterm/code/st610 midterm head.R')

##############################################################################

wb = loadWorkbook("C:/Users/ggg/Desktop/st610/midterm/alldata.xlsx")

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

data <- list(
	RunHH1,
	RunHL1,
	RunLH1,
	RunLL1,
    RunHH2,
    RunHL2,
    RunLH2,
    RunLL2
)

##############################################################################
    #do bootstrap
##############################################################################

bootstrap <- function(data, n = 10, max.iter = 10){

	m <- length(data)

	p <- dim(data[[1]])[2] - 1
	S <- m * n
	
	
	samp.mean <- rep(NA, max.iter)
	samp.var <- rep(NA, max.iter)
	
	sigma2.e.hat.manly <- rep(NA, max.iter)
	eta.hat.manly <- rep(NA, max.iter)
	theta.hat.manly <- rep(NA, max.iter)
	beta.hat.manly <- matrix(NA, ncol = p, nrow = max.iter)
	
	sigma2.e.hat.boxcox <- rep(NA, max.iter)
	eta.hat.boxcox <- rep(NA, max.iter)
	lambda.hat.boxcox <- rep(NA, max.iter)
	beta.hat.boxcox <- matrix(NA, ncol = p, nrow = max.iter)
	
	
	n.sub <- rep(NA, m)
	
	for (k in 1:m){
		
		n.sub[k] <- dim(data[[k]])[1]
		
	}
	
	
	for (iter in 1:max.iter){
	
		data.samp <- as.data.frame(matrix(NA, ncol = p + 1, nrow = S))
	
		pointer <- 0
	
		for (k in 1:m){
		
			selected <- sample(1:n.sub[k], size = n, replace = TRUE)
			data.samp[(pointer + 1):(pointer + n), ] <- data[[k]][selected, ]
			pointer <- pointer + n
		
		}
		
		subplot <- data.samp[, 2]
		
		names(data.samp) <- c('Y', 'Bar', 'RakeAngle', 'CuttingSpeed', 'Interaction')
		
		X <- data.samp[, -c(1, 2)]
		Y <- data.samp[, 1]
		
		
		samp.mean[iter] <- mean(Y)
		samp.var[iter] <- var(Y)
		
	
		est.manly <- optim(c(0.06032473, -1.90828079), LR.func.manly, control=list(fnscale=-1), 
				gr=NULL, lower=c(0, -Inf), Y = Y, X = X, subplot = subplot)
					
		ests.manly <- estimates.manly(est.manly$par, Y, X, subplot)
		
		est.boxcox <- optim(c(0.08163416, -0.44716495), LR.func.boxcox, control=list(fnscale=-1), 
				gr=NULL, lower=c(0, -Inf), Y = Y, X = X, subplot = subplot)
				
		ests.boxcox <- estimates.boxcox(est.boxcox$par, Y, X, subplot)
		
		sigma2.e.hat.manly[iter] <- ests.manly$sigma2.e.hat
		eta.hat.manly[iter] <- est.manly$par[1]
		theta.hat.manly[iter] <- est.manly$par[2]
		beta.hat.manly[iter, ] <- ests.manly$beta.hat
		
		sigma2.e.hat.boxcox[iter] <- ests.boxcox$sigma2.e.hat
		eta.hat.boxcox[iter] <- est.boxcox$par[1]
		lambda.hat.boxcox[iter] <- est.boxcox$par[2]
		beta.hat.boxcox[iter, ] <- ests.boxcox$beta.hat

		res <- list(
				samp.mean = samp.mean,
				samp.var = samp.var,
				
				sigma2.e.hat.manly = sigma2.e.hat.manly, 
				eta.hat.manly = eta.hat.manly,
				theta.hat.manly = theta.hat.manly,
				beta.hat.manly = beta.hat.manly,
				
				sigma2.e.hat.boxcox = sigma2.e.hat.boxcox, 
				eta.hat.boxcox = eta.hat.boxcox,
				lambda.hat.boxcox = lambda.hat.boxcox,
				beta.hat.boxcox = beta.hat.boxcox
				
		)
		
		save(res, file = 'C:/Users/ggg/Desktop/st610/midterm/est.dist.Rdata')
	
	}

	
		
	return(res)
	
}


max.iter <- 5000

est.dist <- bootstrap(data, max.iter = max.iter)

##############################################################################

samp.mean <- est.dist$samp.mean
samp.var <- est.dist$samp.var
samp.sd <- sqrt(samp.var)

sigma2.e.hat.manly <- est.dist$sigma2.e.hat.manly 
eta.hat.manly <- est.dist$eta.hat.manly
theta.hat.manly <- est.dist$theta.hat.manly
beta.hat.manly <- est.dist$beta.hat.manly

sigma2.e.hat.boxcox <- est.dist$sigma2.e.hat.boxcox
eta.hat.boxcox <- est.dist$eta.hat.boxcox
lambda.hat.boxcox <- est.dist$lambda.hat.boxcox
beta.hat.boxcox <- est.dist$beta.hat.boxcox

##############################################################################
    #get quantiles and boxplots
##############################################################################

qs <- c(0.025, 0.25, 0.5, 0.75, 0.975)

samp.mean.quantile <- quantile(samp.mean, qs)
samp.sd.quantile <- quantile(samp.sd, qs)

sigma2.e.hat.manly.quantile <- quantile(sigma2.e.hat.manly, qs)
eta.hat.manly.quantile <- quantile(eta.hat.manly, qs)
theta.hat.manly.quantile <- quantile(theta.hat.manly, qs)
beta0.hat.manly.quantile <- quantile(beta.hat.manly[, 1], qs)
beta1.hat.manly.quantile <- quantile(beta.hat.manly[, 2], qs)
beta2.hat.manly.quantile <- quantile(beta.hat.manly[, 3], qs)
beta3.hat.manly.quantile <- quantile(beta.hat.manly[, 4], qs)

sigma2.e.hat.boxcox.quantile <- quantile(sigma2.e.hat.boxcox, qs)
eta.hat.boxcox.quantile <- quantile(eta.hat.boxcox, qs)
lambda.hat.boxcox.quantile <- quantile(lambda.hat.boxcox, qs)
beta0.hat.boxcox.quantile <- quantile(beta.hat.boxcox[, 1], qs)
beta1.hat.boxcox.quantile <- quantile(beta.hat.boxcox[, 2], qs)
beta2.hat.boxcox.quantile <- quantile(beta.hat.boxcox[, 3], qs)
beta3.hat.boxcox.quantile <- quantile(beta.hat.boxcox[, 4], qs)

par(mfrow = c(1, 2))
boxplot(samp.mean, main = 'Sample Mean')
boxplot(samp.sd, main = 'Sample SD')

par(mfrow = c(1, 3))
boxplot(sigma2.e.hat.manly, main = 'Sigma2.e From Manly')
boxplot(eta.hat.manly, main = 'Eta From Manly')
boxplot(theta.hat.manly, main = 'Theta From Manly')

par(mfrow = c(1, 4))
boxplot(beta.hat.manly[, 1], main = 'Int From Manly')
boxplot(beta.hat.manly[, 2], main = 'RA From Manly')
boxplot(beta.hat.manly[, 3], main = 'CS From Manly')
boxplot(beta.hat.manly[, 4], main = 'RA*CS From Manly')

par(mfrow = c(1, 3))
boxplot(sigma2.e.hat.boxcox, main = 'Sigma2.e From Box-Cox')
boxplot(eta.hat.boxcox, main = 'Eta From Box-Cox')
boxplot(lambda.hat.boxcox, main = 'Lambda From Box-Cox')

par(mfrow = c(1, 4))
boxplot(beta.hat.boxcox[, 1], main = 'Int From Box-Cox')
boxplot(beta.hat.boxcox[, 2], main = 'RA From Box-Cox')
boxplot(beta.hat.boxcox[, 3], main = 'CS From Box-Cox')
boxplot(beta.hat.boxcox[, 4], main = 'RA*CS From Box-Cox')



##############################################################################

##############################################################################

##############################################################################

##############################################################################

##############################################################################

##############################################################################

##############################################################################
