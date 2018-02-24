rm(list=ls())
library(foreign)
library(R2jags)

## INPUT
params <- c("b", "inb")
inits1 <- list("b[,1]"=c(0.01, 4.8, 1.2), "b[,2]"=c(0.65, 2.6, 0.43))
inits2 <- list("b[,1]"=c(0.02, 5, 1.25), "b[,2]"=c(0.70, 2.0, 0.42))
inits <- list(inits1, inits2)


# Model: specify the file with the model 
mfile <- "BFL.txt"
#read the data

dta.df<-read.dta("data.dta") #the data file


N <- dim(dta.df)[1] #the number of rows
out <- dta.df$qaly #first outcome
cos <- dta.df$cost #second outcome 
rnd <- dta.df$arm #randomised arm, or instrument more generally 
trt <- dta.df$exp #treatment received, or endogenous explanatory variable to be instrumented
y<-cbind(trt,out,cos) #make a multivariable vector comprised of the bivariate outcome and the endogenous variable
data <- list("N", "rnd", "y")







###########################################################################
###  Initial run to start chains and find out possible autocorrelation  ###
###########################################################################
jini <- jags(data=data, inits=NULL, params, DIC=FALSE, n.chains=2, 
             n.iter=10000, n.burnin=1000, n.thin=2, model.file=mfile)
# Check autocorrelation
jmcmc <- as.mcmc(jini)
autocorr.diag(jmcmc)
autocorr.plot(jmcmc, ask=FALSE)

## INPUT: Set n.thin according to previous observations (we recommend thin=1)
thin <- 1

iter <- thin * 5000
# Initialise counters
jupd <- jini
jupd <- update(jupd, n.iter=iter, n.thin=thin)
jmcmc <- as.mcmc(jupd)
# Check convergence using the Geweke and Heidelberger-Welch statistics 
gwk <- geweke.diag(jmcmc)
gwk
hew <- heidel.diag(jmcmc)
hew
densityplot(jmcmc)

#######################################################################################
###  Update further  so that MC error reduced to about 1% of parameter SE           ###
#######################################################################################


iter <- 5000 * thin
jfin <- update(jupd, n.iter=iter, n.thin=thin)
jmcmc <- as.mcmc(jfin)

#######################################################################################
# saving the Summary statistics
trt.effect.q<-t(summary(jmcmc)$statistics)[,"b[2,2]"]
dq<-trt.effect.q[1]
se.q<-trt.effect.q[2]
#mode.q <- mlv(unlist(c(jmcmc[1][,"b[2,2]"], jmcmc[2][,"b[2,2]"])), method = "hsm")

trt.effect.c<-t(summary(jmcmc)$statistics)[,"b[3,2]"]
dc<-trt.effect.c[1]
se.c<-trt.effect.c[2]
#mode.c <- mlv(unlist(c(jmcmc[1][,"b[3,2]"], jmcmc[2][,"b[3,2]"])), method = "hsm")


#inb 
nb.stats<-t(summary(jmcmc)$statistics)[,"inb"]
mean.nb<-nb.stats[1]
se.nb<-nb.stats[2]
#mode.nb <- mlv(unlist(c(jmcmc[1][,"inb"], jmcmc[2][,"inb"])), method = "hsm")

#credible intervals
quant.q<-t(summary(jmcmc)$quantiles)[,"b[2,2]"]
lb.dq<-quant.q[1]
med.dq<-quant.q[3]
ub.dq<-quant.q[5]

quant.c<-t(summary(jmcmc)$quantiles)[,"b[3,2]"]
lb.dc<-quant.c[1]
med.dc<-quant.c[3]
ub.dc<-quant.c[5]

quant.nb<-t(summary(jmcmc)$quantiles)[,"inb"]
lb.inb<-quant.nb[1]
med.inb<-quant.nb[3]
ub.inb<-quant.nb[5]




#######################################################################################
####################################  END  ############################################
