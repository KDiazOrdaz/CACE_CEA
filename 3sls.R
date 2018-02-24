rm(list=ls())
library(MASS,"AER",systemfit,mice,foreign)
data <- read.dta("../data.dta")

#the data must be ordered as follows total_qalys  total_costs   txreceived eq5d_b

#MI using MICE in R 
M<-50  #50 imputations
data<-data[order(data$rnd),]       # order data by arm

#we impute separate in each randomised arm
data0<-subset(data,rnd==0)
data1<-subset(data,rnd==1)
#the variable rnd must be dropped from data1 and data0

#rnd=1
imp1 <-mice(data1, m = M, method =c("pmm","pmm", "","pmm"),print=FALSE)
#rnd=0
imp0 <-mice(data0, m = M, method =c("pmm","pmm", "","pmm"),print=FALSE)

##merging data
mj<-imput_no<- rep(1:M, each=length(data$rnd))
data_mi <-data[rep(1:nrow(data), M), ]
data_mi$mj<-imput_no

data_mi$baseq_imput<-data_mi$qaly_imput<-<-array(NA, dim=M*length(data$rnd))
data_mi$cost_imput<-array(NA, dim=M*length(data$rnd))

for (j in 1:M){
  data_mi$baseq_imput[data_mi$mj==j]<-c(complete(imp0,j)$baseq , complete(imp1,j)$baseq)
  data_mi$qaly_imput[data_mi$mj==j]<-c(complete(imp0,j)$qaly  , complete(imp1,j)$qaly )
  data_mi$cost_imput[data_mi$mj==j]<-c(complete(imp0,j)$cost , complete(imp1,j)$cost)
}
######################################################
####  3sls 
######################################################
coef.c<-var.coef.c<-matrix(NA, nrow=M, ncol=1)
coef.q<-var.coef.q<-matrix(NA, nrow=M, ncol=1)
coef.inb<-var.coef.inb<-matrix(NA, nrow=M, ncol=1)
coef.icer.3sls<-matrix(NA, nrow=M, ncol=1)


for (j in 1:M){  
  eq.cost <-  data_mi$cost_imput[data_mi$mj==j] ~ 
    data_mi$exp[data_mi$mj==j]+ data_mi$baseq_imput[data_mi$mj==j]
  eq.qaly <-  data_mi$qaly_imput[data_mi$mj==j] ~
    data_mi$exp[data_mi$mj==j]+ data_mi$baseq_imput[data_mi$mj==j]
  eq.Exp.Rand <- data_mi$exp[data_mi$mj==j] ~ 
    data_mi$rnd[data_mi$mj==j]+ data_mi$baseq_imput[data_mi$mj==j]
  
  System3eq <- list( cost=eq.cost, qaly=eq.qaly, trt.received = eq.Exp.Rand )
  reg3 <- systemfit(System3eq, method = "3SLS",
                    inst = ~ data_mi$rnd[data_mi$mj==j] + data_mi$baseq_imput[data_mi$mj==j])
  
  coef.q[j]<-reg3$coefficients["qaly_data_mi$exp[data_mi$mj == j]"]
  coef.c[j]<-reg3$coefficients["cost_data_mi$exp[data_mi$mj == j]"]  
  var.coef.q[j]<-diag(reg3$coefCov)["qaly_data_mi$exp[data_mi$mj == j]"]
  var.coef.c[j]<-diag(reg3$coefCov)["cost_data_mi$exp[data_mi$mj == j]"]
  cov<-reg3$coefCov["qaly_data_mi$exp[data_mi$mj == j]","cost_data_mi$exp[data_mi$mj == j]"]    
  coef.inb[j]<- coef.q[j]*30000-coef.c[j]
  var.coef.inb[j]<-30000^2*(var.coef.q[j])+  var.coef.c[j] -(2*30000*cov)  
  coef.icer.3sls[j]<-(coef.c[j])/(coef.q[j])  
}
###### Apply Rubin's rules ####
mean.c <-mean(coef.c)
var.c <-mean(var.coef.c)
with.var.c<- var.c   # within-imputation variance
bet.var.c<-(1/(M-1))*sum((coef.c-mean.c)^2)   
se.c<- sqrt(with.var.c+(1+1/M)*bet.var.c)

##Q
mean.q <-mean(coef.q)
var.q <-mean(var.coef.q)
with.var.q<- var.q   # within-imputation variance
bet.var.q<-(1/(M-1))*sum((coef.q-mean.q)^2)   
se.q<- sqrt(with.var.q+(1+1/M)*bet.var.q)

##INB
mean.inb <-mean(coef.inb)
var.inb <-mean(var.coef.inb)
with.var.inb<- var.inb   # within-imputation variance
bet.var.inb<-(1/(M-1))*sum((coef.inb-mean.inb)^2)   
se.inb<- sqrt(with.var.inb+(1+1/M)*bet.var.inb)

##ONLY Mean ICER
mean.icer <-mean(coef.icer.3sls)

res.3sls<-c(mean.c, se.c, mean.q, se.q, mean.inb, se.inb, mean.icer)