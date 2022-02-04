#Results
library(ggplot2)

#load data
original_classsizes = c(0.15, 0.34, 0.2975, 0.2125)
load('ImpSim2.RData')
variantA <- ImpVariants[[1]]
variantC <- ImpVariants[[5]]

#store results
imp = list(NA)
imp2 = list(NA)
bias = list(NA)
method = list(NA)


#1. OVERALL GROUPSIZES (bias, coverage CI, SE en ME)
#2. Relationship w/ covariate (bias, coverage CI, SE en ME)
#3. Relationship w/ true variable (bias, coverage CI, SE en ME)


#START
for (sim in 1:nsim) {
  implist <-  variantC[[sim]]
  
#A. Overall group sizes

## i. bias
for(m in 1:5){ #bias 
  imp[[m]] <- prop.table(table(implist[[m]]$imp))
  bias[[m]] <- abs(original_classsizes-imp[[m]])
}
bias 


## ii. SE
st.er <- function(x) sd(x)/sqrt(length(x))
se.p <- function(prop) sqrt((prop*(1-prop))/5000)
se.p(original_classsizes)
#average standard error/Standard deviation over all replications  

## iii. coverage CI (prop times population value falls within 95% CI around estimate over all replications)
CI <-  confint(trueclass) #werkt alleen op lm objects
FunConfInt <- function(prop){
  interv <- function(prop) 1.96*sqrt((prop*(1-prop))/5000)
  lower <- prop-interv(prop)
  upper <- prop+interv(prop) 
  Confid <- cbind(lower, prop, upper)
  return(Confid)
}
FunConfInt(original_classsizes)
Capture <-  ifelse((CI[1]>0|CI[3]<0), 0, 1)

#pool$lower   <- pool$qbar - qt(.975, pool$df) * sqrt(pool$t)
#pool$upper   <- pool$qbar + qt(.975, pool$df) * sqrt(pool$t)
#pool$coverage <- pool$lower <= mean(truth1) & mean(truth1) <= pool$upper
cover= list(NA) 
alpha=0.05
#LB <- quantile(Var, probs=c(alpha/2))
#UB <- quantile(Var, probs=c(1 - (alpha/2)))
cover[k] <- ifelse(LB <= true_var & UB >= true_var, 1, 0)
#make use of ifelse() statement for coverage of CI
ppp_mean <- mean(ifelse((mean_rep>mean_obs) ,1 , 0)) #posterior predictive p-value, the ppp



## ii. SE
st.er <- function(x) sd(x)/sqrt(length(x))

#
stdev2=array(NA)
for (c in 1:4) {stdev3[,c]=sqrt((Ps[c]*(1-Ps[c]))/5000)  }
stdev2
se.P=cbind(LCAS2[[5]]$P.se,LCAS2[[5]]$P)
Ps=se.P[,2]
Ps[1]
#average standard error/Standard deviation over all replications  

## iv. ME


#accuracy = Correct classifications/Total cases

#create confusion matrix? we are interested in population totals, not in classifications right?

#B. Relationship with covariate


} #end loop over simulations

#display results




#visualization bias
#par(mfrow=c(2,3))
#for(m in 1:5){plot(bias[[m]],main = paste("Bias of bootstrap", m), ylab="Bias", xlab= "Classes", ylim = c(-.15, 0.15), type="p", pch=4, abline(h=0), add=T)}
#lines(var_D_tree, ylab="Bias", col=("green"), type="p",pch=4, add=T) #add tree-MILC results for comparison
#legend(x="bottomright", title='Method',legend=c("MILC", "tree-MILC"), col=c("black", "green"), pch=4,box.lty=0, cex=0.6)


