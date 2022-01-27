#Results
library(ggplot2)

#1. OVERALL GROUPSIZES (bias, coverage CI, SE en ME)
#2. Relationship w/ covariate (bias, coverage CI, SE en ME)
#3. Relationship w/ true variable (bias, coverage CI, SE en ME)


imp = list(NA)
imp2 = list(NA)
bias = list(NA)
method = list(NA)

#A. Overall group sizes
trueclass <- prop.table(table(df1$trueclass)) #proportions original data

## i. bias
for(m in 1:5){ #bias 
  imp[[m]] <- prop.table(table(implist[[m]]$imp))
  bias[[m]] <- trueclass-imp[[m]]
}
bias #bias between group sizes of original data and the imputations of the bootstrap data
#pool bias
par(mfrow=c(2,3))
for(m in 1:5){plot(bias[[m]],main = paste("Bias of bootstrap", m), ylab="Bias", xlab= "Classes", ylim = c(-.15, 0.15), type="p", pch=4, abline(h=0), add=T)}
#lines(var_D_tree, ylab="Bias", col=("green"), type="p",pch=4, add=T) #add tree-MILC results for comparison
#legend(x="bottomright", title='Method',legend=c("MILC", "tree-MILC"), col=c("black", "green"), pch=4,box.lty=0, cex=0.6)



## ii. SE
st.er <- function(x) sd(x)/sqrt(length(x))
st.er()
#average standard error/Standard deviation over all replications  

## iii. coverage CI (prop times population value falls within 95% CI around estimate over all replications)
CI <-  confint(trueclass) #werkt alleen op lm objects
FunConfInt <- function(x){
  interv <- function(x) 1.96*sd(x)/sqrt(length(x))
  lower <- mean(x)-interv(x)
  upper <- mean(x)+interv(x) 
  Confid <- cbind(lower, upper)
  return(Confid)
}

Capture <-  ifelse((CI[1]>0|CI[2]<0), 0, 1)



## ii. SE
st.er <- function(x) sd(x)/sqrt(length(x))
#average standard error/Standard deviation over all replications  

## iv. ME


#accuracy = Correct classifications/Total cases

#create confusion matrix? we are interested in population totals, not in classifications right?

#B. Relationship with covariate


#pool$lower   <- pool$qbar - qt(.975, pool$df) * sqrt(pool$t)
#pool$upper   <- pool$qbar + qt(.975, pool$df) * sqrt(pool$t)
#pool$coverage <- pool$lower <= mean(truth1) & mean(truth1) <= pool$upper


