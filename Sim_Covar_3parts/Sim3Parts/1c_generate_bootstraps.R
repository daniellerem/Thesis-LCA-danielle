bootdata = function(data, nboot){
  
  boots = rmultinom(nboot, 
                    sum(data$n), 
                    data$n/sum(data$n))
  dfboot= cbind(data, boots)
  
  return(dfboot)
}