#-------------------------------2. BOOTSTRAP DATA-----------#
load("simulated_dataset_RR.RData") 
dfboot = list(NA) 

for (sim in 1:nsim) {
  
  df1 <- SimData[[sim]] 
  # aggregate data 
  dffreq <- df1[,1:4] %>%  
    count(Y1, Y2, Y3, Y4) 

#create nboot bootstrap samples
nboot = 5
# for each profile, sample nboot times, 
# from the total length of the dataset,
# with a probability equal to the observed frequency of that profile
boots = rmultinom(nboot, 
                  sum(dffreq$n), 
                  dffreq$n/sum(dffreq$n))
dfboot[[sim]]= cbind(dffreq, boots)
} #end bootstrap script
save.image("bootstraps_RR.RData")
