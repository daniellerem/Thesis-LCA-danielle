#-------------------------------5. Results-----------#
load("imputations_RR.RData")
df1 = list(NA)
implist = list(NA)
prop.classes = list(NA)
bias = list(NA)
SimProp.classes = list(NA)
SimBias = list(NA)

for (sim in 1:nsim) { 
  df1 <- SimData[[sim]] 
  implist <- ImpSim[[sim]] 
#Overall group sizes
trueclass <- prop.table(table(df1$trueclass)) 

#calculate bias between group sizes of original data and the imputations of the bootstrap data
for(m in 1:5){ #bias 
  prop.classes[[m]] <- prop.table(table(implist[[m]]$imp))
  bias[[m]] <- trueclass-prop.classes[[m]]
   }
#First pool within simulation the 5 imputations
Pooled.prop.classes <- rowMeans(sapply(prop.classes, unlist))
Pooled.prop.classes-trueclass
Pooled.bias <- rowMeans(sapply(bias, unlist))

#pooled mean group sizes per simulation iteration
SimProp.classes[[sim]] <- Pooled.prop.classes 
Average <- rowMeans(sapply(SimProp.classes, unlist))
Average-trueclass

} #end loop over simulations
SimProp.classes
save.image("Results_RR.RData")