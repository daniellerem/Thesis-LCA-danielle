

##########################################
#        #Part 3: Results                #
##########################################



resultsvariants=list(NA)
resultsvariants2=list(NA)
SimProp.classes= list(NA)
imp = list(NA)
imp2 = list(NA)
bias = list(NA)
method = list(NA)
prop.classes = list(NA)


load('ImpSim.RData')
ImpVariants[[8]][[5]][[5]] #for 8variants, 5 simulations, and 5 imputations
for (variant in 1:8) {
  ImpSim <- ImpVariants[[variant]] 
for (sim in 1:nsim) {
#A. Overall group sizes
trueclass <- prop.table(table(df1$trueclass)) #proportions original data

## i. bias
implist = ImpSim[[sim]]

#Overall group sizes
#calculate bias between group sizes of original data and the imputations of the bootstrap data
for(m in 1:5){ #bias 
  imp[[m]] <- prop.table(table(implist[[m]]$imp))
  imp2[[m]] <- prop.table(table(implist[[m]]$imp2))
  bias[[m]] <- trueclass-imp[[m]]
  method[[m]] <- imp[[m]]-imp2[[m]]
}

#First pool within simulation the 5 imputations
Pooled.prop.classes <- rowMeans(sapply(imp, unlist))
Pooled.prop.classes-trueclass
Pooled.bias <- rowMeans(sapply(bias, unlist))

#pooled mean group sizes per simulation iteration
SimProp.classes[[sim]] <- Pooled.prop.classes 
Average <- rowMeans(sapply(SimProp.classes, unlist))
Average-trueclass

} #end loop over simulations
  resultsvariants[[variant]] <- SimProp.classes
  resultsvariants2[[variant]] <- Average #per variant
  
} #end loop over variants
end_time = Sys.time()
end_time-start_time #elapsed time for script

bias #bias between group sizes of original data and the imputations of the bootstrap data
#pool bias
method #to see whether there is a difference between the two methods to impute the classes in step 4 (conclusion: there is no big difference)
#end loop over nsim

SimProp.classes