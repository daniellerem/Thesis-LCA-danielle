load("LCAS_RR.RData")

set.seed(123)

nboot = 5

# create empty columns to store calculated posteriors
df1[,c("p1","p2","p3","p4","imp")] <- NA 

implist = list()

# for every imputation
for(k in 1:nboot){
  cat(k)
  implist[[k]] = df1
  # for every row
  for(j in 1:nrow(implist[[k]])){
    # and for every LC per row
    for(i in 1:4){
      # we take the probability of that LC
      # and multiply it by the probability of the score of Y1 in the row
      # and multiply it by the probability of the score of Y2 in the row
      # and multiply it by the probability of the score of Y3 in the row
      # and multiply it by the probability of the score of Y4 in the row
      #P(X=1) * P( Y1=1,Y2=1,Y3=1,Y4=1|X=1)
      
      #voor Z1: [j,4] of [j,5]? 
      implist[[k]][j,i+6] = prod(LCAS2[[1]]$P[i],
                                 LCAS2[[1]]$probs$Y1[i,as.numeric(paste(df1[j,1]))],
                                 LCAS2[[1]]$probs$Y2[i,as.numeric(paste(df1[j,2]))],
                                 LCAS2[[1]]$probs$Y3[i,as.numeric(paste(df1[j,3]))],
                                 LCAS2[[1]]$probs$Y4[i,as.numeric(paste(df1[j,4]))],
                                 LCAS2[[1]]$probs$Z1[i,as.numeric(paste(df1[j,4]))]) / 
        # then we divide it by the sum of these products over class 1, 2, 3 and 4
        # sum(PX=1)*P( Y1=1,Y2=1,Y3=1,Y4=1|X=1) + sum(PX=2)*P( Y1=1,Y2=1,Y3=1,Y4=1|X=2) + 
        # sum(PX=3)*P( Y1=1,Y2=1,Y3=1,Y4=1|X=3) + sum(PX=4)*P( Y1=1,Y2=1,Y3=1,Y4=1|X=4)
        sum(prod(LCAS2[[1]]$P[1], 
                 LCAS2[[1]]$probs$Y1[1,as.numeric(paste(df1[j,1]))],
                 LCAS2[[1]]$probs$Y2[1,as.numeric(paste(df1[j,2]))],
                 LCAS2[[1]]$probs$Y3[1,as.numeric(paste(df1[j,3]))],
                 LCAS2[[1]]$probs$Y4[1,as.numeric(paste(df1[j,4]))],
                 LCAS2[[1]]$probs$Z1[1,as.numeric(paste(df1[j,4]))]),
            prod(LCAS2[[1]]$P[2],
                 LCAS2[[1]]$probs$Y1[2,as.numeric(paste(df1[j,1]))],
                 LCAS2[[1]]$probs$Y2[2,as.numeric(paste(df1[j,2]))],
                 LCAS2[[1]]$probs$Y3[2,as.numeric(paste(df1[j,3]))],
                 LCAS2[[1]]$probs$Y4[2,as.numeric(paste(df1[j,4]))],
                 LCAS2[[1]]$probs$Z1[2,as.numeric(paste(df1[j,4]))]),
            prod(LCAS2[[1]]$P[3],
                 LCAS2[[1]]$probs$Y1[3,as.numeric(paste(df1[j,1]))],
                 LCAS2[[1]]$probs$Y2[3,as.numeric(paste(df1[j,2]))],
                 LCAS2[[1]]$probs$Y3[3,as.numeric(paste(df1[j,3]))],
                 LCAS2[[1]]$probs$Y4[3,as.numeric(paste(df1[j,4]))],
                 LCAS2[[1]]$probs$Z1[3,as.numeric(paste(df1[j,4]))]),
            prod(LCAS2[[1]]$P[4],
                 LCAS2[[1]]$probs$Y1[4,as.numeric(paste(df1[j,1]))],
                 LCAS2[[1]]$probs$Y2[4,as.numeric(paste(df1[j,2]))],
                 LCAS2[[1]]$probs$Y3[4,as.numeric(paste(df1[j,3]))],
                 LCAS2[[1]]$probs$Y4[4,as.numeric(paste(df1[j,4]))],
                 LCAS2[[1]]$probs$Z1[4,as.numeric(paste(df1[j,4]))]))
    }
    
    implist[[k]][j,"imp"] = which(rmultinom(1, 1, implist[[k]][j,c("p1","p2","p3","p4")]) == 1)
  }
}

save.image("imputations.RData")