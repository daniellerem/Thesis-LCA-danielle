#4. calculate Posteriors
#posterior = P(Class|Y)= P(Class=x)P(Y=y|Class=x) / P(Y=y)
#The posterior membership probabilities represent the probability of being in a LC given a specific combination of scores on the indicators and covariates.
#P(scorepattern|Class=1)=P(Q1=A1|X=1) * P(Q2=A2|X=1)* P(Q3=A3|X=1) 
      #necessary to calculate the conditional probabilities P(Yn|Class)
#P(scorepattern)=P(Class=1)P(Y|Class)+ ... +P(Class=C)P(Y|Class=C).



#datasets: 
cbind(head(Dat_var_A), head(Dat_var_B), head(Dat_var_C), head(Dat_var_D))
obj <- 1
(poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1), data = varA_boots[[1]], nclass=4))$P  #proportions zijn direct uit poLCA object te halen

            #onnodige code
            props_A <- prop.table(summary(as.factor(Dat_var_A$trueclass))) #proportions of each class
            props_B <- prop.table(summary(as.factor(Dat_var_B$trueclass))) #proportions of each class
            props_C <- prop.table(summary(as.factor(Dat_var_C$trueclass))) #proportions of each class
            props_D <- prop.table(summary(as.factor(Dat_var_D$trueclass))) #proportions of each class
            cbind(props_A,props_B,props_C,props_D)

#$probs are the class-conditional outcome probabilities for the indicator variables
conditionals <- (poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1), data = varA_boots[[1]], nclass=4))$probs #probabilities zijn direct uit poLCA object te halen


#onnodige code:
              #een 4 bij 4 matrix vullen met de volgende 16 proportions (en dat dan 4 keer)
                  #afgaan per source (dat zijn er vier), en dan dus vier matrices met op de rijen de 4 klassen en op de kolommen de categorieën/opties
              var_A_class1 <- Dat_var_A[which(Dat_var_A$trueclass==1),] #class 1 (houses)
              var_A_class2 <- Dat_var_A[which(Dat_var_A$trueclass==2),] #class 2
              var_A_class3 <- Dat_var_A[which(Dat_var_A$trueclass==3),] #class 3
              var_A_class4 <- Dat_var_A[which(Dat_var_A$trueclass==4),] #class 4
              list_classes <- list(var_A_class1, var_A_class2, var_A_class3, var_A_class4)
              
              
              #alles voor variabele Y1 in een matrix. met op elke row de trueclass, en elke kolom een mogelijk antwoord.(answer i|class =c) 
              Conditionals_Y1 <- matrix(data=NA,nrow=4, ncol=4)
              Conditionals_Y2 <- matrix(data=NA,nrow=4, ncol=4)
              Conditionals_Y3 <- matrix(data=NA,nrow=4, ncol=4)
              Conditionals_Y4 <- matrix(data=NA,nrow=4, ncol=4)
              
              #two for loops to create four 4x4 matrices 
              for (c in 1:4){ #rows, iterate over classes 
                for (i in 1:4){ #columns, iterate over answers
                Conditionals_Y1[c,i] <- sum(list_classes[[c]]$Y1[which(list_classes[[c]]$Y1==i)])/sum(list_classes[[c]]$Y1)
                Conditionals_Y2[c,i] <- sum(list_classes[[c]]$Y2[which(list_classes[[c]]$Y2==i)])/sum(list_classes[[c]]$Y2)
                Conditionals_Y3[c,i] <- sum(list_classes[[c]]$Y3[which(list_classes[[c]]$Y3==i)])/sum(list_classes[[c]]$Y3)
                Conditionals_Y4[c,i] <- sum(list_classes[[c]]$Y4[which(list_classes[[c]]$Y4==i)])/sum(list_classes[[c]]$Y4)
                  } }
              conditionals <- list(Conditionals_Y1, Conditionals_Y2,Conditionals_Y3 ,Conditionals_Y4 )
              conditionals    #resultaten zijn opgeslagen in een list van matrices
              



        

#-------------------------------function to calculate the posterior probabilities-----------#
Pclasses <- (poLCA(formula=(cbind(Y1,Y2,Y3,Y4)~1), data = varA_boots[[1]], nclass=4))$P  #proportions zijn direct uit poLCA object te halen

 ssize <- 5000
    #create storage 
    prob.y.given.x <- array(NA,dim=c(ssize,4))
    prob.y <- c()
    posterior_probs <- array(NA,dim=c(ssize,4))
    #function to calculate posterior probabilities, P(class|scores)
    posterior_function <- function(dataset,  ssize=5000, conditionals){ #need to provide a dataset and the conditional probabilities, P(score|class). Default samplesize is set to 5000
            Pclasses <- prop.table(summary(as.factor(dataset$trueclass))) #proportions of each class
      for(i in 1:ssize){ #nr of observations in dataset
      for(c in 1:4){ #nr of classes
        prob.y.given.x[i,c] <- conditionals[[1]][c,(dataset[i,1])]*conditionals[[2]][c,(dataset[i,2])]*conditionals[[3]][c,(dataset[i,3])]*conditionals[[4]][c,(dataset[i,4])]
      }}
    for(i in 1:ssize){ 
        prob.y[i] <- Pclasses[1]*prob.y.given.x[i,1]+Pclasses[2]*prob.y.given.x[i,2]+Pclasses[3]*prob.y.given.x[i,3]+Pclasses[4]*prob.y.given.x[i,4]
      }
    for(i in 1:ssize){ 
      for(c in 1:4){ #nr of classes
        posterior_probs[i,c] <-  (Pclasses[c]*prob.y.given.x[i,c])/prob.y[i]  
      }}
      posterior_probs<<-posterior_probs #assign result to the environment (to be able to access outside the function)
      }#end function
    posterior_function(dataset = Dat_var_A, conditionals = conditionals)
   #show first five results 
    posterior_probs[1:5,]
 #conclusion: most cases in dataset A can be quite confidently assigned to one class via modal assignment        

