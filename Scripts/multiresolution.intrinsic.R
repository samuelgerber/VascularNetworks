library(mop)
source("../../Code/Scripts/multiresolution.R")

args <- commandArgs(trailingOnly = TRUE)
index = as.integer( args[1] )

print("Doing transport on index")
print( index )
load("../../Code/Data/Bullit01/all.trees.processed.02.Rdata")

dir.create("./transport-maps")


trp.lp <- multiscale.transport.create.lp( oType = 26 )
icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )

features <- matrix(NA, nrow=42, ncol=10)
for( i in 1:length(all$data) ){
  X = all$data[[i]]
  mres = multiresolution.gmra(X, n=11)

  trp = list()
  for(scale in 1:10){
    if(scale==1){
      w1 <- mres$Xin$v
    }
    else{
      w1 <- mres$X[[scale-1]]$v
    }
    w2 <- mres$X[[scale]]$v
    trp[[scale]] = multiscale.transport.solve( 
                                              trp.lp, 
                                              mres$gmra[[scale]], 
                                              mres$gmra[[scale+1]], 
                                              p = 2, nType=0, 
                                              dType=1, 
                                              w1=w1, 
                                              w2=w2
                                             )
    features[i, scale] = trp[[scale]]$cost[ length(trp[[scale]]$cost) ]
  }
  save( trp, file = sprintf("transport-maps/transport%.3d.Rdata", i) )
}

save( features, file = "features.Rdata" )


library(kernlab)
library(caret)

load( "../../Data/normal-database-tree-only.Rdata" )

resGLM <- c()
resLM <- c()

#for(i in 1:10 ){
  data1 = data.frame( features[,c(7,8,10)], gender = as.factor(labels[,2]) )
  levels( data1$gender ) <- c("F", "M", NA)
  data2 = data.frame( features[,c(1,8)], age = labels[,1] )

  trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 20)
  #svm_Linear <- train(gender ~., data = data, method = "svmLinear", trControl=trctrl)
  #svm_Radial <- train(gender ~., data = data, method = "svmRadialWeights", trControl=trctrl)
  glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
  lmCV <- train(age ~., data = data2, method = "lm", trControl=trctrl)
  resGLM <- rbind(resGLM, glmCV$result)
  resLM <- rbind(resLM, lmCV$result)

#}

