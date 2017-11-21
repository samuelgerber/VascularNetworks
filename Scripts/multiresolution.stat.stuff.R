source("multiresolution.pairwise.distances.R")

library(kernlab)
library(caret)


load( "../../Data/normal-database-tree-only.Rdata" )

nscales = 8
distances = load.multiresolution.transport.v1.distances( 
  "../../Processed/MultiresolutionTransportV1-02/transport-maps/", 42, nscales )


# a few plots
points.all = c()
par(mfrow = c(2, 4) )
#repeated crossvalidation
resGLM <- c()
resLM <- c()
for( i in 1:nscales){
  dtmp = distances$distances[[i]]
  #dtmp  = 0;
  #for( s in nscales:i){
  #  dtmp = dtmp +distances[[s]]
  #}
  mds = cmdscale(dtmp, k=41, eig=TRUE)
  plot( mds$eig )
  #plot( mds$points, col=as.factor( labels[,2] ) )
  points.all = cbind(points.all, mds$points[,1:3])
  points = mds$points[,1:3]

  data1 = data.frame( points, gender = as.factor(labels[,2]) )
  levels( data1$gender ) <- c("F", "M", NA)
  data2 = data.frame( points, age = labels[,1] )

  trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 50)
  #svm_Linear <- train(gender ~., data = data, method = "svmLinear", trControl=trctrl)
  #svm_Radial <- train(gender ~., data = data, method = "svmRadialWeights", trControl=trctrl)
  glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
  lmCV <- train(age ~., data = data2, method = "lm", trControl=trctrl)
  resGLM <- rbind(resGLM, glmCV$result)
  resLM <- rbind(resLM, lmCV$result)

}

data1 = data.frame( matrix(runif(42*5), nrow=42), gender = as.factor(labels[,2]) )
levels( data1$gender ) <- c("F", "M", NA)
data2 = data.frame( matrix(runif(42*5), nrow=42), age = labels[,1] )

trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 50)
  #svm_Linear <- train(gender ~., data = data, method = "svmLinear", trControl=trctrl)
  #svm_Radial <- train(gender ~., data = data, method = "svmRadialWeights", trControl=trctrl)
glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
lmCV <- train(age ~., data = data2, method = "lm", trControl=trctrl)
glmCV
lmCV
