source("pairwise.distances.R")

library(kernlab)
library(caret)


load( "../../Data/normal-database-tree-only.Rdata" )
#load("../../Data/all.trees.Rdata")





grid.partition <- function(X){
 
  partition <- 0
  for( i in 1:ncol(X) ){
    partition = partition + (cut(X[,i], 2, labels=FALSE)-1) * 2^(i-1)
  }
  partition + 1

}

n.partitions = 16
distances = load.transport.distances.partitioned( 
  "../../Processed/Transport06/transport-maps/", 42, grid.partition, n.partitions, 2 )

#repeated crossvalidation
resGLM <- c()
resLM <- c()
  

points <- c()
for( i in 1:n.partitions){
  for( j in 1:n.partitions){
    mds <- cmdscale( distances[[i]][[j]], k=3 )
    s = sprintf( "%d %d: %d", i, j, ncol(mds) ) 
    print(s)
    if( ncol(mds) > 0){
    points <- cbind(points, mds)  
     
data1 = data.frame( mds, gender = as.factor(labels[,2]) )
levels( data1$gender ) <- c("F", "M", NA)
data2 = data.frame( mds, age = labels[,1] )

trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 50)
#svm_Linear <- train(gender ~., data = data, method = "svmLinear", trControl=trctrl)
#svm_Radial <- train(gender ~., data = data, method = "svmRadialWeights", trControl=trctrl)
glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
lmCV <- train(age ~., data = data2, method = "lm", trControl=trctrl)
resGLM <- rbind(resGLM, glmCV$result)
resLM <- rbind(resLM, lmCV$result)
    }
  }
}

data1 = data.frame( points, gender = as.factor(labels[,2]) )
levels( data1$gender ) <- c("F", "M", NA)
data2 = data.frame( points, age = labels[,1] )

trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 200)
#svm_Linear <- train(gender ~., data = data, method = "svmLinear", trControl=trctrl)
#svm_Radial <- train(gender ~., data = data, method = "svmRadialWeights", trControl=trctrl)
glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
lmCV <- train(age ~., data = data2, method = "lm", trControl=trctrl)
resGLM <- rbind(resGLM, glmCV$result)
resLM <- rbind(resLM, lmCV$result)



tg <- expand.grid(alpha = 0, lambda = seq(10, 0, length.out=10)^2)
trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 50)

glmnet1 <- train(gender ~., data = data1, method = "glmnet", trControl=trctrl, tuneGrid=tg)
glmnet2 <- train(age ~., data = data2, method = "glmnet", trControl=trctrl, tuneGrid=tg)


sum = 0
for( i in 1:n.partitions){
  for( j in 1:n.partitions){
    sum = sum + distances[[i]][[j]][1,2]^2
  }
}
