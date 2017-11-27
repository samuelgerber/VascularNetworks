source("pairwise.distances.R")

library(kernlab)
library(caret)


load( "../../Data/normal-database-tree-only.Rdata" )
#load("../../Data/all.trees.Rdata")

distances = load.transport.distances( 
  "../../Processed/Transport06/transport-maps/", 42 )


# a few plots
image(distances)
dev.new()
mds = cmdscale(distances, k=40, eig=TRUE)
plot( mds$eig )
dev.new()
plot( mds$points, col=as.factor( labels[,2] ) )


#repeated crossvalidation
resGLM <- c()
resLM <- c()
for(i in 1:20){
  data1 = data.frame( mds$points[,1:i], gender = as.factor(labels[,2]) )
  levels( data1$gender ) <- c("F", "M", NA)
  data2 = data.frame( mds$points[,1:i], age = labels[,1] )

  trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 200)
  #svm_Linear <- train(gender ~., data = data, method = "svmLinear", trControl=trctrl)
  #svm_Radial <- train(gender ~., data = data, method = "svmRadialWeights", trControl=trctrl)
  glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
  lmCV <- train(age ~., data = data2, method = "lm", trControl=trctrl)
  resGLM <- rbind(resGLM, glmCV$result)
  resLM <- rbind(resLM, lmCV$result)

}

tg <- expand.grid(alpha = 1, lambda = seq(10, 0, length.out=10)^2)
trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 50)

  glmnet1 <- train(gender ~., data = data1, method = "glmnet", trControl=trctrl, tuneGrid=tg)
  glmnet2 <- train(age ~., data = data2, method = "glmnet", trControl=trctrl, tuneGrid=tg)

#old stuff

#lm1 <- lm( labels[,1] ~ mds$points[, 1:4] )
#summary(lm1)


#glm1 <- glm(gender ~.,family=binomial(link='logit'), data=data)
#summary( glm1 )
#p <- as.factor(predict(glm1) > 0)
#levels(p) <- c("F","M", "M-F")
#Accuracy(data$gender, p)
#Precision(data$gender, p)
#Recall(data$gender, p)

#library(e1071)

#svm1 = svm(  gender ~ ., data)

#library(MLmetrics)
#Accuracy(data$gender, svm1$fitted)
#Precision(data$gender, svm1$fitted)
#Recall(data$gender, svm1$fitted)


