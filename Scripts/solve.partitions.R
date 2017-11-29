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

n.subjects = 42
n.partitions = 16
partitions = load.transport.partitions( 
  "../../Processed/Transport06/transport-maps/", n.subjects, grid.partition, n.partitions)



n.k = 20
x <- cmdscale(partitions$distances, k=n.k)
#setup linear programing problem to sole for partitions
d.k <- array(0, c(n.subjects, n.subjects, ncol(x) ) )
for(i in 1:ncol(x) ){
  d.k[,,i] = as.matrix( dist(x[,i]) )
}


library(clpAPI)

# preparing the model
lp <- initProbCLP()
     
ncols  <- n.partitions^2 * n.k
nrows  <- n.subjects^2 * n.k
     
# objective function
obj <- rep(1, ncols)

# upper and lower bounds of the cols
clower <- rep(0, ncols)
cupper <- rep(1, ncols)
     
# upper and lower bounds of the rows
rlower <- c()
for(i in 1:n.subjects){
  for(j in 1:n.subjects){
    if( i != j ){
      for(k in 1:n.k){
        rlower <- c(rlower, d.k[i,j,k]  )  
      }
    }
  }
}
rlower <- c(rlower, rep(1, n.k) )
rupper <- clower     


# constraint matrix
ia <- as.vector(  outer( rep(0:(n.partitions^2-1), n.subjects*(n.subjects-1) ), 
                         (0:(n.k-1)) * n.partitions^2, "+") )
print( length(ia) )
ia <- c(ia, as.vector(  outer( 0:(n.partitions^2-1), (0:(n.k-1)) * n.partitions^2, "+") ) )
print( length(ia) )

ja <- n.partitions^2 * 0:(n.subjects * (n.subjects-1) * n.k + n.k)

ar <- c()
for(i in 1:n.subjects){
  for(j in 1:n.subjects){
    if( i != j ){
      ar <- c(ar, rep( as.vector( partitions$cost[,,i,j]), n.k ) )    
    }
  }
}
print(length(ar))
ar <- c( ar, rep(1, n.partitions^2 * n.k) )
print(length(ar))

browser()

# direction of optimization
setObjDirCLP(lp, 1)
     
# load problem data
loadProblemCLP( lp, ncols, nrows, ia, ja, ar,
                clower, cupper, obj, rlower, rupper)
     
# solve lp problem
solveInitialCLP(lp)
     
# retrieve the results
statsus <- getSolStatusCLP(lp)
objective <- getObjValCLP(lp)
primal <- getColPrimCLP(lp)
     
# remove problem object
delProbCLP(lp)
     




#repeated crossvalidation
resGLM <- c()
resLM <- c()
for(i in 1:20){
  data1 = data.frame( x[,1:i], gender = as.factor(labels[,2]) )
  levels( data1$gender ) <- c("F", "M", NA)
  data2 = data.frame( x[,1:i], age = labels[,1] )

  trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 20)
  #svm_Linear <- train(gender ~., data = data, method = "svmLinear", trControl=trctrl)
  #svm_Radial <- train(gender ~., data = data, method = "svmRadialWeights", trControl=trctrl)
  glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
  lmCV <- train(age ~., data = data2, method = "lm", trControl=trctrl)
  resGLM <- rbind(resGLM, glmCV$result)
  resLM <- rbind(resLM, lmCV$result)

}

