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
  d.k[,,i] = as.matrix( dist(x[,i]) )^2
}


library(clpAPI)

# preparing the model
 
ncols  <- n.partitions^2 * n.k
nrows  <- (n.subjects^2-n.subjects) * n.k + n.partitions^2
     
# objective function
obj <- rep(1, ncols)
#obj <- c()
#for(i in 1:n.k){
#  obj = c(obj, rep( mean(d.k[,,i]), n.partitions^2)  )  
#}


# upper and lower bounds of the cols
clower <- rep(0, ncols)
cupper <- rep(1, ncols)
     
# upper and lower bounds of the rows
rupper <- c()
for(k in 1:n.k){
  for( i in 1:n.subjects ){
    rupper <- c(rupper, d.k[i,-i, k] * 10 )  
  }
}
rlower = rep(0, length(rupper) )
rupper <- c(rupper, rep(1, n.partitions^2) )
rlower <- c(rlower, rep(1, n.partitions^2) )


# constraint matrix
ia <- as.vector(
        rbind(  
          outer( 0:(n.subjects^2 - n.subjects-1), 
                 rep( (0:(n.k-1)) * (n.subjects^2 - n.subjects), each=n.partitions^2) , "+"),
          rep( nrows - (1:(n.partitions^2) ),  n.k )
        )
      )
         


ja <- (n.subjects^2 - n.subjects + 1 ) * 0:(n.partitions^2  * n.k )

ar <- c()
for(k1 in 1:n.partitions){
  for(k2 in 1:n.partitions){
    for(i in 1:n.subjects){
       ar <- c(ar, partitions$mass[i,-i,k1,k2]*partitions$cost[i,-i, k1, k2] )
    }
    ar <- c(ar, 1)
  }
}
ar <- rep(ar, n.k)

print( length(ia) )
print( length(ar) )
print( max(ia) )
print( length(ja) )
print( max(ja) )

browser()


lp <- initProbCLP()
    
# direction of optimization
setObjDirCLP(lp, -1)
     
# load problem data
loadProblemCLP( lp, ncols, nrows, ia, ja, ar,
                clower, cupper, obj, rlower, rupper)
     
# solve lp problem
solveInitialCLP(lp)
     
# retrieve the results
status <- getSolStatusCLP(lp)
objective <- getObjValCLP(lp)
colPrimal <- getColPrimCLP(lp)
rowDual <- getRowDualCLP(lp)    
rowPrimal <- getRowPrimCLP(lp)

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

