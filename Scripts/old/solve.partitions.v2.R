source("pairwise.distances.R")

library(kernlab)
library(caret)


load( "../../Data/normal-database-tree-only.Rdata" )
#load("../../Data/all.trees.Rdata")





grid.partition <- function(X){
 
  partition <- 0
  for( i in 1:(ncol(X)-1) ){
    partition = partition + (cut(X[,i], 3, labels=FALSE)-1) * 3^(i-1)
  }
  partition + 1

}

n.subjects = 42
n.partitions = 27
partitions = load.transport.partitions( 
  "../../Processed/Transport06/transport-maps/", n.subjects, grid.partition, n.partitions)



n.k = 20
x <- cmdscale(partitions$distances, k=n.k)
#setup linear programing problem to sole for partitions
d.k <- array(0, c(n.subjects, n.subjects, ncol(x) ) )
for(i in 1:ncol(x) ){
  d.k[,,i] = as.matrix( dist(x[,i]) )^2
}



#leasts quares with eaulity constraints
A <- c()
for(k1 in 1:n.partitions){
  for(k2 in 1:n.partitions){
    A <- cbind(A, as.vector( partitions$cost[,, k1, k2] ) )
  }
}

#least squares objective
B <- c()
for(k in 1:n.k){
  B <- c(B, d.k[,, k] )  
}

n.p2 = n.partitions^2
f.eval <- function(x){
  y = c()
  for(i in 1:n.k){
    start = (i-1)*n.p2 + 1
    y = c(y, A %*% x[ start:(start+n.p2-1) ] )
  } 
  objective = sum( (y - B)^2 )

  gradient <- c()
  for(i in 1:n.k){
    start = (i-1)*nrow(A) + 1
    gradient = c(gradient, t(A) %*% ( 2*(y-B)[start:(start+nrow(A)-1)] ) )
  } 
  gradient =   list(objective=objective, gradient=gradient)
}



eq.jacobian <- matrix(0, n.p2, n.k*n.p2)
index = 0:(n.k-1) *n.p2
for( i in 1:n.p2){
  eq.jacobian[i, index+i] = 1
}

eq.eval <- function(x){
  print(summary(x))
  constraints <- rowSums( matrix(x, ncol=n.k) ) - 1
  list(constraints=constraints, jacobian=eq.jacobian)
}



library(nloptr)
local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-2 )
opts <- list("algorithm"="NLOPT_LD_AUGLAG", 
             "xtol_rel"=1.0e-3, 
             "print_level"=1, 
             "maxeval" = 10000,
             "local_opts" = local_opts
             )
res <- nloptr( x0 = rep(0, n.p2*n.k), 
               eval_f = f.eval,
               eval_g_eq = eq.eval,
               lb = rep(0, n.p2*n.k),
               opts = opts)


opts <- list("algorithm"="NLOPT_LD_MMA", 
             "xtol_rel"=1.0e-7, 
             "print_level"=1, 
             "maxeval" = 10000,
             )
res <- nloptr( x0 = rep(0, n.p2*n.k), 
               eval_f = f.eval,
               lb = rep(0, n.p2*n.k),
               ub = rep(1, n.p2*n.k),
               opts = opts)




proj <- list()
for(i in 1:n.k){
  start = (i-1)*n.p2 + 1
  proj[[i]] <- res$solution[start:(start+n.p2-1)]
}



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

