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

data1 = data.frame( x[,1:15], gender = as.factor(labels[,2]) )
levels( data1$gender ) <- c("F", "M", NA)
glm.model <- glm(gender ~ ., data=data1, family=binomial() )
glm.dir <- glm.model$coefficients[-1]
glm.dir <- glm.dir / sum(glm.dir^2)

x.proj <- x[,1:15] %*% glm.dir

#leasts quares with eaulity constraints
A <- c()
for(k1 in 1:n.partitions){
  for(k2 in 1:n.partitions){
    A <- cbind(A, as.vector( partitions$cost[,, k1, k2] ) )
  }
}

#least squares objective
B <- as.vector(as.matrix(dist(x.proj))^2)

n.p2 = n.partitions^2
f.eval <- function(x){
  y = A %*% x
  objective = sum( (y - B)^2 )

  gradient <- t(A) %*% ( 2*(y-B) )
  
  gradient =   list(objective=objective, gradient=gradient)
}



library(nloptr)

opts <- list("algorithm"="NLOPT_LD_MMA", 
             "xtol_rel"=1.0e-7, 
             "print_level"=1, 
             "maxeval" = 10000,
             )
res <- nloptr( x0 = rep(0, n.p2), 
               eval_f = f.eval,
               lb = rep(0, n.p2),
               ub = rep(1, n.p2),
               opts = opts)




dists.proj <- A %*% res$solution

mds2 <- cmdscale( matrix(dists.proj, n.subjects), eig=TRUE, k=41)

data1 = data.frame( mds2$points[,1], gender = as.factor(labels[,2]) )
levels( data1$gender ) <- c("F", "M", NA)
trctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 20)
glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
glmCV

