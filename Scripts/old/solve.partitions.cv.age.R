source("pairwise.distances.R")

library(kernlab)
library(caret)
library(MLmetrics)

load( "../../Data/normal-database-tree-only.Rdata" )
load("../Data/Bullit01/all.trees.processed.02.Rdata")





grid.partition <- function(X){
 
  partition <- 0
  for( i in 1:(ncol(X)-1) ){
    partition = partition + (cut(X[,i], 4, labels=FALSE)-1) * 4^(i-1)
  }
  partition + 1

}

n.subjects = 42
n.partitions = 4^3
#partitions = load.transport.partitions( 
#  "../../Processed/Transport06/transport-maps/", n.subjects, grid.partition, n.partitions)



n.k = 20
n.p2 = n.partitions^2

#leasts quares with eqaulity constraints
A <-  matrix( as.vector(partitions$cost), nrow=42^2, ncol=n.p2)


x <- cmdscale(partitions$distances, k=n.k)

#setup linear programing problem to solve for partitions
d.k <- array(0, c(42, 42, ncol(x) ) )
for(i in 1:ncol(x) ){
  d.k[,,i] = as.matrix( dist(x[,i]) )^2
}
res$solution = rep(0, n.p2)
  
n.repeats <- 100
lmCV <- rep(NA, n.repeats)
sols <- list()
lms <- c()
for(k in 1:n.repeats){
  perm <-sample(1:n.subjects)
  index.train = perm[1:28]
  index.test = perm[29:42]
  



  #data1 = data.frame( x[index.train,1:15], gender = as.factor(labels[index.train,2]) )
  #levels( data1$gender ) <- c("F", "M", NA)
  #glm.model <- glm(gender ~ ., data=data1, family=binomial() )
  #glms <- rbind(glms, glm.model$coefficients)
  #glm.dir   <- glm.model$coefficients[-1]
  #glm.dir   <- glm.dir / sum(glm.dir^2)
  #print(glm.model)
  #x.proj <- x[ ,1:15] %*% glm.dir
  #B <- as.vector( as.matrix(dist(x.proj))^2 )

  B <- as.vector( as.matrix( dist(as.integer( labels[index.train,1] ) ))^2 )


  #least squares
  f.eval <- function(x){
    y = A %*% x
    objective = sum( (y - B)^2 )

    gradient <- t(A) %*% ( 2*(y-B) )
  
    list(objective=objective, gradient=gradient)
  }



  library(nloptr)

  opts <- list("algorithm"="NLOPT_LD_LBFGS", 
             "xtol_rel"=1.0e-4,
             "xtol_abs"=rep(1e-6, n.p2),
             "print_level"=1, 
             "maxeval" = 1000
             )
  res <- nloptr( x0 = res$solution, 
                 eval_f = f.eval,
                 lb = rep(0, n.p2),
                 ub = rep(1, n.p2),
                 opts = opts
                )

  sol <- matrix(res$solution, n.partitions)
  sols[[k]] <- sol
  dists.proj <- A %*% res$solution

  mds2 <- cmdscale( matrix(dists.proj, n.subjects), eig=TRUE, k=41)
  data.train = data.frame( x = mds2$points[index.train,1], age = labels[index.train,1] )
  data.test  = data.frame( x = mds2$points[index.test,1] , age = labels[index.test,1] )
  colnames(data.test) <- colnames(data.train) 

  lm.model <- lm(age ~ ., data=data.train, family=binomial() )
  p.test <- predict(lm.model, newdata=data.test )

  lmCV[k] <- RMSE(p.test, data.test$age)

}



