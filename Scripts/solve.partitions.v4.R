source("pairwise.distances.R")

library(kernlab)
library(caret)


load( "../../Data/normal-database-tree-only.Rdata" )
load("../Data/Bullit01/all.trees.processed.02.Rdata")




grid.partition.weight <- function(X){
 
  s2 = 100
  partition <- list()
  r <- list()
  for( i in 1:4){
   r[[i]] <- c(range(X[,i]), 0)
   d <- r[[i]][2] - r[[i]][1]
   r[[i]][4] =r[[i]][1] + 4*d/5
   r[[i]][3] =r[[i]][1] + 3*d/5
   r[[i]][2] =r[[i]][1] + 2*d/5
   r[[i]][1] =r[[i]][1] + 1*d/5
  }
  index = 1
  for( i in 1:4 ){
    for( j in 1:4 ){
      for( k in 1:4 ){
        D <- sweep(X[, 1:3], 2, c(r[[1]][i], r[[2]][j], r[[3]][k]) )
        partition[[index]] <- exp(-rowSums(D^2) / (2*s2))
        #partition[[index]] <- partition[[index]] / sum(partition[[index]] )
        index <- index + 1
      }
    }
  }
  sum.partition <- Reduce("+", partition)
  for(i in 1:length(partition) ){
    partition[[i]] = partition[[i]] / sum.partition
  }
  partition
}

n.subjects = 42
n.partitions = 64
partitions = load.transport.weighted.partitions( 
  "../../Processed/Transport06/transport-maps/", n.subjects, grid.partition.weight, n.partitions)



n.k = 20
x <- cmdscale(partitions$distances, k=n.k)
#setup linear programing problem to solve for partitions
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
    A <- cbind(A, as.vector( partitions$cost[ , , k2, k1] ) )
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
             "maxeval" = 10000
             )
res <- nloptr( x0 = rep(0, n.p2), 
               eval_f = f.eval,
               lb = rep(0, n.p2),
               ub = rep(1, n.p2),
               opts = opts)


sol <- matrix(res$solution, n.partitions)


dists.proj <- A %*% res$solution

mds2 <- cmdscale( matrix(dists.proj, n.subjects), eig=TRUE, k=41)

data1 = data.frame( mds2$points[,1], gender = as.factor(labels[,2]) )
levels( data1$gender ) <- c("F", "M", NA)
trctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 20)
glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
glmCV



vals <- sort(sol[sol!=0], decreasing=TRUE)
vals <- vals[seq(1,length(vals), by=2)]


p.ind <-   which( sol == vals[3], arr.ind=TRUE ) 


X = all$data[[17]]
X.p <- grid.partition.weight(X[,1:4])
elem1 = is.element(X.p, p.ind[,1])
elem2 = is.element(X.p, p.ind[,2])

par(mar = c(4,4,2,2) )
par( mfrow = c(1,3) )
xr = range(X$x)
yr = range(X$y)
zr = range(X$z)

col1 <- rgb( 0.00, 0.59, 0.76, X.p[[p.ind[1,1]]] )
col2 <- rgb( 0.88, 0.49, 0.09, X.p[[p.ind[1,2]]] )


symbols( X[, 1:2], circles=X[,4]*1.5, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xlab="x", ylab="y", xaxt="n", yaxt="n", xlim=xr, ylim=yr)
symbols( X[, 1:2], circles=X[,4]*1.5, inches=FALSE, bg=col1, fg="#00000000", add=TRUE)
symbols( X[, 1:2], circles=X[,4]*1.5, inches=FALSE, bg=col2, fg="#00000000",  add=TRUE)

symbols( X[, c(1,3)], circles=X[,4]*1.5,, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xlab="x", ylab="z", xaxt="n", yaxt="n", xlim=xr, ylim=zr)
symbols( X[, c(1,3)], circles=X[,4]*1.5, inches=FALSE, bg=col1, fg="#00000000", add=TRUE)
symbols( X[, c(1,3)], circles=X[,4]*1.5, inches=FALSE, bg=col2, fg="#00000000", add=TRUE)

symbols( X[, 2:3], circles=X[,4]*1.5,, inches=FALSE, bg="#00000010",
         fg="#00000000", bty="n", xlab="x", ylab="z", xaxt="n", yaxt="n", xlim=yr, ylim=zr)
symbols( X[, 2:3], circles=X[,4]*1.5, inches=FALSE, bg=col1, fg="#00000000", add=TRUE)
symbols( X[, 2:3], circles=X[,4]*1.5, inches=FALSE, bg=col2, fg="#00000000",  add=TRUE)



