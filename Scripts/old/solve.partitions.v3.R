source("pairwise.distances.R")

library(kernlab)
library(caret)


load( "../../Data/normal-database-tree-only.Rdata" )
load("../../Data/Bullit01/all.trees.processed.02.Rdata ")





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


p.ind = which( sol == vals[1], arr.ind=TRUE ) 

X = all$data[[17]]
X.p <- grid.partition(X[,1:4])
elem1 = is.element(X.p, p.ind[,1])
elem2 = is.element(X.p, p.ind[,2])
ind0 <- which( !(elem1 & elem2) )
ind1 <- which( elem1 )
ind2 <- which( elem2  )

par(mar = c(4,4,2,2) )
par( mfrow = c(1,3) )
xr = range(X$x)
yr = range(X$y)
zr = range(X$z)

symbols( X[ind0, 1:2], circles=X[ind0,4]*1.5, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xlab="x", ylab="y", xaxt="n", yaxt="n", xlim=xr, ylim=yr)

symbols( X[ind1, 1:2], circles=X[ind1,4]*1.5, inches=FALSE, bg="#0097c3", 
        fg="#00000000", add=TRUE)

symbols( X[ind2, 1:2], circles=X[ind2,4]*1.5, inches=FALSE, bg="#E17D1740", 
        fg="#00000000",  add=TRUE)


symbols( X[ind0, c(1,3)], circles=X[ind0,4]*1.5,, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xlab="x", ylab="z", xaxt="n", yaxt="n", xlim=xr, ylim=zr)

symbols( X[ind1, c(1,3)], circles=X[ind1,4]*1.5, inches=FALSE, bg="#0097c3", 
        fg="#00000000", add=TRUE)

symbols( X[ind2, c(1,3)], circles=X[ind2,4]*1.5, inches=FALSE, bg="#E17D1740", 
        fg="#00000000", add=TRUE)


symbols( X[ind0, 2:3], circles=X[ind0,4]*1.5,, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xlab="y", ylab="z", xaxt="n", yaxt="n", xlim=yr, ylim=zr)

symbols( X[ind1, 2:3], circles=X[ind1,4]*1.5, inches=FALSE, bg="#0097c3", 
        fg="#00000000",  add=TRUE)

symbols( X[ind2, 2:3], circles=X[ind2,4]*1.5, inches=FALSE, bg="#E17D1740", 
        fg="#00000000", add=TRUE)




library(rgl)
plot3d(X[ind1,1:3], alpha=0.5, type="s", radius=X[ind1,4], col="#551A8B",
        box=FALSE, axes=FALSE,  xlab="", ylab="", zlab="", )

plot3d(X[ind2,1:3], alpha=0.5, type="s", radius=X[ind2,4], col="#E17D17", add=T )

plot3d(X[ind0,1:3], alpha=0.15,  type="s", radius=X[ind0,4], col="#000000", add=T)




