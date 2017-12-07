source("pairwise.distances.R")

library(kernlab)
library(caret)


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





#least squares with l1 penalty
library(lbfgs)
f.eval.lbfgs <- function(x){
  y = A %*% x
  sum( (y - B)^2 )
}

g.eval.lbfgs <- function(x){
  y = A %*% x
  t(A) %*% ( 2*(y-B) )
}

res.lbfgs <- lbfgs( f.eval.lbfgs, g.eval.lbfgs,vars= res.lbfgs$par, 
                    orthantwise_c=1000, max_iterations=1000)

sol <- matrix(res.lbfgs$par, n.partitions)

dists.proj <- A %*% res.lbfgs$par


#least squares
n.p2 = n.partitions^2
f.eval <- function(x){
  y = A %*% x
  objective = sum( (y - B)^2 )

  gradient <- t(A) %*% ( 2*(y-B) )
  
  gradient =   list(objective=objective, gradient=gradient)
}



library(nloptr)

opts <- list("algorithm"="NLOPT_LD_LBFGS", 
             "xtol_rel"=1.0e-3, 
             "print_level"=1, 
             "maxeval" = 1000
             )
res$solution[res$solution<0] = 0;
res <- nloptr( x0 = res$solution,#rep(0, n.p2), 
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


sig.dist <- c()
i.rm <- which(sol !=0, arr.ind=TRUE)
for( i in 1:nrow(i.rm) ){
  if(i.rm[i,1] < i.rm[i, 2]){
    
    tmp <- matrix(0, nrow(sol), ncol(sol) )  
    tmp[i.rm[i,1], i.rm[i, 2] ] = sol[i.rm[i,1], i.rm[i, 2] ]
    tmp[i.rm[i,2], i.rm[i, 1] ] = sol[i.rm[i,2], i.rm[i, 1] ]
    #tmp <- sol
    #tmp[i.rm[i,1], i.rm[i, 2] ] = 0
    #tmp[i.rm[i,2], i.rm[i, 1] ] = 0
    dists.tmp <- A %*% as.vector(tmp)
    sig.dist = rbind( sig.dist, c( mean(dists.tmp / dists.proj, na.rm=T), i.rm[i,]) )
  }
}
sig.dist <- as.data.table(sig.dist)
colnames(sig.dist) <- c("fraction", "index1", "index2")
sig.dist <- sig.dist[order(fraction, decreasing=T), ]



par(mar = c(1,1,1,1) )
par( mfrow = c(3,3) )


for(i in 1:3){
p.ind <- sig.dist[i, 2:3]


X1 = all$data[[1]]
xr = range(X1$x)
yr = range(X1$y)
zr = range(X1$z)

X = expand.grid(seq(xr[1], xr[2], length.out=20),
                seq(yr[1], yr[2], length.out=20),
                seq(zr[1], zr[2], length.out=20)
                )
X[,4] = 4
X.p <- grid.partition(X[,1:4])
elem1 = is.element(X.p, t(p.ind[ ,1]))
elem2 = is.element(X.p, t(p.ind[ ,2]))
ind0 <- which( !(elem1 | elem2) )
ind1 <- which( elem1 )
ind2 <- which( elem2  )
X[ind0, 4] = 0
symbols( X1[, 1:2], circles=X1[,4]*1.5, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xaxt="n", yaxt="n", xlim=xr, ylim=yr)

if(length(ind1) > 0 ){
symbols( X[ind1, 1:2], circles=X[ind1,4]*1.5, inches=FALSE, bg="#0097c340", 
        fg="#00000000", add=TRUE)
}
if(length(ind2) > 0 ){
symbols( X[ind2, 1:2], circles=X[ind2,4]*1.5, inches=FALSE, bg="#E17D1740", 
        fg="#00000000",  add=TRUE)
}



symbols( X1[, c(1,3)], circles=X1[,4]*1.5,, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xaxt="n", yaxt="n", xlim=xr, ylim=zr)

if(length(ind1) > 0 ){
symbols( X[ind1, c(1,3)], circles=X[ind1,4]*1.5, inches=FALSE, bg="#0097c340", 
        fg="#00000000", add=TRUE)
}
if(length(ind2) > 0 ){
symbols( X[ind2, c(1,3)], circles=X[ind2,4]*1.5, inches=FALSE, bg="#E17D1740", 
        fg="#00000000", add=TRUE)
}

symbols( X1[, 2:3], circles=X1[,4]*1.5,, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xaxt="n", yaxt="n", xlim=yr, ylim=zr)

if(length(ind1) > 0 ){
symbols( X[ind1, 2:3], circles=X[ind1,4]*1.5, inches=FALSE, bg="#0097c340", 
        fg="#00000000",  add=TRUE)
}
if(length(ind2) > 0 ){
symbols( X[ind2, 2:3], circles=X[ind2,4]*1.5, inches=FALSE, bg="#E17D1740", 
        fg="#00000000", add=TRUE)
}

}


library(rgl)
plot3d(X[ind1,1:3], alpha=0.5, type="s", radius=X[ind1,4], col="#551A8B",
        box=FALSE, axes=FALSE,  xlab="", ylab="", zlab="", )

plot3d(X[ind2,1:3], alpha=0.5, type="s", radius=X[ind2,4], col="#E17D17", add=T )

plot3d(X[ind0,1:3], alpha=0.15,  type="s", radius=X[ind0,4], col="#000000", add=T)




