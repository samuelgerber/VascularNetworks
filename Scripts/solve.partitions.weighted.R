source("pairwise.distances.R")
source("partition.plot.R")
source("partition.least.squares.R")
library(caret)


load( "../../Data/normal-database-tree-only.Rdata" )
load("../Data/Bullit01/all.trees.processed.02.Rdata")




grid.partition.weight <- function(X){
 
  s2 = 400
  partition <- list()
  r <- matrix(0, nrow=3, ncol=4)
  for( i in 1:3){
   tmp <- range(X[,i])
   d <- tmp[2] - tmp[1]
   r[i,] = seq( tmp[1]+d/5, tmp[1] + 4*d/5, length=4)
  }
  index = 1
  for( i in 1:4 ){
    for( j in 1:4 ){
      for( k in 1:4 ){
        D <- sweep(X[, 1:3], 2, c(r[1,i], r[2,j], r[3,k]) )
        D[,3] = D[,3]*3
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
#partitions = load.transport.weighted.partitions( 
#  "../../Processed/Transport06/transport-maps/", n.subjects, 
#  grid.partition.weight, n.partitions )


ls <- partition.least.squares(partitions, labels)

mds2 <- cmdscale( matrix(ls$dists.proj, n.subjects), eig=TRUE, k=41)
data1 = data.frame( mds2$points[,1], gender = as.factor(labels[,2]) )
levels( data1$gender ) <- c("F", "M", NA)
trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 20)
glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
glmCV


A <- matrix( as.vector(partitions$cost), nrow=n.subjects^2, ncol=n.partitions^2) 
sig.dist <- distance.fraction( ls$sol, ls$dists.proj, A )
partition.plot.weighted( sig.dist.c[1:3, ], all$data[[1]], grid.partition.weight )
ls.cv <- partition.least.squares.cv( partitions, labels )











#compare to just using mass in each partition
trctrl <- trainControl( method = "repeatedcv", number = 3, repeats = 20, 
                        classProbs = TRUE, summaryFunction = twoClassSummary )

V <- matrix(NA, nrow=n.subjects, ncol=n.partitions)
R <- matrix(NA, nrow=n.subjects, ncol=n.partitions)

for(i in 1:n.subjects){
  X <- all$data[[i]]
  X.p <- grid.partition.weight( X[,1:4] )
  for(j in 1:length(X.p) ){
    V[i,j] =  sum( X.p[[j]] * X$v ) / sum(X.p[[j]] ) 
    R[i,j] =  sum( X.p[[j]] * X$r ) / sum(X.p[[j]] ) 
  }
}

data1 = data.frame( prcomp( R)$x, gender = as.factor(labels[,2]) )
levels( data1$gender ) <- c("F", "M", NA)
res1 <- c()
for(i in 1:20){
  glmCV <- train(gender ~., data = data1[, c(1:i,43)], method = "glm", trControl=trctrl)
  res1 <- rbind(res1, glmCV$results)
}

  
x <- cmdscale(partitions$distances, k=20)
res <- c()
for( i in 1:ncol(x)){
  data1 = data.frame( x[,1:i], gender = as.factor(labels[,2]) )
  levels( data1$gender ) <- c("F", "M", NA)
  levels( data1$gender ) <- c("F", "M", NA)
  glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl, metric="ROC")
  res <- rbind(res, glmCV$results)
}

ylim <-c( min(res1$ROC-res1$ROCSD), max(res$ROC + res$ROCSD) )


par(mar=c(5,5,2,2))
plot( res$ROC, cex=2, ylim=ylim, xlab="Dimensions", ylab="AUC", 
      cex.lab=2, cex.axis=1.5, bty="n", axes=FALSE)
axis(1, c(1,5,10,15,20), cex.axis=1.5)
axis(2, cex.axis=1.5)
abline(h=24/41, col="gray", lwd=3)
arrows(1:20, res$ROC-res$ROCSD, 1:20, res$ROC+res$ROCSD, length=0.05, angle=90, code=3, lwd=2)
points(res$ROC, col="#0097c3", pch=19, cex=2)
points(res$ROC, cex=2)
arrows(1:20+0.3, res1$ROC-res1$ROCSD, 1:20+0.3, res1$ROC+res1$ROCSD, length=0.05, angle=90, code=3, lwd=2)
points(1:20+0.3, res1$ROC, col="#E17D17", pch=19, cex=2)
points(1:20+0.3, res1$ROC, cex=2)


#glm.model <- glm(gender ~ ., data=data1, family=binomial() )
#glm.dir   <- glm.model$coefficients[-1]
#glm.dir   <- glm.dir / sum(glm.dir^2)
#x.proj <- x %*% glm.dir

#sig.dist.c <- distance.fraction.cor(ls$sol, as.vector(as.matrix(dist(x.proj))), A)



#data1 = data.frame( x[,c(1:15)], gender = as.factor(labels[,2]) )
#levels( data1$gender ) <- c("F", "M", NA)
#levels( data1$gender ) <- c("F", "M", NA)
#glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl, metric="ROC")

