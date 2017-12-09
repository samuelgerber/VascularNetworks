source("pairwise.distances.R")
source("partition.plot.R")
source("partition.least.squares.R")

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


ls <- partition.least.squares(partitions, labels)

mds2 <- cmdscale( matrix(ls$dists.proj, n.subjects), eig=TRUE, k=41)
data1 = data.frame( mds2$points[,1], gender = as.factor(labels[,2]) )
levels( data1$gender ) <- c("F", "M", NA)
trctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 20)
glmCV <- train(gender ~., data = data1, method = "glm", trControl=trctrl)
glmCV

sig.dist <- distance.fraction(ls$sol, ls$dists.proj, 
                              matrix( as.vector(partitions$cost), nrow=n.subjects^2, ncol=n.partitions^2) )

partition.plot( sig.dist[1:3, ], all$data[[1]], grid.partition)

ls.cv <- partition.least.squares.cv(partitions, labels)




#compare to just using mass in each partition
trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 20, classProbs = TRUE, summaryFunction = twoClassSummary)

V <- matrix(NA, nrow=n.subjects, ncol=n.partitions)
R <- matrix(NA, nrow=n.subjects, ncol=n.partitions)

for(i in 1:n.subjects){
  X <- all$data[[i]]
  X.p <- grid.partition( X[,1:4] )
  for(j in 1:max(X.p) ){
    w <- which(X.p == j)
    V[i,j] =  sum( X.p[w] * X$v[w] ) / sum( X.p[w] ) 
    R[i,j] =  sum( X.p[w] * X$r[w] ) / sum( X.p[w] ) 
  }
}
 V[is.na(V)] = 0
 R[is.na(R)] = 0
data1 = data.frame( prcomp(cbind(V, R))$x, gender = as.factor(labels[,2]) )
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


