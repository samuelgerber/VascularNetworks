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


