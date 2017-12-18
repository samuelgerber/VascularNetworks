source("pairwise.distances.R")
source("partition.plot.R")
source("partition.least.squares.R")

library(caret)
library(RColorBrewer)


load( "../../Data/normal-database-tree-only.Rdata" )
load("../Data/Bullit01/all.trees.processed.02.Rdata")




grid.partition <- function(X){
 
  partition <- 0
  for( i in 1:(ncol(X)-1) ){
    partition = partition + (cut(X[,i], 6, labels=FALSE)-1) * 6^(i-1)
  }
  partition + 1

}

n.subjects = 42
n.partitions = 6^3
partitions = load.transport.weighted.partitions( 
  "../../Processed/Transport06/transport-maps/", 
  n.subjects, 
  grid.partition.weight, n.partitions )


ls <- partition.least.squares.individual(partitions, labels)


source("partition.plot.R")
index = 2
X <- all$data[[index]]
X.p <- grid.partition(X[,1:4])
pal <- brewer.pal(n=9, "YlOrRd")
ramp <- colorRamp(pal)
sol <- matrix(as.vector(ls$sol), n.subjects)[index, ]
col = rgb( ramp( sol/max(sol) )/255 )
col = col[X.p]
plot3d.slices(X, col)
