source("transport.distances.R")

load( "../Data/normal-database-tree-only.Rdata" )
load("../Data/all.trees.Rdata")

distances = load.transport.distances( 
  "../Processed/Transport01/transport-maps", 42 )

image(distances)
dev.new()

mds = cmdscale(distances, k=40, eig=TRUE)
plot( mds$eig )
dev.new()
plot( mds$points, col=as.factor( labels[,2] ) )


lm1 <- lm( labels[,1] ~ mds$points[, 1:4] )
summary(lm1)


