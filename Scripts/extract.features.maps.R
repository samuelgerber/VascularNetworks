load("./graphs.Rdata")
load("../Data/Bullit01/all.trees.Rdata")

library( igraph )
library( RANN )


sp.distances <- list()
for( i in 1:length( graph.edges ) ){
  print(i)
  tree <- all$data[[i]]
  edges = graph.edges[[i]] 
  seedpoints = seeds[[i]]

  g = graph( edges, directed=FALSE)
  from = edges[ seq(1, length(edges), by=2) ]
  to = edges[ seq(2, length(edges), by=2) ]
  
  D <- sqrt( rowSums( (tree[from, 1:3] - tree[to, 1:3] )^2 ) )
  sp <- distances(g, to=seedpoints, weights=D)
  x <- apply(sp, 1, min)
  index = which( x != Inf )
  #x= x-min(x)
  #x = x/max(x[index] )

  sp.distances[[i]] = x

}
