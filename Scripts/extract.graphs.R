load("../Data/Bullit01/all.trees.Rdata")

library( igraph )
library( RANN )


#graphs <- list()
seeds <- list()
graph.edges <- list()

for( i in 1:length( all$data ) ){
  print( sprintf("%d from %d", i, length(all$data) ) ) 

  tree <- all$data[[i]]
  ndata = c()
  ids = unique(tree$id)
  edges = c()
  for( id in ids){
    index = which( tree$id == id )
    if(sum(index) > 1){
      e <- rep(index, each=2)
      edges = c(edges, e[2:(length(e)-1)] )
    }
    X = tree[index[c(1, length(index) )], 1:3]
    Y = tree[-index, 1:3]
    nnindex = 1:nrow(tree)
    nnindex = nnindex[-index]
    knn <- nn2(Y, X, searchtype="radius", eps=0, radius=6, k=1)
    #if( knn$nn.dists[1,1] <  knn$nn.dists[2,1] ){
       if( knn$nn.idx[1,1] > 0 ){
         edges =c(edges, c(index[1], nnindex[ knn$nn.idx[1,1] ] ) )
       }
    #}
    #else 
    if( knn$nn.idx[2,1] > 0 ){
       edges =c(edges, c(index[length(index)], nnindex[knn$nn.idx[2,1]] ) )
    }
  }
  #g = graph( edges, directed=FALSE)
  #from = edges[ seq(1, length(edges), by=2) ]
  #to = edges[ seq(2, length(edges), by=2) ]
  #plot(tree[,1:2], pty=".", cex=0.01 )
  #segments(tree[from,1], tree[from,2], tree[to,1], tree[to,2])

  id =c()
  seedpoints = c()
  for( k in 1:4 ){
    if(length(id) > 0 ){
      s1 = which.max( tree[-id,4] )
      index = 1:nrow(tree)
      index= index[-id]
      s1 = index[s1]
    }
    else{
      s1 = which.max( tree[ ,4] )
    }
    id = c(id, which( tree$id[s1] == tree$id ) )
    seedpoints = c(seedpoints, s1)
  }

  graph.edges[[i]] = edges
  seeds[[i]] = seedpoints

}

save(graph.edges, seeds, file="graphs.Rdata")
