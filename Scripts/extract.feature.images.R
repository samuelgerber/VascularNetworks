load("./graphs.Rdata")
load("../Data/Bullit01/all.trees.Rdata")

library( igraph )
library( RANN )

xr <- c()
yr <- c()
zr <- c()

for(tree in all$data){
  xr <- range( c(tree[,1], xr) )
  yr <- range( c(tree[,2], yr) )
  zr <- range( c(tree[,3], zr) )
}



for( i in 1:length(features) ){
  print(i)

  im <- array( dim = c(xr[2] - xr[1] + 1, yr[2]-yr[1] +1, zr[2]-zr[1] +1 ) )
  
  f <- features[[i]]
  tree = all$data[[i]]

  index <- which( !is.na(f) )
  tree[,1] = as.integer( tree[,1] - xr[1] + 1 )
  tree[,2] = as.integer( tree[,2] - yr[1] + 1 )
  tree[,3] = as.integer( tree[,3] - zr[1] + 1 )
  im[tree[index,1], tree[index,2], tree[index,3] ] = f[index]


}
