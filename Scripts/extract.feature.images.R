#load("./graphs.Rdata")
load("../Data/Bullit01/all.trees.Rdata")
load("./features.sp.dists.Rdata")

library( igraph )
library( RANN )
library( mmand )
#library( data.table )

xr <- c()
yr <- c()
zr <- c()

for(tree in all$data){
  xr <- range( c(tree[,1], xr) )
  yr <- range( c(tree[,2], yr) )
  zr <- range( c(tree[,3], zr) )
}


images <- list()
for( i in 1:length(features) ){
  print(i)

  im <- array( 0, dim = c(xr[2] - xr[1] + 1, yr[2]-yr[1] +1, zr[2]-zr[1] +1 ) )
  
  f <- features[[i]]
  tree = all$data[[i]]

  index <- which( f != Inf )
  tree[,1] = as.integer( tree[,1] - xr[1] + 1 )
  tree[,2] = as.integer( tree[,2] - yr[1] + 1 )
  tree[,3] = as.integer( tree[,3] - zr[1] + 1 )
  x = arrayInd(1:length(im), dim(im) )
  knn <- nn2(tree[index, 1:3], x, k=1)
  im[x] = f[index][knn$nn.idx[,1] ] 
  #im[ as.matrix(tree[index,1:3]) ] = f[index]

  #tmp = im
  #for(j in 1:10 ){
  #   print( j )
  #   tmp = gaussianSmooth(tmp, rep(5, 3) )
  #   tmp[im != 0] = im[ im!= 0]
  #}
  images[[i]] = im 

  #browser()
}

d <- c(xr[2] - xr[1] + 1, yr[2]-yr[1] +1, zr[2]-zr[1] +1 ) 

X <- c()

bimages <- list()
for(i in 1:length(images) ){
   print(i)
   bimages[[i]] = gaussianSmooth(images[[i]], 3 )
   X = rbind(X, as.vector(bimages[[i]]) )
}

pca <- prcomp(X)

v <- array( pca$rotation[,1], dim = d )
image(v[,,50])
