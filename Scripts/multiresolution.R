smooth.plain <- function(X, k=20){
  library(RANN)
  x = nn2(X[,1:3], k=k)
  Xnew = 0
  for(i in 1:ncol(x$nn.idx) ){ Xnew = Xnew + X[x$nn.idx[,i], ] }
  Xnew / k
}




smooth.weighted <- function(X, sigma = 5, k=4*sigma){
  library(RANN)
  x = nn2(X[,1:3], k=k)
  w = exp( -x$nn.dists / (2*sigma*sigma) )
  wSum = 0
  Xnew = 0
  for(i in 1:ncol(x$nn.idx) ){ 
    wtmp = w[,i] * X$v[x$nn.idx[,i]] 
    wSum = wSum + wtmp 
    Xnew = Xnew + X[x$nn.idx[,i], ] * wtmp}
  colnames(Xnew) <- colnames(X)
  sweep(Xnew, 1, wSum, "/") 
}




smooth.volume <- function(X, k=40){
  library(RANN)
  x = nn2(X[,1:3], k=k)
  wSum = 0
  Xnew = X
  Xnew = 0
  for(i in 1:ncol(x$nn.idx) ){ 
    wtmp = X$v[x$nn.idx[,i]] 
    wSum = wSum + wtmp 
    Xnew = Xnew + X[x$nn.idx[,i], 1:3] * wtmp}
  sweep(Xnew, 1, wSum, "/") 
  Xnew = cbind(Xnew, X[, 4:ncol(X)] )
  colnames(Xnew) <- colnames(X)
  Xnew
}




subsample <- function(X, eps=0.25){
  library(gmra)
  gmra1 = gmra.create.ikm(X=X, eps=eps, nKids=8, stop=3)
  Xc = gmra.centers(gmra1, 100)
}




multiresolution.gmra <- function(X, n=8){
  library(gmra)
  mres <- list( gmra = list(), X = list(), index = list())
  layout( matrix(1:8, 2, 4) )
  for( i in 1:n){
    mres$gmra[[i]] <-  gmra.create.ikm( X[,1:3], nKids=4, eps=0.3, stop=3)
    Xnew <- gmra.centers( mres$gmra[[i]], 1000 )[, 1:3]
    index = gmra.partition( mres$gmra[[i]], 1000)
    v = rep(0, nrow(Xnew) )
    l = rep(0, nrow(Xnew) )
    r = rep(0, nrow(Xnew) )
    for(j in 1:length(index) ){
      v[j] = sum( X$v[ index[[j]] ] )
      l[j] = sum( X$l[ index[[j]] ] )
      r[j] = sum( X$r[ index[[j]] ] * X$v[ index[[j]] ] ) / v[j]
    }
    Xnew = as.data.frame( cbind(Xnew, r, l, v) )
    colnames(Xnew) <- colnames(X)[1:6]
    mres$X[[i]] = Xnew
    mres$index[[i]] = index
    symbols( Xnew, circles=Xnew$r, inches=FALSE, bg="#00000070", 
             fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")

    X <- smooth.weighted(Xnew, 2^i, k=40)
  }
  mres
}




multiresolution.transport <- function( mres1, mres2 ){
 
   #1. Compute transport map a finest scale
   trp.lp <- multiscale.transport.create.lp( )
   icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
   multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
   multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )
   trp <- multiscale.transport.solve( 
                                      trp.lp, 
                                      mres1$gmra[[1]], 
                                      mres2$gmra[[1]], 
                                      w1 = mres1$X[[1]]$v, 
                                      w2 = mres2$X[[1]]$v,
                                      p = 2, 
                                      nType = 0,
                                      dType = 1 
                                    )
   

   
   #2. Compute decompositions
   n <- length(mres1$gmra)
   print(n)

   #Store transport vector at each scale
   map <- trp$map[[ length(trp$map) ]] 
   print( summary(map) )
   forward1 = 1:length(mres1$index[[1]] )
   forward2 = 1:length(mres2$index[[1]] )
   delta <- vector("list", n)
   for( i in 1:n ){
     print( i )
     delta[[i]] = mres2$X[[i]][ forward2[ map[ ,2] ] , ] - 
                  mres1$X[[i]][ forward1[ map[ ,1] ] , ]

     if( i < n ){
       partition = mres1$index[[i+1]]
       for( j in 1:length( partition ) ){
         forward1[ which( forward1 %in% partition[[j]] ) ] = j
       }
       partition = mres2$index[[i+1]]
       for( j in 1:length( partition ) ){
         forward2[ which( forward2 %in% partition[[j]] ) ] = j
       }
     }

     print( summary( delta[[i]] ) )
   }
   #for( i in 2:n){
   #  delta[[i-1]] = delta[[i-1]] - delta[[i]]
   #}

   list( delta = delta, trp = trp )
}


multiresolution.transport.interpolate <- function( mtrp, t, scale ){
  ntrp = length(mtrp$trp$from) 
  X    = mtrp$trp$from[[ ntrp ]]
  map  = mtrp$trp$map[[ ntrp ]] 
  
  X[ map[, 1], ] + t * mtrp$delta[[ scale ]]
}
