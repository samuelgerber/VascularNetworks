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


multiresolution.gmra <- function(X, n=5){
  library(gmra)
  mres <- list( gmra = list() X = list(), index = list())
  layout( matrix(1:10, 2, 5) )
  for( i in 1:n){
    symbols( X, circles=X$r, inches=FALSE, bg="#00000070", 
             fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
    mres$gmra[[i]] <-  gmra.create.ikm( X[,1:3], nKids=4, eps=0.3, stop=3)
    Xnew <- gmra.centers( mres$gmra[[i]], 1000 )
    index = gmra.partition( mres$gmra[[i]], 1000)
    v = rep(0, nrow(Xnew) )
    l = rep(0, nrow(Xnew) )
    r = rep(0, nrow(Xnew) )
    for(i in 1:length(index) ){
      v[i] = sum( X$v[ index[[i]] ] )
      l[i] = sum( X$l[ index[[i]] ] )
      r[i] = sum( X$r[ index[[i]] ] * X$v[ index[[i]] ] ) / v[i]
    }
    Xnew = as.data.frame( cbind(Xnew, r, l, v) )
    colnames(Xnew) <- colnames(X)[1:6]
    mres$X[[i]] = Xnew
    mres$index[[i]] = index

    X <- smooth.weighted(Xnew, 2^i, k=40)
  }
  mres
}


multiresolution.transport <- function( mres1, mres2 ){
 
   trp.lp <- multiscale.transport.create.lp( )
   icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
   multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
   multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )
   
   n <- length(mres1$gmra)
   trp <- vector("list", n )

   w2 <- list()
   X2 <- list()
   for( i in 1:n) ){
     X2 <- gmra.centers( mres2$gmra[[i]] )
     w2 <- mres2$X$v
   }

   gmra2 <- mres2$gmra[[ n ]]
   for( i in length(mres1$gmra):1 ){
     w1 = mres1$X$v
     w2 = mres2$X$v
     trp[[i]] <- multiscale.transport.solve( trp.lp, mres1$gmra[[i], gmra2, p = 2, 
                                             nType=0, dType=1, w1=w1, w2=w2 )

     if( i > 1){
       cIndex = gmra.partition( gmra2, 100)
       map = trp[[i]]$map[[ length(trp[[i]]$cost) ]]
       for( j in (i-1):1 ){
            
       }
     }

   }
}
