smooth.plain <- function(X, k=20){
  library(RANN)
  x = nn2(X, k=k)
  Xnew = 0
  for(i in 1:ncol(x$nn.idx) ){ Xnew = Xnew + X[x$nn.idx[,i], ] }
  Xnew / k
}




smooth.weighted <- function(X, volume, sigma = 5, k=4*sigma){
  library(RANN)
  print(summary(X))
  nn = nn2(X, k=k)
  w = exp( -nn$nn.dists / (2*sigma*sigma) )
  wSum = 0
  Xnew = 0
  for(i in 1:ncol(nn$nn.idx) ){ 
    wtmp = w[,i] * volume[nn$nn.idx[,i]] 
    wSum = wSum + wtmp 
    Xnew = Xnew + X[nn$nn.idx[,i], ] * wtmp}
  colnames(Xnew) <- colnames(X)
  sweep(Xnew, 1, wSum, "/") 
}




subsample <- function(X, eps=0.25){
  library(gmra)
  gmra1 = gmra.create.ikm(X=X, eps=eps, nKids=8, stop=3)
  Xc = gmra.centers(gmra1, 100)
}




multiresolution.gmra <- function(X, n=8){
  library(gmra)
  library(data.table)


  mres <- list( gmra = list(), X = list(), index = list() )
  layout( matrix(1:8, 2, 4) )
  for( i in 1:n){
    X <-  as.data.table(X)
    mres$gmra[[i]] <-  gmra.create.ikm( X[, .(x, y, z, r)], nKids=4, eps=0.3, stop=3)
    Xnew <- gmra.centers( mres$gmra[[i]], 1000 )[, 1:3]
    index <- gmra.partition(mres$gmra[[i]], 1000)
    
    sizes <- sapply(index, length)

    X$partition = rep(0, nrow(X) )
    X$partition[ unlist(index) ] = rep(1:length(sizes), sizes)

     
    Y = X[, .( r = sum(r*v), l = sum(l), v = sum(v)), by=partition]
    Y$r = Y$r / Y$v
    Y = Y[order(partition)] 
    Y$x = Xnew[,1]
    Y$y = Xnew[,2]
    Y$z = Xnew[,3]
   
    mres$X[[i]]     = Y
    mres$index[[i]] = index

    symbols( Y$x, Y$y, circles=Y$r, inches=0.02, bg="#00000070", 
             fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
      
    if( i<n){
      X <- smooth.weighted(Y[ ,c("x", "y", "z", "r")], Y$v, 2^i, k=40)
      X = cbind(X, Y[, l, v ] )
    }

  }
  mres
}




multiresolution.transport <- function( mres1, mres2 ){
   library(mop)

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
   ntrp <- length(trp$map)
   map <- trp$map[[ ntrp ]] 
   
   from = rep(0, length(trp$fromSize[[ ntrp ]] ) )
   to = rep(0, length(trp$toSize[[ ntrp ]] ) )
   from[  rep( 1:length( trp$fromSize[[ntrp]] ) , trp$fromSize[[ntrp]] ) ] = trp$fromIndex[[ ntrp ]]
   from = from[ map[,1] ] 
   to[    rep( 1:length( trp$toSize[[ntrp]] )   , trp$toSize[[ntrp]] ) ]   = trp$toIndex[[ ntrp ]]
   to = to[ map[,2] ]

   delta <- vector("list", n)
   
   forward1 = 1:length( trp$fromIndex[[ntrp]] )
   forward2 = 1:length( trp$toIndex[[ntrp]] )

   for( i in 1:n ){
     print( i )

     partition = rep( 1:length(mres1$index[[i]]), sapply(mres1$index[[i]], length) )
     partition[ unlist( mres1$index[[i]] ) ] = partition
     forward1  = partition[ forward1 ]
     
     partition = rep( 1:length(mres2$index[[i]]), sapply(mres2$index[[i]], length) )
     partition[ unlist( mres2$index[[i]] ) ] = partition
     forward2  = partition[ forward2 ]

     #tmp = forward1
     #for( j in 1:length( partition ) ){
     #  forward1[ which( tmp %in% partition[[j]] ) ] = j
     #}
     
     #partition = mres2$index[[i]]
     #tmp = forward2
     #for( j in 1:length( partition ) ){
     #  forward2[ which( tmp %in% partition[[j]] ) ] = j
     #}

     if( i == 1){
        X1 = mres1$X[[i]][ forward1[ from ],   ]
        X2 = mres2$X[[i]][ forward2[ to ],   ]
     }
     delta[[i]] = mres2$X[[i]][ forward2[ to ], ] - 
                  mres1$X[[i]][ forward1[ from ],   ]

     print( summary( delta[[i]] ) )
   }
   for( i in 2:n){
     delta[[i-1]] = delta[[i-1]] - delta[[i]]
   }

   list( delta = delta, trp = trp, from = X1, to=X2 )
}


multiresolution.transport.interpolate <- function( mtrp, t ){
  ntrp = length(mtrp$trp$from) 
  X    = mtrp$from
  for(i in 1:length(mtrp$delta) ){
    X = X + t * mtrp$delta[[ i ]]
  }
  X
}


multiresolution.plot.interpolation.2d <- function(mtrp, radius.scaling ){
  index = 0
  for( t in c( seq(0, 1, by=0.01), seq(1, 0, by=-0.01) ) ){ 
    X = multiresolution.transport.interpolate(mtrp, t=t)
    symbols( X$x, X$y, circles=X$r*radius.scaling, inches=FALSE, bg="#00000010", 
             fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
    dev.copy(png, sprintf( "mip-%.5d.png", index) )
    dev.off()
    index = index +1
  }

}


