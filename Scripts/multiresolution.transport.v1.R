# Multiresolution transport versoin 1
# Transport finest scale and decomposition that transport plan according to teh
# coarser scales

multiresolution.transport.v1 <- function( mres1, mres2, trp.lp = NULL ){
   library(mop)

   #1. Compute transport map a finest scale
   if( is.null(trp.lp)){
     trp.lp <- multiscale.transport.create.lp( 26 )
     icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
     multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
     multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )
   }
   trp <- multiscale.transport.solve( 
                                      trp.lp, 
                                      mres1$gmra[[1]], 
                                      mres2$gmra[[1]], 
                                      w1 = mres1$Xin$v, 
                                      w2 = mres2$Xin$v,
                                      p = 2, 
                                      nType = 0,
                                      dType = 1 
                                    )
   

   
   #2. Compute decompositions
   n <- length(mres1$gmra)

   #Store transport vector at each scale
   ntrp <- length(trp$map)
   map <- trp$map[[ ntrp ]] 
  
   #map transport plan indicies to finest scale data set
   # transport map indices do not match up with gmra input 
   from = rep(0, length(trp$fromSize[[ ntrp ]] ) )
   to = rep(0, length(trp$toSize[[ ntrp ]] ) )

   fromSizes = rep( 1:length( trp$fromSize[[ntrp]] ) , trp$fromSize[[ntrp]] ) 
   from[ fromSizes ] = trp$fromIndex[[ ntrp ]]
   from = from[ map[,1] ] 

   toSizes = rep( 1:length( trp$toSize[[ntrp]] )   , trp$toSize[[ntrp]] ) 
   to[ toSizes ]   = trp$toIndex[[ ntrp ]]
   to = to[ map[,2] ]

   #convert mass to  index
   #mass = map[ trp$fromIndex[[ ntrp ]][ map[,1] , 3]
   
   #map transport plan forwrad to data at each scale
   forward1 = 1:length( trp$fromIndex[[ntrp]] )
   forward2 = 1:length( trp$toIndex[[ntrp]] )
   
   delta <- vector("list", n)
   for( i in 1:n ){

     #upated forward map to current scale
     forward1  = mres1$partition[[i]][ forward1 ]
     #partition = rep( 1:length(mres2$index[[i]]), sapply(mres2$index[[i]], length) )
     #partition[ unlist( mres2$index[[i]] ) ] = partition
     forward2  = mres2$partition[[i]][ forward2 ]

     #store from and to data sets
     if( i == 1){
        X1 = mres1$X[[i]][ forward1[ from ],   ]
        X2 = mres2$X[[i]][ forward2[ to ],   ]
     }

     #store deltas
     delta[[i]] = mres2$X[[i]][ forward2[ to ], ] - 
                  mres1$X[[i]][ forward1[ from ],   ]

     #print( summary( delta[[i]] ) )
   }

   #store tarnsport plan differences at each scale
   for( i in 2:n){
     delta[[i-1]] = delta[[i-1]] - delta[[i]]
   }

   list( delta = delta, trp = trp, from = X1, to=X2, mass=map[,3] )
}








multiresolution.transport.v1.interpolate.scale.from <- function( mtrp, t, scale ){
  ntrp = length( mtrp$delta ) 
  X    = mtrp$from
  if(scale > 1){
    for(i in 1:(scale-1) ){
      X = X + mtrp$delta[[ i ]]
    }
  }
  X = X + t*mtrp$delta[[ scale ]]
  X
}


multiresolution.transport.v1.interpolate.scale.to <- function( mtrp, t, scale ){
  ntrp = length( mtrp$delta ) 
  X    = mtrp$to
  if(scale > 1){
    for(i in 1:(scale-1) ){
      X = X - mtrp$delta[[ i ]]
    }
  }
  X = X - t*mtrp$delta[[ scale ]]
  X
}



multiresolution.transport.v1.interpolate.scale.from.v2 <- function( mtrp, t){
  ntrp = length( mtrp$delta ) 
  X    = mtrp$from
  if( t<0 ){
    t = 0
  }
  for(i in 1:ntrp ){
      X = X + t^{1/sqrt(i/4)} * mtrp$delta[[ i ]]
  }
  X
}




multiresolution.transport.v1.interpolate.scale.to.v2 <- function( mtrp, t){
  ntrp = length( mtrp$delta ) 
  X    = mtrp$to
  if( t<0 ){
    t = 0
  }

  for(i in 1:ntrp ){
      X = X - t^{1/sqrt(i/4)} * mtrp$delta[[ i ]]
  }
  X
}



multiresolution.transort.v1.plot.interpolation.2d <- function(mtrp, radius.scaling ){
  index = 0

  for( s in 1:length(mtrp$delta) ){
    step = 5 / sqrt( max(mtrp$delta[[s]][, .(x^2+y^2+z^2)] ) )
    print(step)
  #step=0.05
    for( t in seq(0, 1, length.out=1/step)  ){ 
      X = multiresolution.transport.v1.interpolate.scale.from( mtrp, t=t, s)
      X$r[X$r<0] = 0
      print(summary(X$r))
      symbols( X$x, X$y, circles=X$r*radius.scaling, inches=FALSE, bg="#00000010", 
               fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
      dev.copy( png, sprintf( "mip-%.5d.png", index) )
      dev.off()
      index = index +1
    }
  }

  for( s in 1:length(mtrp$delta) ){
    step = 5 / sqrt( max(mtrp$delta[[s]][, .(x^2+y^2+z^2)] ) )
    print(step)
    for( t in seq(0, 1, length.out=1/step) ){ 
      X = multiresolution.transport.v1.interpolate.scale.to( mtrp, t=t, s)
      X$r[X$r<0] = 0
      symbols( X$x, X$y, circles=X$r*radius.scaling, inches=FALSE, bg="#00000010", 
               fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
      dev.copy( png, sprintf( "mip-%.5d.png", index) )
      dev.off()
      index = index +1
    }
  }

 # symbols( mtrp$from$x,mtrp$from$y, circles=mtrp$from$r*radius.scaling, inches=FALSE, bg="#00000010", 
  #             fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
   #   dev.copy( png, sprintf( "mip-%.5d.png", index) )
    #  dev.off()


}


