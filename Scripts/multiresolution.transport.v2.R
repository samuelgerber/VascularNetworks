# Multiresolution transport version 2
# Transport across scales within each subject
# transport across subjects at coarsest scale

multiresolution.transport.v2 <- function( mres1, mres2, trp.lp = NULL ){
   library(mop)

   #1. Compute transport map a finest scale
   if( is.null(trp.lp) ){
     trp.lp <- multiscale.transport.create.lp( 26 )
     icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
     multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
     multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )
   }


   n <- length(mres1$gmra)
   mtrp <- list( trp1 <- list(), trp2 <- list() )
   for(i in 2:n ){
     if( i==2 ){
       w1 = mres1$Xin$v
       w2 = mres1$X[[i-1]]$v
     }
     else{
       w1 = mres1$X[[i-2]]$v
       w2 = mres1$X[[i-1]]$v
     }
     w1 = w1 /sum( w1 )
     w2 = w2 /sum( w2 )
     mtrp$trp1[[i-1]] <- multiscale.transport.solve( 
                                      trp.lp, 
                                      mres1$gmra[[i-1]], 
                                      mres1$gmra[[i]],
                                      w1 = w1,
                                      w2 = w2, 
                                      p = 2, 
                                      nType = 0,
                                      dType = 1
                                    )

     if( i==2 ){
       w1 = mres2$Xin$v
       w2 = mres2$X[[i-1]]$v
     }
     else{
       w1 = mres2$X[[i-2]]$v
       w2 = mres2$X[[i-1]]$v
     }
     w1 = w1 /sum( w1 )
     w2 = w2 /sum( w2 )
     mtrp$trp2[[i-1]] <- multiscale.transport.solve( 
                                      trp.lp, 
                                      mres2$gmra[[i-1]], 
                                      mres2$gmra[[i]],
                                      w1 = w1,
                                      w2 = w2, 
                                      p = 2, 
                                      nType = 0,
                                      dType = 1
                                    )
   }

   mtrp$trp <- multiscale.transport.solve( 
                                      trp.lp, 
                                      mres1$gmra[[n]], 
                                      mres2$gmra[[n]],
                                      w1 = mres1$X[[n-1]]$v,
                                      w2 = mres2$X[[n-1]]$v, 
                                      p = 2, 
                                      nType = 0,
                                      dType = 1
                                    )

  mtrp
   
}



multiscale.transport.v2.delta <- function (mst, index, t) {
    from = mst$from[[index]]
    to = mst$to[[index]]
    map = mst$map[[index]]
    to[map[, 2], ] - from[map[, 1], ] 
}




multiresolution.transport.v2.plot.interpolation.2d <- function( mtrp, radius.scaling ){
  
  index <- 0
  n <- length(mtrp$trp1)
  for( i in 1:n ){
    trp = mtrp$trp1[[i]]
    delta <- multiscale.transport.delta( trp, length( trp$cost), t=1)[,1:3]
    step = 5 / sqrt( max(rowSums( delta*delta) ) )
    print(step)

    npoints = length(mtrp$trp1[[1]]$fromSize[[ length(mtrp$trp1[[1]]$fromSize) ]] )
    npoints2 = length(mtrp$trp1[[i]]$fromSize[[ length(mtrp$trp1[[i]]$fromSize) ]] )
    col = rgb(0,0,0, min(1, 0.075 * sqrt( npoints/npoints2 ) ) )
    for( t in seq(0, 1, length.out=1/step)  ){ 
      X = multiscale.transport.interpolate( trp, length(trp$cost), t=t )
      symbols( X$X, circles=X$X[,4] * radius.scaling, inches=FALSE, bg=col, 
               fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
      dev.copy( png, sprintf( "mv2ip-%.5d.png", index) )
      dev.off()
      index = index +1
    }
  
  
  }


  trp = mtrp$trp
  delta <- multiscale.transport.v2.delta( trp, length( trp$cost), t=1)[,1:3]
  step = 5 / sqrt( max(rowSums( delta*delta) ) )
  print(step)

  npoints = length(mtrp$trp1[[1]]$fromSize[[ length(mtrp$trp1[[1]]$fromSize) ]] )
  npoints2 = length(mtrp$trp$fromSize[[ length(mtrp$trp$fromSize) ]] )
  col = rgb(0,0,0, min(1, 0.075 * sqrt( npoints/npoints2 ) ) )

  for( t in seq(0, 1, length.out=1/step)  ){ 
    X = multiscale.transport.interpolate( trp, length(trp$cost), t=t )
    symbols( X$X, circles=X$X[,4] * radius.scaling, inches=FALSE, bg=col, 
               fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
    dev.copy( png, sprintf( "mv2ip-%.5d.png", index) )
    dev.off()
    index = index +1
  }



  for( i in n:1 ){

    trp = mtrp$trp2[[i]]
    delta <- multiscale.transport.delta( trp, length( trp$cost), t=1)[,1:3]
    step = 5 / sqrt( max(rowSums( delta*delta) ) )
    print(step)

    npoints = length(mtrp$trp2[[1]]$fromSize[[ length(mtrp$trp2[[1]]$fromSize) ]] )
    npoints2 = length(mtrp$trp2[[i]]$fromSize[[ length(mtrp$trp2[[i]]$fromSize) ]] )
    col = rgb(0,0,0, min(1, 0.075 * sqrt( npoints/npoints2 ) ) )

    for( t in seq(1, 0, length.out= 1/step)  ){ 
      X = multiscale.transport.interpolate( trp, length(trp$cost), t=t )
      symbols( X$X, circles=X$X[,4] * radius.scaling, inches=FALSE, bg=col, 
               fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
      dev.copy( png, sprintf( "mv2ip-%.5d.png", index) )
      dev.off()
      index = index +1
    }
  }
  

}

