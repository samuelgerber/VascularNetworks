
partition.plot <- function(p.index, X1, partition.function){

par( mar = c(1,1,1,1) )
par( mfrow = c(nrow(p.index),3) )


for(i in 1:nrow(p.index) ){
  p.ind <- p.index[i, 2:3]


  xr = range(X1$x)
  yr = range(X1$y)
  zr = range(X1$z)

  X = expand.grid(seq(xr[1], xr[2], length.out=23),
                seq(yr[1], yr[2], length.out=23),
                seq(zr[1], zr[2], length.out=23)
                )
  X[,4] = 4
  X.p <- partition.function(X[,1:4])
  elem1 = is.element(X.p, t(p.ind[ ,1]))
  elem2 = is.element(X.p, t(p.ind[ ,2]))
  ind0 <- which( !(elem1 | elem2) )
  ind1 <- which( elem1 )
  ind2 <- which( elem2  )
  X[ind0, 4] = 0
  symbols( X1[, 1:2], circles=X1[,4]*1.5, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xaxt="n", yaxt="n", xlim=xr, ylim=yr)

  if(length(ind1) > 0 ){
     symbols( X[ind1, 1:2], circles=X[ind1,4]*1.5, inches=FALSE, bg="#0097c340", 
        fg="#00000000", add=TRUE)
  }
  if(length(ind2) > 0 ){
    symbols( X[ind2, 1:2], circles=X[ind2,4]*1.5, inches=FALSE, bg="#E17D1740", 
        fg="#00000000",  add=TRUE)
  }

  symbols( X1[, c(1,3)], circles=X1[,4]*1.5,, inches=FALSE, bg="#00000010", 
         fg="#00000000", bty="n", xaxt="n", yaxt="n", xlim=xr, ylim=zr)

  if(length(ind1) > 0 ){
    symbols( X[ind1, c(1,3)], circles=X[ind1,4]*1.5, inches=FALSE, bg="#0097c340", 
        fg="#00000000", add=TRUE)
  }
  if(length(ind2) > 0 ){
    symbols( X[ind2, c(1,3)], circles=X[ind2,4]*1.5, inches=FALSE, bg="#E17D1740", 
        fg="#00000000", add=TRUE)
  }

  symbols( X1[, 2:3], circles=X1[,4]*1.5,, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xaxt="n", yaxt="n", xlim=yr, ylim=zr)

  if(length(ind1) > 0 ){
    symbols( X[ind1, 2:3], circles=X[ind1,4]*1.5, inches=FALSE, bg="#0097c340", 
        fg="#00000000",  add=TRUE)
  }
  if(length(ind2) > 0 ){
    symbols( X[ind2, 2:3], circles=X[ind2,4]*1.5, inches=FALSE, bg="#E17D1740", 
        fg="#00000000", add=TRUE)
  }

}


#library(rgl)
#plot3d(X1[ ,1:3], alpha=0.5, type="s", radius=X1[,4], col="#551A33",
#        box=TRUE, axes=TRUE,  xlab="", ylab="", zlab="" )
#
#plot3d(X[ind2,1:3], alpha=0.5, type="s", radius=X[ind2,4], col="#E17D17", add=T )
#
#plot3d(X[ind0,1:3], alpha=0.15,  type="s", radius=X[ind0,4], col="#000000", add=T)

}








partition.plot.weighted <- function(p.index, X1, partition.function){

  par( mar = c(1,1,1,1) )
  par( mfrow = c(nrow(p.index),3) )

  xr = range(X1$x)
  yr = range(X1$y)
  zr = range(X1$z)

  X = expand.grid(seq(xr[1], xr[2], length.out=23),
                  seq(yr[1], yr[2], length.out=23),
                  seq(zr[1], zr[2], length.out=23)
                  )
  X[,4] = 4
  X.p <- partition.function(X[,1:4])

  for(i in 1:nrow(p.index) ){
    p.ind <- p.index[i, 2:3]



    col1 <- rgb( 0.00, 0.59, 0.76, 0.1*X.p[[ as.integer(p.ind[1,1]) ]] )
    col2 <- rgb( 0.88, 0.49, 0.09, 0.1*X.p[[ as.integer(p.ind[1,2]) ]] )


    symbols( X1[, 1:2], circles=X1[,4]*1.5, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xlab="x", ylab="y", xaxt="n", yaxt="n", xlim=xr, ylim=yr)
    symbols( X[, 1:2], circles=X[,4]*1.5, inches=FALSE, bg=col1, fg="#00000000", add=TRUE)
    symbols( X[, 1:2], circles=X[,4]*1.5, inches=FALSE, bg=col2, fg="#00000000",  add=TRUE)

    symbols( X1[, c(1,3)], circles=X1[,4]*1.5,, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xlab="x", ylab="z", xaxt="n", yaxt="n", xlim=xr, ylim=zr)
    symbols( X[, c(1,3)], circles=X[,4]*1.5, inches=FALSE, bg=col1, fg="#00000000", add=TRUE)
    symbols( X[, c(1,3)], circles=X[,4]*1.5, inches=FALSE, bg=col2, fg="#00000000", add=TRUE)

    symbols( X1[, 2:3], circles=X1[,4]*1.5,, inches=FALSE, bg="#00000010",
         fg="#00000000", bty="n", xlab="x", ylab="z", xaxt="n", yaxt="n", xlim=yr, ylim=zr)
    symbols( X[, 2:3], circles=X[,4]*1.5, inches=FALSE, bg=col1, fg="#00000000", add=TRUE)
    symbols( X[, 2:3], circles=X[,4]*1.5, inches=FALSE, bg=col2, fg="#00000000",  add=TRUE)
  }

}




partition.plot.weighted.fm <- function(sig.index, X1, partition.function, lwd.factor){
  FF<- 0
  FM <- 0
  MF <- 0
  MM <- 0
  up   <- upper.tri(partitions$mass[1,2,,])
  for(i in 1:n.subjects){
    for(j in 1:n.subjects){
      if(labels[i,2] == "F"){
        if( labels[j,2] == "F"){
          FF <- FF + partitions$mass[i,j,,]
        }
        else{
          FM <- FM + partitions$mass[i,j,,]
        } 
      }
      else{
        if( labels[j,2] == "F"){
          MF <- MF + partitions$mass[i,j,,]
        }
        else{
          MM <- MM + partitions$mass[i,j,,]
        } 
      }
    }
  }


  par( mar = c(1,1,1,1) )
  par( mfrow = c(1, 3) )


  

  xr = range(X1$x)
  yr = range(X1$y)
  zr = range(X1$z)
  ranges <- rbind(xr,yr,zr)
  X = expand.grid(seq(xr[1], xr[2], length.out=23),
                  seq(yr[1], yr[2], length.out=23),
                  seq(zr[1], zr[2], length.out=23)
                  )
  X[,4] = 4
  X.p <- partition.function(X[,1:4])
   
  dims <- rbind(c(1,2), c(1,3), c(2,3))
  print(dims)
  for(d in 1:nrow(dims)){
    symbols( X1[, dims[d, ]], circles=X1[,4]*1.5, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xlim=ranges[dims[d,1], ], ylim=ranges[dims[d,2], ] )
    grid(4,4, lwd=2, col="black")
    for(i in 1:nrow(sig.index) ){

      i.s <- as.integer(sig.index[i,2])
      i.e <- as.integer(sig.index[i,3])
      if( MF[i.s, i.e] > FM[i.s, i.e] ){
        i.t = i.s
        i.s = i.e
        i.e = i.t
      }
      start <- t(X) %*% X.p[[ i.s]]  / sum( X.p[[ i.s]] )
      end <- t(X) %*% X.p[[ i.e ]] / sum( X.p[[ i.e ]] )
      arrows( start[dims[d,1]], start[dims[d,2]], end[dims[d,1]], end[dims[d,2]], 
              lwd= lwd.factor * sig.index[i,1], col = rgb( 0.88, 0.49, 0.09, 0.5) )
    }
  }

}

partition.plot.weighted.slice.fm <- function(sig.index, X1, partition.function){
  FF<- 0
  FM <- 0
  MF <- 0
  MM <- 0
  up   <- upper.tri(partitions$mass[1,2,,])
  for(i in 1:n.subjects){
    for(j in 1:n.subjects){
      if(labels[i,2] == "F"){
        if( labels[j,2] == "F"){
          FF <- FF + partitions$mass[i,j,,]
        }
        else{
          FM <- FM + partitions$mass[i,j,,]
        } 
      }
      else{
        if( labels[j,2] == "F"){
          MF <- MF + partitions$mass[i,j,,]
        }
        else{
          MM <- MM + partitions$mass[i,j,,]
        } 
      }
    }
  }



  

  xr = range(X1$x)
  yr = range(X1$y)
  zr = range(X1$z)
  ranges <- rbind(xr,yr,zr)
  X = expand.grid(seq(xr[1], xr[2], length.out=23),
                  seq(yr[1], yr[2], length.out=23),
                  mean(zr)
                  )
  Y = expand.grid(mean(xr),
                  seq(yr[1], yr[2], length.out=23),
                  seq(zr[1], zr[2], length.out=23)
                  )
  Z = expand.grid(seq(xr[1], xr[2], length.out=23),
                  mean(yr),
                  seq(zr[1], zr[2], length.out=23)
                  )

  X[,4] = 4
  Y[,4] = 4
  Z[,4] = 4
  
  
  X.p <- partition.function(X[,1:4])
  Y.p <- partition.function(Y[,1:4])
  Z.p <- partition.function(Z[,1:4])
  
  slices.p <- list(X.p, Y.p, Z.p)
  slices <- list(X, Y, Z)

  library(rgl)
  plot3d(X1[ ,1:3], alpha=1, type="s", radius=X1[,4], col="#771A13",
        box=TRUE, axes=TRUE,  xlab="", ylab="", zlab="" )
  planes3d(0,0,1, -mean(zr) ) 
  planes3d(0,1,0, -mean(yr) ) 
  planes3d(1,0,0, -mean(xr) ) 
  for(s in 1:3){
    from <- 0
    to <- 0
    for(i in 1:nrow(sig.index) ){

      i.s <- as.integer(sig.index[i,2])
      i.e <- as.integer(sig.index[i,3])
      if( MF[i.s, i.e] > FM[i.s, i.e] ){
        i.t = i.s
        i.s = i.e
        i.e = i.t
      }
      from <- from + slices.p[[s]][[i.s]]
      to <- to + slices.p[[s]][[i.t]]
    }
    from = from 
    to   = to 

    f.i = which(from > 0.001)
    t.i = which(to > 0.001)

    col1 <- rgb( 0.00, 0.59, 0.76 )
    col2 <- rgb( 0.88, 0.49, 0.09 )
    spheres3d(slices[[s]][f.i,], alpha=0.5*from[f.i], col=col1, radius=2)
    #spheres3d(slices[[s]][t.i,], alpha=0.5*to[t.i], col=col2, radius=2)
  }

}
