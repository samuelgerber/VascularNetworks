
partition.plot <- function(p.index, X1, partition.function){

par( mar = c(1,1,1,1) )
par( mfrow = c(nrow(p.index),4) )

  xr = range(X1$x)
  yr = range(X1$y)
  zr = range(X1$z)
  rr = range(X1$r)

  X = expand.grid(seq(xr[1], xr[2], length.out=23),
                  seq(yr[1], yr[2], length.out=23),
                  seq(zr[1], zr[2], length.out=23),
                  seq(rr[1], rr[2], length.out=23)
                  )
  X.p <- partition.function(X[,1:4])

for(i in 1:nrow(p.index) ){
  p.ind <- p.index[i, 2:3]

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


  symbols( X1[, c(1,4)], circles=X1[,4]*1.5,, inches=FALSE, bg="#00000010", 
        fg="#00000000", bty="n", xaxt="n", yaxt="n", xlim=yr, ylim=rr)

  if(length(ind1) > 0 ){
    symbols( X[ind1,c(1,4)], circles=X[ind1,4]*2.5, inches=FALSE, bg="#0097c340", 
        fg="#00000000",  add=TRUE)
  }
  if(length(ind2) > 0 ){
    symbols( X[ind2, c(1,4)], circles=X[ind2,4]*2.5, inches=FALSE, bg="#E17D1740", 
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





##
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




partition.plot.3d.weighted.slice.fm <- function( sig.index, 
                                              X1, 
                                              partition.function, 
                                              labels, 
                                              partitions,
                                              sol,
                                              X){
  
  xr = range(X1$x)
  yr = range(X1$y)
  zr = range(X1$z)
  ranges <- rbind(xr,yr,zr)
  plot3d.slices(X1)


  dir.total = 0
  dir.mm = 0
  dir.ff = 0
  col1 <- rgb( 0.01, 0.59, 0.76 )
  col2 <- rgb( 0.88, 0.49, 0.09 )
  n.mm = 0
  n.ff = 0
  n.fm = 0
  mass.fm <- rep(0, dim(partitions$mass)[3])
  for(i in 1:nrow(sig.index) ){
    from <- as.integer(sig.index[i,2])
    to <- as.integer(sig.index[i,3])
    coeff<- sol[from, to]
    sig <- as.double(sig.index[i, 1])
    #x.from <- as.vector(t(X) %*% X.p[[ from ]] / sum(X.p[[ from ]]))
    #x.to <- as.vector(t(X) %*% X.p[[ to ]] / sum(X.p[[ to ]]))
    dir <- 0
    n = 0
    for(k1 in 1:dim(partitions$deltas)[1]){
      for(k2 in 1:dim(partitions$deltas)[2]){
        tmp1 <- c( partitions$deltas[ k1,k2, from,   to, ], 1) * coeff * 
                partitions$mass[ k1,k2, from,   to ]
        tmp2 <- c(partitions$deltas[ k2,k1,   to, from, ], -1) * coeff *
                partitions$mass[ k1,k2, from,   to ]
        if(labels[k1,2] != labels[k2,2] ){
           n.fm = n.fm + 1
           n = n =1
           if(labels[k1, 2] == "M"){
             dir = dir + tmp1 - tmp2
           }
           else{
             dir = dir - tmp1 + tmp2
           }
        }
        else if(labels[k1, 2] == "M"){
          n.mm = n.mm + 1
          dir.mm = dir.mm + (tmp1-tmp2) 
        }
        else{
          n.ff = n.ff + 1
          dir.ff = dir.ff + (tmp1-tmp2) 
        }
      }
    }
    mass.fm[from] = mass.fm[from] + sign(dir[5]) * sqrt(abs(dir[5])) * sig
    mass.fm[to] = mass.fm[to] - sign(dir[5]) * sqrt(abs(dir[5])) * sig

    dir.total = dir.total + dir 
    #a = min(0.75, max(0.3, 20*as.double(sig.index[i,1]) ) ) #dir[5] * 1000) )
    #xf <- x.from[1:3] + runif(3) * 10 - 5
    #xt <- x.to[1:3] + runif(3) * 10 - 5
    #segments3d( rbind(xf, xt), col=col1, lwd=3, alpha=a) 
    #spheres3d(xf , alpha=a, col=col1, radius=4)
    #spheres3d(xt , alpha=a, col=col2, radius=4)
  }

  x.m <- matrix(0, nrow=length(mass.fm), ncol=3);
  for( n in 1:length(X) ){
    print(n)
    X.p <- partition.function(X[[n]][,1:4])
    for(i in 1:length(X.p) ){
      ind <- which(X.p[[i]] > 0 )
      x <- as.vector(t(X[[n]][ind, 1:3]) %*% X.p[[ i ]][ind] / sum(X.p[[ i ]][ind]))
      x.m[i, ] <- x.m[i, ] + x
    }
  }
  x.m = x.m / length(X)

  #a = min(0.75, max(0.3, 20*as.double(sig.index[i,1]) ) ) #dir[5] * 1000) )
  for( i in 1:length(mass.fm) ){

    if( mass.fm[i] < 0){
      spheres3d(x.m[i, ], col=col1, radius= -2000*mass.fm[i], alpha=0.7)
    }
    else{
      spheres3d(x.m[i, ], col=col2, radius=2000*mass.fm[i], alpha=0.7)
    }
  }

  print(sum(mass.fm))
  list(dir.total/n.fm, dir.ff/n.ff, dir.mm/n.mm, mass.fm)
}







partition.plot.3d.slice.fm <- function( sig.index, 
                                              X1, 
                                              partition.function, 
                                              labels, 
                                              partitions,
                                              sol,
                                              X){
  
  xr = range(X1$x)
  yr = range(X1$y)
  zr = range(X1$z)
  ranges <- rbind(xr,yr,zr)
  plot3d.slices(X1)


  dir.total = 0
  dir.mm = 0
  dir.ff = 0
  col1 <- rgb( 0.01, 0.59, 0.76 )
  col2 <- rgb( 0.88, 0.49, 0.09 )
  n.mm = 0
  n.ff = 0
  n.fm = 0
  mass.fm <- rep(0, dim(partitions$mass)[3])
  for(i in 1:nrow(sig.index) ){
    from <- as.integer(sig.index[i,2])
    to <- as.integer(sig.index[i,3])
    coeff<- sol[from, to]
    sig <- as.double(sig.index[i, 1])
    #x.from <- as.vector(t(X) %*% X.p[[ from ]] / sum(X.p[[ from ]]))
    #x.to <- as.vector(t(X) %*% X.p[[ to ]] / sum(X.p[[ to ]]))
    dir <- 0
    n = 0
    for(k1 in 1:dim(partitions$deltas)[1]){
      for(k2 in 1:dim(partitions$deltas)[2]){
        tmp1 <- c( partitions$deltas[ k1,k2, from,   to, ], 1) * coeff * 
                partitions$mass[ k1,k2, from,   to ]
        tmp2 <- c(partitions$deltas[ k2,k1,   to, from, ], -1) * coeff *
                partitions$mass[ k1,k2, from,   to ]
        if(labels[k1,2] != labels[k2,2] ){
           n.fm = n.fm + 1
           n = n =1
           if(labels[k1, 2] == "M"){
             dir = dir + tmp1 - tmp2
           }
           else{
             dir = dir - tmp1 + tmp2
           }
        }
        else if(labels[k1, 2] == "M"){
          n.mm = n.mm + 1
          dir.mm = dir.mm + (tmp1-tmp2) 
        }
        else{
          n.ff = n.ff + 1
          dir.ff = dir.ff + (tmp1-tmp2) 
        }
      }
    }
    mass.fm[from] = mass.fm[from] + sign(dir[5]) * sqrt(abs(dir[5])) * sig
    mass.fm[to] = mass.fm[to] - sign(dir[5]) * sqrt(abs(dir[5])) * sig

    dir.total = dir.total + dir 
    #a = min(0.75, max(0.3, 20*as.double(sig.index[i,1]) ) ) #dir[5] * 1000) )
    #xf <- x.from[1:3] + runif(3) * 10 - 5
    #xt <- x.to[1:3] + runif(3) * 10 - 5
    #segments3d( rbind(xf, xt), col=col1, lwd=3, alpha=a) 
    #spheres3d(xf , alpha=a, col=col1, radius=4)
    #spheres3d(xt , alpha=a, col=col2, radius=4)
  }

  x.m <- matrix(0, nrow=length(mass.fm), ncol=3)
  n.x <- rep(0, length(mass.fm) )
  for( n in 1:length(X) ){
    print(n)
    X.p <- partition.function(X[[n]][,1:4])
    for(i in 1:max(X.p) ){
      ind <- which(X.p[[i]] == i )
      if(length(ind) > 0 ){
        x <- colMeans( X[[n]][ind, 1:3] ) 
        x.m[i, ] <- x.m[i, ] + x
        n.x[i] = n.x[i] + 1
      }
    }
  }
  for(i in 1:nrow(x.m) ){
    x.m[i, ] = x.m[i, ] / n.x[i]
  }
  print(summary(x.m) )
  #a = min(0.75, max(0.3, 20*as.double(sig.index[i,1]) ) ) #dir[5] * 1000) )
  scale = max(abs(mass.fm) )
  print(scale)
  for( i in 1:length(mass.fm) ){

    if( mass.fm[i] < 0){
      spheres3d(x.m[i, ], col=col1, radius= -20 * mass.fm[i] / scale, alpha=0.7)
    }
    else{
      spheres3d(x.m[i, ], col=col2, radius = 20*mass.fm[i]/scale, alpha=0.7)
    }
  }
  list(dir.total/n.fm, dir.ff/n.ff, dir.mm/n.mm, mass.fm)
}




plot3d.slices <- function(X1, col="#881111", alpha=1){

  library(rgl) 

  xr = c(0, 352) + 53
  yr = c(0, 422) + 12 
  zr = c(0, 226) - 55

  library(rgl)
  plot3d( X1[ ,1:3], type="s", radius=X1[ , 4], col=col, alpha=alpha,
           box=FALSE, axes=TRUE,  xlab="", ylab="", zlab="" )

 zmat <- matrix(mean(zr),2,2)
 ymat <- matrix(mean(yr),2,2)
 xmat <- matrix(mean(xr),2,2)  

 g.xy <- expand.grid(xr[1:2], yr[1:2])
 g.xz <- expand.grid(xr[1:2], zr[2:1])
 g.yz <- expand.grid(yr[1:2], zr[2:1])

  persp3d(matrix(g.xy[,1], 2,2) , matrix(g.xy[,2], 2, 2), zmat,
          lit=F,fog=T,color="white",textype="rgb",texture="BW-z.png",add=T, alpha=1)
          #lit=F,fog=T,color="white",textype="rgb",texture="BW-x2.png",add=T, alpha=1)

  
  persp3d(matrix(g.xz[,1], 2,2) , ymat, matrix(g.xz[,2], 2, 2),
          lit=F,fog=T,color="white",textype="rgb",texture="BW-y.png",add=T, alpha=1)
          #lit=F,fog=T,color="white",textype="rgb",texture="BW-y2.png",add=T, alpha=1)

  
  persp3d(xmat, matrix(g.yz[,1], 2,2), matrix(g.yz[,2], 2, 2),
                lit=F,fog=T,color="white",textype="rgb",texture="BW-x.png",add=T, alpha=1)
                #lit=F,fog=T,color="white",textype="rgb",texture="BW-z2.png",add=T, alpha=1)

}

plot3d.vascular <- function(X1, col="#881111", alpha=1, add=FALSE){
  library(rgl)
  plot3d( X1[ ,1:3], type="s", radius=X1[ , 4], col=col, alpha=alpha,
           box=FALSE, axes=FALSE,  xlab="", ylab="", zlab="", add=add )


}
