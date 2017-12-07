
partition.plot <- function(p.index, X1, partition.function){

par( mar = c(1,1,1,1) )
par( mfrow = c(nrow(p.index),3) )


for(i in 1:nrow(p.index) ){
  p.ind <- p.index[i, 2:3]


  xr = range(X1$x)
  yr = range(X1$y)
  zr = range(X1$z)

  X = expand.grid(seq(xr[1], xr[2], length.out=20),
                seq(yr[1], yr[2], length.out=20),
                seq(zr[1], zr[2], length.out=20)
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
#        box=TRUE, axes=TRUE,  xlab="", ylab="", zlab="", )
#
#plot3d(X[ind2,1:3], alpha=0.5, type="s", radius=X[ind2,4], col="#E17D17", add=T )
#
#plot3d(X[ind0,1:3], alpha=0.15,  type="s", radius=X[ind0,4], col="#000000", add=T)

}




partition.plot.weighted <- function(p.index, X1, partition.function){

  par( mar = c(1,1,1,1) )
  par( mfrow = c(nrow(p.index),3) )


  for(i in 1:nrow(p.index) ){
    p.ind <- p.index[i, 2:3]


    xr = range(X1$x)
    yr = range(X1$y)
    zr = range(X1$z)

    X = expand.grid(seq(xr[1], xr[2], length.out=31),
                  seq(yr[1], yr[2], length.out=31),
                  seq(zr[1], zr[2], length.out=31)
                  )
    X[,4] = 4
    X.p <- partition.function(X[,1:4])

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

