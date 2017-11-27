smooth.plain <- function(X, k=20){
  library(RANN)
  x = nn2(X, k=k)
  Xnew = 0
  for(i in 1:ncol(x$nn.idx) ){ Xnew = Xnew + X[x$nn.idx[,i], ] }
  Xnew / k
}




smooth.weighted <- function(X, volume, sigma = 5, k=4*sigma){
  library(RANN)
  nn = nn2(X, k=k)
  w = exp( -nn$nn.dists / (2*sigma*sigma) )
  wSum = 0
  Xnew = 0
  for(i in 1:ncol(nn$nn.idx) ){ 
    wtmp = w[,i] * volume[nn$nn.idx[,i]] 
    wSum = wSum + wtmp 
    Xnew = Xnew + X[nn$nn.idx[,i], ] * wtmp
  }
  colnames(Xnew) <- colnames(X)
  sweep(Xnew, 1, wSum, "/") 
}




subsample <- function(X, eps=0.25){
  library(gmra)
  gmra1 = gmra.create.ikm(X=X, eps=eps, nKids=8, stop=3)
  Xc = gmra.centers(gmra1, 100)
}




multiresolution.gmra <- function(X, n=8, show=FALSE, radius.scaling=1){
  library(gmra)
  library(data.table)

  xr = range(X$x)
  yr = range(X$y)

  mres <- list( gmra = list(), X = list(), index = list(), partition = list(), Xin=X )
  #layout( matrix(1:8, 2, 4) )
  if(show){
    par(mar = c(1,1,1,1) )
    par( mfrow = c(2,ceiling(n/2)) )
  }
  npoints = nrow(X)
  for( i in 1:n){
    X <-  as.data.table(X)
    mres$gmra[[i]] <-  gmra.create.ikm( X[, .(x, y, z, r)], nKids=4, eps=0.3, stop=3)
    Xnew <- gmra.centers( mres$gmra[[i]], 1000 )[, 1:3]
    index <- gmra.partition(mres$gmra[[i]], 1000)
    
    sizes <- sapply(index, length)

    X$partition = rep(0, nrow(X) )
    X$partition[ unlist(index) ] = rep(1:length(sizes), sizes)
    mres$partition[[i]] = X$partition
     
    Y = X[, .( r = sum(r*v), l = sum(l), v = sum(v)), by=partition]
    Y$r = Y$r / Y$v
    Y = Y[order(partition)] 
    Y$x = Xnew[,1]
    Y$y = Xnew[,2]
    Y$z = Xnew[,3]
    Y$partition = NULL

    mres$X[[i]]     = Y
    mres$index[[i]] = index

    if(show){
      col = rgb(0,0,0, min(1, 0.075 * sqrt( npoints/length(index)) ) )
      symbols( Y$x, Y$y, circles=1.5*Y$r*radius.scaling, inches=FALSE, bg=col, 
             fg="#00000000", bty="n", xlab="", ylab="", xlim=xr, ylim=yr ) 
    }
      
    if( i<n){
      X <- smooth.weighted(Y[ ,c("x", "y", "z", "r")], Y$v, 2^i, k=40)
      X = cbind(X, Y[, l, v ] )
    }

  }
  mres
}


