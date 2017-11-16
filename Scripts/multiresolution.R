smooth <- function(X, k=20){
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
  print( length(wSum) )
  sweep(Xnew, 1, wSum, "/") 
}

subsample <- function(X, eps=0.25){
  library(gmra)
  gmra1 = gmra.create.ikm(X=X, eps=eps, nKids=8, stop=3)
  Xc = gmra.centers(gmra1, 100)
  
}
