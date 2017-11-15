smooth <- function(X, k=20){
  library(RANN)
  x = nn2(X[,1:3], k=k)
  Xnew = 0
  for(i in 1:ncol(x$nn.idx) ){ Xnew = Xnew + X[x$nn.idx[,i], ] }
  Xnew / k
}

subsample <- function(X, eps=0.25){
  library(gmra)
  gmra1 = gmra.create.ikm(X=X, eps=eps, nKids=8, stop=3)
  gmra.centers(gmra1, 100)
}
