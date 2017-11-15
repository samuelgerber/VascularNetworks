load("../../Data/all.trees.Rdata")



for( i in 1:length( all$data ) ){
  print(i) 

  tree <- all$data[[i]]
  ndata = c()
  ids = unique(tree$id)
  for( id in ids){
    index = tree$id == id
    if(sum(index) > 1){
      
    X = tree[index, ]
    n = nrow(X)
    l = X[1:(n-1), 1:3] - X[2:n, 1:3]
    l = sqrt( rowSums( l^2 ) )
    cl = cumsum(l)
    A = (X[1:(n-1), 1:4] + X[2:n, 1:4]) / 2
    v = l * pi * A[,4]^2
    tmp1 = aggregate(. ~ cut(cl, max(2, cl / 2 ) ), A, mean)[,2:5]
    tmp2 = aggregate(. ~ cut(cl, max(2, cl / 2 ) ), data.frame(l,v), sum )[,2:3]
    ndata = rbind(ndata,   cbind(tmp1, tmp2, id) )

    }
  }
  colnames(ndata)  <-  c("x", "y", "z", "r", "l", "v", "id")
  all$data[[i]] <- data.frame( ndata )


}
