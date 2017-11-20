
load.transport.distances <- function( folder, n){

  distances = matrix(0, nrow=n, ncol=n)
  for( i in 1:(n-1) ){
    for( j in (i+1):n){
      print( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      load( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      distances[i, j] = trp$cost[ length( trp$cost) ]
      distances[j, i] =  distances[i, j]
    }
  }
  distances
}



