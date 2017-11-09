
load.transport.distances <- function( folder, n){

  distances = matrix(0, nrow=n, ncol=0);
  for( i in 1:(n-1) ){
    for( j in (i+1):n){
      load( sprintf("%s/tranpsort$.3d-%.3d.Rdata", folder, i, j) )
      distances[i, j] = trp$cost[ length( trp$cost) ]
      distances[j, i] =  distances[i, j]
    }
  }
  distances
}


load.transport.distances <- function( folder, n, ){

  distances = maytrix(0, nrow=n, ncol=0);
  for( i in 1:(n-1) ){
    for( j in (i+1):n){
      load( sprintf("%s/tranpsort$.3d-%.3d.Rdata", folder, i, j) )
      mto = unlist(lapply(trp$toRadius, mean))
      mfrom = unlist(lapply(trp$fromRadius, mean))
      m = (mfrom + mto) / 2
      distances[i, j] = trp$cost[ length( trp$cost) ]
      distances[j, i] =  distances[i, j]
    }
  }
  distances
}



