
load.mulitresolution.transport.distances <- function( folder, n , nscales){

  distances = list()
  for( i in 1:nscales){
    distances[[i]] = matrix(0, nrow=n, ncol=n)
  }
  for( i in 1:(n-1) ){
    for( j in (i+1):n){
      print( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      load( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      map = mtrp$trp$map[[ length( mtrp$trp$cost ) ]]
      for(s in 1:nscales){
        d = sqrt( sum(mtrp$delta[[s]][ , .( x^2+y^2+z^2+r^2) ] * mtrp$mass) )
        distances[[s]][i, j] = d
        distances[[s]][j, i] = d
      }
    }
  }
  distances
}


load.mulitresolution.v2.transport.distances <- function( folder, n , nscales){

  distances = list()
  for( i in 1:nscales){
    distances[[i]] = matrix(0, nrow=n, ncol=n)
  }
  for( i in 1:(n-1) ){
    for( j in (i+1):n){
      print( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      load( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      map = mtrp$trp$map[[ length( mtrp$trp$cost ) ]]
      for(s in 1:nscales){
        if( i == nscales){
          d = mtrp$trp$cost[ length( mtrp$trp$cost ) ]
        }
        else{
          d =  mtrp$trp1[[i]]$cost[ length( mtrp$trp1[[i]]$cost ) ] +
               mtrp$trp2[[i]]$cost[ length( mtrp$trp2[[i]]$cost ) ]
        }
        distances[[s]][i, j] = d
        distances[[s]][j, i] = d
      }
    }
  }
  distances
}


