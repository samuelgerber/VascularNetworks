
load.multiresolution.transport.v1.distances <- function( folder, n , nscales){
  library(data.table)
  distances = list()
  for( i in 1:nscales){
    distances[[i]] = matrix(0, nrow=n, ncol=n)
  }
  debug1 = matrix(0, nrow=n, ncol=n)
  debug2 = matrix(0, nrow=n, ncol=n)
  for( i in 1:(n-1) ){
    for( j in (i+1):n){
      print( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      load( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      map = mtrp$trp$map[[ length( mtrp$trp$cost ) ]]
      delta = 0
      for(s in nscales:1){
        #delta = delta + mtrp$delta[[s]]
        tmp = as.data.table(mtrp$delta[[s]])

        delta = delta + tmp
        d = sqrt( sum( tmp[ , .( x^2+y^2+z^2+r^2) ] * mtrp$mass) )
        distances[[s]][ i, j ] = d 
        distances[[s]][ j, i ] = d
      }
      debug1[i, j] = mtrp$trp$cost[ length( mtrp$trp$cost ) ]
      debug1[j, i] = debug1[i,j]
      d = sqrt( sum( delta[ , .( x^2+y^2+z^2+r^2) ] * mtrp$mass) )
      debug2[i, j] = d
      debug2[j, i]= d

    }
  }
  list(distances=distances, debug1=debug1, debug2=debug2)
}


load.multiresolution.transport.v2.distances <- function( folder, n , nscales){

  d.scales = matrix(0, nrow=nscales-1, ncol=42)
  distances = matrix(0, nrow=n, ncol=n)
  for( i in 1:(n-1) ){
    for( j in (i+1):n){
      print( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      load( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      d = mtrp$trp$cost[ length( mtrp$trp$cost ) ]
      distances[i, j] = d
      distances[j, i] = d
      
    }
    for(s in 1:(nscales-1) ){
      d.scales[s, i] =  mtrp$trp1[[s]]$cost[ length( mtrp$trp1[[s]]$cost ) ]       
      d.scales[s, j] =  mtrp$trp1[[s]]$cost[ length( mtrp$trp1[[s]]$cost ) ]       
    }
  }
  list(distances=distances, d.scales=d.scales)
}


