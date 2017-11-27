
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



load.transport.distances.partitioned <- function( folder, 
                                                  n.subjects, 
                                                  partition.function,  
                                                  n.partitions, 
                                                  p){

  distances = vector( "list", n.partitions )
  for( i in 1:n.partitions){
    distances[[i]] = vector( "list", n.partitions)
    for( j in 1:n.partitions){
      distances[[i]][[j]] = matrix(0, nrow=n.subjects, ncol=n.subjects)
    }
  }
  for( i in 1:(n.subjects-1) ){
    for( j in (i+1):n.subjects){
      print( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      load( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      n <- length(trp$cost) 
      map = trp$map[[n]]
      partition.from = partition.function( trp$from[[n]] )
      partition.to = partition.function( trp$to[[n]] )
      p.from <- unique( partition.from )
      p.to <- unique( partition.to )
      for( k1 in p.from ){
        for( k2 in p.to ){
          k.from = partition.from == k1 | partition.from == k2
          k.to   = partition.to   == k2 | partition.to   == k1
          index  = which( k.from[ map[,1] ] & k.to[ map[,2] ] )
          dtmp = sum( map[index, 3] * map[index, 4] )
          distances[[k1]][[k2]][i, j] =  dtmp^(1/p)
          distances[[k1]][[k2]][j, i] =  dtmp^(1/p)
        }
      }
    }
  }
  distances
}

