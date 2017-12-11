
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
    #distances[[i]] = matrix(0, nrow=n.subjects, ncol=n.subjects)
  }
  for( i in 1:(n.subjects-1) ){
    for( j in (i+1):n.subjects){
      print( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      load( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      n <- length(trp$cost) 

      map = trp$map[[n]]
      partition.from = partition.function( trp$from[[n]] )
      partition.to = partition.function( trp$to[[n]] )
      p.from <- max( partition.from )
      p.to <- max( partition.to )
      for( k1 in 1:n.partitions ){
        for( k2 in 1:p.to ){
          k.from = partition.from == k1 
          k.to   = partition.to   == k2 
          index  = which( k.from[ map[,1] ] & k.to[ map[,2] ] )
          #k.from = partition.from == k2 
          #k.to   = partition.to   == k1 
          #index2  = which( k.from[ map[,1] ] & k.to[ map[,2] ] )
          #index <- unique( c(index1, index2) )
          #dtmp1 = sum( map[index1, 3] * map[index1, 4] )^(1/p)
          #dtmp2 = sum( map[index2, 3] * map[index2, 4] )^(1/p)
          #dtmp  = (dtmp1 + dtmp2)/2
          if(length(index) > 0 ){
            dtmp = ( sum( (map[index, 3] * map[index, 4])) / sum(map[index,3]) )^(1/p) 
            #distances[[k1]][i, j] =  dtmp
            #distances[[k1]][j, i] =  dtmp
            distances[[k1]][[k2]][i, j] =  dtmp
            distances[[k1]][[k2]][j, i] =  dtmp
          }
          #distances[[k1]][[k2]][i, j] =  distances[[k1]][[k2]][i, j] + dtmp
          #distances[[k1]][[k2]][j, i] =  distances[[k1]][[k2]][j, i] + dtmp
          #distances[[k2]][[k1]][i, j] =  distances[[k2]][[k1]][i, j] + dtmp
          #distances[[k2]][[k1]][j, i] =  distances[[k2]][[k1]][j, i] + dtmp
        }
      }
    }
  }
  distances
}



load.transport.partitions <- function( folder, 
                                       n.subjects, 
                                       partition.function,  
                                       n.partitions 
                                      ){

  distances = matrix(0, nrow=n.subjects, ncol=n.subjects)
  costs = array(0, dim=c( n.subjects, n.subjects, n.partitions, n.partitions ) ) 
  mass  = array(0, dim=c( n.subjects, n.subjects, n.partitions, n.partitions ) ) 
  deltas = array(0, dim=c( n.subjects, n.subjects, n.partitions, n.partitions, 4 ) ) 

  for( i in 1:(n.subjects-1) ){
    for( j in (i+1):n.subjects){
      print( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      load( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      
      n <- length(trp$cost) 

            
      distances[i, j] = trp$cost[ n ]
      distances[j, i] = trp$cost[ n ]
      
      map = trp$map[[n]]
      delta = trp$to[[n]][map[,2], ] - trp$from[[n]][map[,1], ]

      partition.from = partition.function( trp$from[[n]] )
      partition.to = partition.function( trp$to[[n]] )
      p.from <- max( partition.from )
      p.to <- max( partition.to )
      for( k1 in 1:n.partitions ){
        for( k2 in 1:p.to ){
          k.from = partition.from == k1 
          k.to   = partition.to   == k2 
          index  = which( k.from[ map[,1] ] & k.to[ map[,2] ] )
          if(length(index) > 0 ){
            mtmp = sum( map[index, 3] )
            ctmp = sum( map[index, 4] * map[index,3] )
            dtmp = t(delta[index,])  %*% map[index,3]
            mass[ i, j, k1, k2] =  mtmp
            costs[i, j, k1, k2] =  ctmp
            deltas[i, j, k1, k2, ] = dtmp

            mass[ j, i, k2, k1] =  mtmp
            costs[j, i, k2, k1] =  ctmp
            deltas[j, i, k2, k1, ] = -dtmp
          }
        }
      }
    }
  }
  list(cost=costs, mass=mass, distances=distances, deltas=deltas)
}




load.transport.weighted.partitions <- function( folder, 
                                       n.subjects, 
                                       partition.function,  
                                       n.partitions ){

  distances = matrix(0, nrow=n.subjects, ncol=n.subjects)
  costs = array(0, dim=c( n.subjects, n.subjects, n.partitions, n.partitions ) ) 
  mass  = array(0, dim=c( n.subjects, n.subjects, n.partitions, n.partitions ) ) 
  deltas = array(0, dim=c( n.subjects, n.subjects, n.partitions, n.partitions, 4 ) ) 
  
  for( i in 1:(n.subjects-1) ){
    for( j in (i+1):n.subjects){
      print( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      load( sprintf("%s/transport%.3d-%.3d.Rdata", folder, i, j) )
      
      n <- length(trp$cost) 

            
      distances[i, j] = trp$cost[ n ]
      distances[j, i] = trp$cost[ n ]
      
      map = trp$map[[n]]
      
      delta = trp$to[[n]][map[,2], ] - trp$from[[n]][map[,1], ]
      
      partition.from = partition.function( trp$from[[n]] )
      partition.to = partition.function( trp$to[[n]] )
      for( k1 in 1:length(partition.from) ){
        for( k2 in 1:length(partition.to) ){
           weight1 <- partition.from[[k1]]
           weight2 <- partition.to[[k2]]
           w <- weight1[map[, 1]] * weight2[map[, 2]]
           
           #w <- w /sum(w)
           mtmp = sum( w * map[, 3] )
           ctmp = sum( map[, 4] * w * map[,3] )
           dtmp = t(delta)  %*% (w*map[ ,3])

           mass[ i, j, k1, k2] =  mtmp
           costs[i, j, k1, k2] =  ctmp
           deltas[i, j, k1, k2, ] = dtmp
            
           mass[ j, i, k2, k1] =  mtmp
           costs[j, i, k2, k1] =  ctmp
           deltas[j, i, k2, k1, ] = -dtmp
        }
      }
    }
  }
  list(cost=costs, mass=mass, distances=distances, deltas=deltas)
}
