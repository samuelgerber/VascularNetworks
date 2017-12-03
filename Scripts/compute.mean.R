library(mop)
library(data.table)


load("../../Code/Data/Bullit01/all.trees.processed.02.Rdata")

#n.subjects = 10
n.subjects = length(all$data)

gmra = vector("list", n.subjects)
weights = vector("list", n.subjects)
radius.scaling = 100
X.mean = c()
W.mean = c()
for( i in 1:n.subjects ){
  X = all$data[[i]][,1:4]
  weights[[i]] = all$data[[i]]$v
  X[, 4] = X[, 4] * radius.scaling
  colnames(X) <- c("x", "y", "z", "r")
  gmra[[i]] = gmra.create.ikm(X=X, eps=0.0001, nKids=4)
  X.mean = rbind(X.mean, X)
  W.mean = c(W.mean, weights[[i]] )
}

gmra.tmp <- gmra.create.ikm(X.mean, eps=25, stop=3, nKids=4)

X.mean <- as.data.table( gmra.centers(gmra.tmp, 1000) )
partition <- gmra.partition(gmra.tmp, 1000)
p.id <- rep(1:length(partition), sapply(partition, length) )
dt <- data.table(p.id=p.id, w = W.mean)
dt = dt[, .(sum(w), by=p.id)]

W.mean = dt[order(p.id), ]$w

colnames(X.mean) = c("x", "y", "z", "r")
X = X.mean  
symbols( X$x, X$y, circles=X$r/radius.scaling, inches=FALSE, bg="#00000013", 
             fg="#00000000", bty="n", xlab="", ylab="" ) 

trp.lp <- multiscale.transport.create.lp( oType = 26 )
icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )

step = 1

X.progress = list()
X.progress[[1]] = X.mean
costs <- list()
for(iter in 1:40){
  gmra.mean = gmra.create.ikm(X=X.mean, eps=5, nKids=4, stop=3)

  X.update = as.data.table( matrix(0, nrow = nrow(X.mean), ncol = ncol(X.mean) ) )
  mass.update = rep(0, nrow(X.mean) )
  colnames(X.update) = colnames(X.mean)
  sum.cost = 0
  for( j in 1:length(gmra) ){
    
    trp <- multiscale.transport.solve( trp.lp, gmra.mean, gmra[[j]], p = 2, nType=0, 
                                       dType=1, w1=W.mean, w2=weights[[j]] )
    n = length(trp$cost)
    sum.cost = sum.cost + trp$cost[[n]]
    if(trp$cost[[n]] > 0 ){
     

    map = trp$map[[n]]
    from.index = trp$fromIndex[[n]]
    from.size = trp$fromSize[[n]] 
    delta = trp$to[[n]][map[,2], ] - trp$from[[n]][map[,1], ]
    dt = data.table( x = delta[,1], y=delta[,2], z=delta[,3], r=delta[,4], mass = map[,3], from.id=map[,1])
    v.sum = dt[, .( x=sum(x*mass), y=sum(y*mass), z=sum(z*mass), r=sum(r*mass), mass=mass ), by=from.id]
    v.sum = v.sum[order(from.id), ]
    v.sum = v.sum[rep(1:length(from.size), from.size), ]

    X.update[from.index, ] = X.update[from.index, ] + v.sum[, .(x, y, z, r)]
    mass.update[from.index] = mass.update[from.index] + v.sum$mass
    }

  }
  for( j in 1:ncol(X.update) ){
    X.update[ ,j] = X.update[ ,j, with=FALSE]/mass.update
  }
  
  print( summary(X.update) )  
  symbols( X$x, X$y, circles=X$r/radius.scaling, inches=FALSE, bg="#00000007", 
             fg="#00000000", bty="n", xlab="", ylab="" ) 
  X.mean = X.mean + step * X.update / n.subjects
  X.mean$r[X.mean$r<0] = 0
  #W.mean = mass.update / n.subjects
 
  X.progress[[iter+1]] = X.mean
  costs[[iter]] = sum.cost
   
  symbols( X.mean$x, X.mean$y, circles=X.mean$r/radius.scaling, inches=FALSE, bg="#FF000016", 
             fg="#00000000", bty="n", xlab="", ylab="", add=TRUE )
  #browser() 
}

