library(mop)

args <- commandArgs(trailingOnly = TRUE)
index = as.integer( args[1] )

print("Doing transport on index")
print( index )
load("../../Data/all.trees.Rdata")

gmra = list()
data = list()
weights = list()
for( i in 1:length(all$data) ){
  X = all$data[[i]][,1:4]
  X = X[ seq(1, nrow(X), by=10), ]
  weights[[i]] = X[,4] / sum(X[,4])

  X[, 4] = X[, 4] * 100;
  data[[i]] = X
  
  gmra[[i]] = gmra.create.ikm(X=X, eps=0.001, nKids=8)
}


trp.lp <- multiscale.transport.create.lp( oType = 26 )
icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )
for( j in (index+1):length(gmra) ){
    
    trp <- multiscale.transport.solve( trp.lp, gmra[[index]], gmra[[j]], p = 2, nType=0, 
                                       dType=1, w1=weights[[i]], w2=weights[[j]] )
    save( trp, file = sprintf("transport-maps/transport%.3d-%.3d.Rdata", index, j) )
}



