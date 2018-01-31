library(mop)

args <- commandArgs(trailingOnly = TRUE)
index = as.integer( args[1] )

print("Doing transport on index")
print( index )
load("../../Code/Data/Bullit01/all.trees.processed.02.Rdata")

gmra = vector("list", 42)
weights = vector("list", 42)
for( i in index:length(all$data) ){
  X = all$data[[i]][,1:4]
  weights[[i]] = all$data[[i]]$v
  X[, 4] = X[, 4] * 100;
  gmra[[i]] = gmra.create.ikm(X=X, eps=0.0001, nKids=8)
}


dir.create("./transport-maps")

trp.lp <- multiscale.transport.create.lp( oType = 26 )
icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )
for( j in (index+1):length(gmra) ){
    
    trp <- multiscale.transport.solve( trp.lp, gmra[[index]], gmra[[j]], p = 2, nType=0, 
                                       dType=1, w1=weights[[index]], w2=weights[[j]], scaleMass=FALSE )
    save( trp, file = sprintf("transport-maps/transport%.3d-%.3d.Rdata", index, j) )
}

# Shell command:
# printf %s\\n {1..41} | xargs -t -n1 -P8 -I{} RScript pairwise.transport.unscaled.R {}
