library(mop)
source("../../Code/Scripts/multiresolution.R")
source("../../Code/Scripts/multiresolution.transport.v2.R")

args <- commandArgs(trailingOnly = TRUE)
index = as.integer( args[1] )

print("Doing transport on index")
print( index )
load("../../Code/Data/Bullit01/all.trees.processed.02.Rdata")

mres = vector("list", 42)
for( i in index:length(all$data) ){
  mres[[i]] = multiresolution.gmra( all$data[[i]] )
}



dir.create("./transport-maps")

trp.lp <- multiscale.transport.create.lp( 26 )
icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )
for( j in (index+1):length( mres) ){
    mtrp <- multiresolution.transport.v2( mres[[index]], mres[[j]], trp.lp )
    save( mtrp, file = sprintf("transport-maps/transport%.3d-%.3d.Rdata", index, j) )
}

# Shell command:
# printf %s\\n {1..41} | xargs -t -n1 -P7 -I{} RScript multiresolution.pairwise.distance.R {}

