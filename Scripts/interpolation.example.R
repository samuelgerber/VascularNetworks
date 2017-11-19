library( mop )
source( "plots.R" )

load( "../Data/Bullit01/all.trees.processed.02.Rdata" )

X1 = all$data[[1]][,1:4]
w1 = all$data[[1]]$v
X1[, 4] = X1[, 4] * 100;
  
X2 = all$data[[2]][,1:4]
w2 = all$data[[2]]$v
X2[, 4] = X2[, 4] * 100;


gmra1 = gmra.create.ikm(X=X1, eps=0.3, nKids=4, stop=3)
gmra2 = gmra.create.ikm(X=X2, eps=0.3, nKids=4, stop=3)

trp.lp <- multiscale.transport.create.lp(26 )
icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )
t <- system.time( trp <- multiscale.transport.solve( trp.lp, gmra1, gmra2, p = 2, nType=0, 
                                       dType=1, w1=w1, w2=w2 ) )

print(t)

plot.interpolation.2d(trp, 0.01)

