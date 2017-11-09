source("read.tre.R")
library(mop)

if(FALSE){

X1 <- read.tree( "../Data/Normal02/VascularNetwork.tre")
X1 = X1[seq(1, nrow(X1), by=10), ]
X2 <- read.tree( "../Data/Normal03/VascularNetwork.tre")
X2 = X2[seq(1, nrow(X2), by=10), ]
w1 = X1[, 4]
w2 = X2[, 4]
w1 =  w1 /sum(w1)
w2 =  w2 /sum(w2)
#include radi in metric
X1[,4] = X1[,4]*10
X2[,4] = X2[,4]*10
}

print(dim(X1))
print(dim(X2))

gmra1 = gmra.create.ikm(X=X1[,1:4], eps=0.001, nKids=8)
print("done")
gmra2 = gmra.create.ikm(X=X2[,1:4], eps=0.001, nKids=8)
print("done")
     
trp.lp <- multiscale.transport.create.lp( oType = 26 )
icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 1, 0 )
multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )
multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 1 )

time1 <- system.time( 
  trp1 <- multiscale.transport.solve( trp.lp, gmra1, gmra2, p = 2, nType=0, dType=1,w1=w1, w2=w2 ) 
)

index = 0
for( t in c( seq(0, 1, by=0.01), seq(1, 0, by=-0.01) ) ){ 
  X = multiscale.transport.interpolate(trp1, length(trp1$map), t=t)
  #plot(NA, xlim=range(X$X[,1]), ylim=range(X$X[,2]), 
  #     bty="n", xlab="", ylab="", axes=FALSE)
  symbols(X$X, circles=X$X[,4]/10, inches=FALSE, bg="#00000010", 
          fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
  dev.copy(png, sprintf( "ip-%.5d.png", index) )
  dev.off()
  index = index +1
}

library(rgl)

ts = c( seq(0, 1, by=0.01), seq(1, 0, by=-0.01) )
ts = rep(ts, 6)
n=length(ts)
for( i in 1:n ){ 
  X = multiscale.transport.interpolate(trp1, length(trp1$map), t=ts[i])
  plot3d(X$X[,1:3], alpha=0.15, box=FALSE, axes=FALSE, 
         xlab="", ylab="", zlab="", type="s", radius=X$X[,4]/10 )
  rgl.viewpoint( 0.2 * (i-1), -90 + (i-1) * 10/n )
  rgl.snapshot( sprintf( "ip3d-%.5d.png", i) )
  index = index +1
}



