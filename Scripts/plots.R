source("read.tre.R")
library(mop)


plot.interpolation.2d <- function(trp1, radius.scaling ){
  index = 0
  for( t in c( seq(0, 1, by=0.01), seq(1, 0, by=-0.01) ) ){ 
    X = multiscale.transport.interpolate(trp1, length(trp1$map), t=t)
    #plot(NA, xlim=range(X$X[,1]), ylim=range(X$X[,2]), 
    #     bty="n", xlab="", ylab="", axes=FALSE)
    symbols( X$X, circles=X$X[,4]*radius.scaling, inches=FALSE, bg="#00000010", 
             fg="#00000000", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
    dev.copy(png, sprintf( "ip-%.5d.png", index) )
   dev.off()
    index = index +1
  }

}



plot.interpolation.3d <- function(trp1, radius.scaling){

  library(rgl)

  ts = c( seq(0, 1, by=0.01), seq(1, 0, by=-0.01) )
  ts = rep(ts, 6)
  n=length(ts)
  for( i in 1:n ){ 
    X = multiscale.transport.interpolate(trp1, length(trp1$map), t=ts[i])
    plot3d(X$X[,1:3], alpha=0.15, box=FALSE, axes=FALSE, 
           xlab="", ylab="", zlab="", type="s", radius=X$X[,4]*radius.scaling )
    rgl.viewpoint( 0.2 * (i-1), -90 + (i-1) * 10/n )
    rgl.snapshot( sprintf( "ip3d-%.5d.png", i) )
    index = index +1
  }

}


#imagemagick gif command
# convert -dispose previous -delay 5 -alpha remove -loop 0 ip-00*.png ip.gif


