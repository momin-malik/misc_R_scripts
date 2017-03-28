# This produces a dot plot (a horizontal histogram made with dots instead of bars)
# for counts of triads in a graph. Helpfully, I generate pictures of the triads. 
# This uses the network package, rather than the igraph package. 
# V1.0, by Momin M. Malik, August 11, 2016.

library(network)
library(sna)

# Replace g with your graph. 
triad.census <- triad.census(g)

census.triads <- rep(list(network.initialize(3)),16) # length(triads)
names(census.triads) <- colnames(triad.census)
census.triads[["012"]][1,2] <- 1
census.triads[["102"]][1,2] <- 1; census.triads[["102"]][2,1] <- 1
census.triads[["021D"]][2,1:3] <-1
census.triads[["021U"]][1:3,2] <- 1
census.triads[["021C"]][1,2] <- 1;census.triads[["021C"]][2,3] <- 1
census.triads[["111D"]][1:2,3] <- 1; census.triads[["111D"]][3,1] <- 1
census.triads[["111U"]][3,1:2] <- 1; census.triads[["111U"]][1,3] <- 1
census.triads[["030T"]][1,2:3] <- 1; census.triads[["030T"]][3,2] <- 1
census.triads[["030C"]][1,2] <- 1; census.triads[["030C"]][2,3] <- 1; census.triads[["030C"]][3,1] <- 1
census.triads[["201"]][1,2:3] <- 1; census.triads[["201"]][2:3,1] <- 1
census.triads[["120D"]][2:3,1] <- 1; census.triads[["120D"]][1:2,3] <- 1
census.triads[["120U"]][1,2:3] <- 1; census.triads[["120U"]][3,1:2] <- 1
census.triads[["120C"]][1,2:3] <- 1; census.triads[["120C"]][2,3] <- 1; census.triads[["120C"]][3,1] <- 1
census.triads[["210"]][1,2:3] <- 1; census.triads[["210"]][3,1:2] <- 1; census.triads[["210"]][2,3] <- 1
census.triads[["300"]][1,2:3] <- 1; census.triads[["300"]][2,c(1,3)] <- 1; census.triads[["300"]][3,1:2] <- 1

coords <- t(matrix(c(c(-1/sqrt(3),0),c(0,1),c(1/sqrt(3),0)),nrow=2,ncol=3))

pdf("triad.census.pdf",width=10,height=7.5)
w <- 15 # Additional width past the first column
h <- 19
rpad <- 1 # Subtracts from width on right side
for (j in 1:nrow(triad.census)) {
  layout(matrix(c(1:h,rep(h+1,h*(w-rpad)),rep(h+2,h*rpad)),nrow=h),
         widths=c(rep(1,h),w-rpad,rpad),
         heights=c(rep(1,h),h,h))
  par(mar=c(0,0,0,0))
  plot.new()
  for (i in 1:length(census.triads)) {
    gplot(census.triads[[i]],
          coord=coords,
          vertex.col=1,
          jitter=F,
          edge.lwd=1,
          vertex.cex=2,
          arrowhead.cex=3)
  }
  plot.new()
  plot.new()
  par(mar=c(4.5,0,1.5,0)) # bottom, left, top, right
  dotchart(log10(rev(triad.census[j,])),xlim=c(.3,log10(10^8.1)),xaxt='n')
  orders <- 9
  seq <- rep(c(1,2,5),orders)*rep(10^(0:(orders-1)),each=3)
  abline(v=log10(seq[-1]),col="lightgray",lty=2)
  plot.new()
}
dev.off()
par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
