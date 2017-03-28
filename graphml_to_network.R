setwd("~/Documents/CMU/Sven Apel/qemu")

# library(ergm)
# help('ergm-terms')
# detach("package:ergm", unload=TRUE)

files.folder <- "GraphML" # Enter the folder within that directory that contains your count files
filenames <- list.files(files.folder, pattern="*.graphml", full.names=T)
# ldf <- lapply(lapply(lapply(lapply(filenames, igraph::read.graph, format="graphml"),asNetwork),as.matrix),network)
# ldf <- lapply(lapply(filenames, igraph::read.graph, format="graphml"),asNetwork)
library("intergraph")
library("igraph")
ldf <- lapply(filenames, read.graph, format="graphml")
vertices <- V(ldf[[1]])$name
for (i in 1:length(ldf)) {
  vertices <- union(vertices,V(ldf[[i]])$name)
  E(ldf[[i]])$weight <- 1
}
gBlank <- make_empty_graph() + vertices(vertices)
for (i in 1:length(ldf)) {
  ldf[[i]] <- union(ldf[[i]],gBlank)
}

ldf <- lapply(ldf, asNetwork)
detach("package:igraph", unload=TRUE)
rm(vertices,i,gBlank,files.folder,filenames)

ldf2 <- list()
for (i in 1:(length(ldf)-1)) {
  M1 <- as.matrix(ldf[[i+1]])
  M1 <- M1[order(rownames(M1)),]
  M1 <- M1[,order(colnames(M1))]
  M2 <- as.matrix(ldf[[i]])
  M2 <- M2[order(rownames(M2)),]
  M2 <- M2[,order(colnames(M2))]
  ldf2[[i]] <- M2-M1
}

count <- lapply(ldf2,table)
delta <- matrix(NA,ncol=3,nrow=length(ldf2))
for (i in 1:length(count)) {
  try(delta[i,1] <- count[[i]]["-1"])
  try(delta[i,2] <- count[[i]]["0"]-length(ldf2))
  try(delta[i,3] <- count[[i]]["1"])
}

delta[is.na(delta)] <- 0

pdf("qemu_dynamics.pdf",width=10,height=7.5)
plot(1:113,delta[,1],type="l",xlab="Time interval",ylab="Change in number of active ties",main="qemu Dynamics")
points(1:113,delta[,1],pch=19,cex=.5)
lines(1:113,delta[,3],col=2,lty=2)
points(1:113,delta[,3],col=2,pch=19,cex=.5)
legend("topleft",inset=c(.05,.08),legend=c("Ties created","Ties Destroyed"),col=1:2,pch=19,lty=1:2)
dev.off()

library("network")
library("degreenet")
library("magrittr")
library("tergm")

# lapply(lapply(ldf,degree,cmode="indegree"),max)
# hist(lapply(ldf,degree,cmode="indegree")[[111]])

# Make an "indegree at previous time point" attribute!

# Problem: not the same network!! Go back to igraph to fix. 
# Do the same trick as before: Initialize all the nodes in a blank graph,
# then take the union with the edges to get the complete nodeset. 
# gBlank <- make_empty_graph() + vertices(union(rownames(egolistfrnCSS[[i]]),rownames(egolistfrnLAS[[i]])))
# 
# gCSS <- simplify(graph.adjacency(egolistfrnCSS[[i]],mode="directed"))
# V(gCSS)$name <- rownames(egolistfrnCSS[[i]])
# gCSS <- union(gBlank,gCSS)
# 
# gLAS <- simplify(graph.adjacency(egolistfrnLAS[[i]],mode="directed"))
# V(gLAS)$name <- rownames(egolistfrnLAS[[i]])
# gLAS <- union(gBlank,gLAS)


samp.fit <- stergm(ldf,
                   formation=~edges,
                   dissolution=~edges,
                   estimate="CMLE",times=50:60,
                   control.stergm(CMLE.MCMC.burnin=1024))

mcmc.diagnostics(samp.fit)
summary(samp.fit)
plot(samp.fit)