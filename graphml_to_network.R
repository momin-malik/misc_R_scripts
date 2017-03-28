# Sometimes, in order to apply models like stERGMs or SIENA, one needs to go from temporal graphML
# objects to "network" library objects that have all nodes present in the system. This turned out 
# to be slightly tricky.
# 
# Version 1.0. By Momin M. Malik, April 29, 2017.

files.folder <- "GraphML" # Enter the folder within that directory that contains your count files
filenames <- list.files(files.folder, pattern="*.graphml", full.names=T)
library("intergraph")
library("igraph")
ldf <- lapply(filenames, read.graph, format="graphml") # Read in all graphml files at once
vertices <- V(ldf[[1]])$name # Initialize the vertex names
for (i in 1:length(ldf)) { # go through adding new nodes, and making wedge weights 1
  vertices <- union(vertices,V(ldf[[i]])$name)
  E(ldf[[i]])$weight <- 1
}
gBlank <- make_empty_graph() + vertices(vertices) # Initialize an empty graph with the total node set
for (i in 1:length(ldf)) {
  ldf[[i]] <- union(ldf[[i]],gBlank) # Union graph with nonactive nodes. List methods werne't working
}

ldf <- lapply(ldf, asNetwork)
detach("package:igraph", unload=TRUE)
rm(vertices,i,gBlank,files.folder,filenames)
