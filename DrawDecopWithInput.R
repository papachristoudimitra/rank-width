DrawDecopWithInput <- function(decop){
  
  require(igraph) #install package igraph
  
  #input= decomposition
  #draws the tree decomposition
  
  #nodes = length(decop) + 1
  n <- length(decop) + 1
  graph <- vector()
  
  #graph should contain the edges in a way: (1,2,1,3) if there is edge 1-2,1-3 
  
  for (i in 1:(n-1)) {
  for (j in 1:(n-1)){
    comb <- sort(c(decop[[j]][[1]],decop[[j]][[2]]))
    if (isTRUE(all.equal(sort(decop[[i]][[1]]),comb)) || isTRUE(all.equal(sort(decop[[i]][[2]]),comb)))
      graph <- c(graph,i,j)
  }
  }
  #put the leaves:
  
  #make a vector with node names
  names <- vector()
  for (m in 1:n) {
    names[m] <- (paste0("N.", m))
  } 
  for (nodec in (n-1):1){
    
    if (length(decop[[nodec]][[1]])==1) {
      label <- names[decop[[nodec]][[1]]]
      graph <- c(graph,nodec,label)
    }
    if (length(decop[[nodec]][[2]])==1) {
      label <- names[decop[[nodec]][[2]]]
      graph <- c(graph,nodec,label)
    }      
  }   
  #plot it
  
  G <- graph(graph, directed = FALSE )
  plot(G, layout = layout.reingold.tilford(G, root=1)) 
}