CreateGraph <- function (n) {
  
  #constructs random graphs
  #input=n=number of vertices of the graph
  #and plots the graph
  
  require(igraph) #install package igraph
  
  #n the number of vertices
  #create matrix
  M <- matrix(2, n, n) #we use number 2 for testing if there is an assigned value or not
  
  for (i in 1:n){
    for (j in 1:n){
    rand <- floor(runif(1, min=0, max=2)) #fill the matrix with 0 or 1
      
      #if I want to be more dense or more sparse, use:
      #rand <- sample(0:1,1,replace=T,prob=c(0.9,0.1))
    
      if (M[i,j]==2){ #change only the non assigned ones
        if (i==j) {
          M[i,j] <- 0 #set 0 for [1,1], [2,2] ect.
        } else {
          #matrix has to be symmetric
          M[i,j] <- rand
          M[j,i] <- rand
        }
      }
    }
  }
  
  #in order to plot it, I want to have the matrix like:
  #c(1,2,1,3) meaning these is an edge between 1-2, 1-3 ect
  
  aa <- vector()
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      if (M[i,j]==1){
        aa <- c(aa,i,j) #check if there is an edge, if yes add the i,j in the aa vector
      }
    }
  }
  
  #plot the graph
  G <- graph( aa, directed = FALSE )
  plot(G)
  return(M)
}