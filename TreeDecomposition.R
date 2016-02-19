TreeDecomposition <- function (n)
{
# n is the number of vertices of the graph (we assume n>2)
#it returns a list containing the internal nodes of a random decomposition
#length(intnodes)=number of internal nodes
#intnodes[[1]] 1st internal node
#intnodes[[1]][[1]] left child of node 1, subset of the vertices
#intnodes[[1]][[2]] right child of node 1, subset of the vertices


counter <- n #number of leaves I want to have
end <- 0 #if end=0, continue. else, stop
nodec <- 1 #counter for internal nodes, we start from i
i <- 2 #helping counter

vertices <- vector()
vertices <- c(vertices, 1:n) #vector consisting of n element defining the vertices

A <- RandomSets(vertices) #random subset A of vertices
#B <- vector() #creating empty vectors
#C <- vector()


#find the remaining vertices
B <- setdiff(vertices,A) 


#create node.1
node.1 <- list(A,B)
#keep track of internal nodes
intnodes <- vector()
intnodes <- list(node.1) #intnodes[[1]] is node.1, intnodes[[1]][[1]] is the Lchild of node.1,
                         #intnodes[[1]][[2]] is the Rchild ect

#while endo!=1, we need to continue
#we assume that we have more that 2 vertices

while (end==0){
  #we continue to find node.2, using node.1 etc
  
  #if everything are leaves, we stop
  if (length(intnodes[[nodec]][[1]])==1) counter <- counter - 1
  if (length(intnodes[[nodec]][[2]])==1) counter <- counter - 1
  
  if (length(intnodes[[nodec]][[1]])>1){
    A <- RandomSets(intnodes[[nodec]][[1]]) #random subset A of vertices of node.i A=Left, B=Right
    #find the remaining vertices
    B <- setdiff(intnodes[[nodec]][[1]],A)
    #keep node
    intnodes[[i]] <- list(A,B) #we use counter i, to idnicate the internal nodes
    i <- i + 1 #save at the next position
  }
     
  if (length(intnodes[[nodec]][[2]])>1){
    A <- RandomSets(intnodes[[nodec]][[2]])
    #find the remaining vertices
    B <- setdiff(intnodes[[nodec]][[2]],A)
    #keep node
    intnodes[[i]] <- list(A,B)
    i <- i + 1
  }
  
  nodec <- nodec + 1 #continue to examine next internal node
  
  if (counter==0){ #we stop when counter=0, meaning that we have constructed all vertices as leaves
    end <- 1
  }
}
return(intnodes)
}

