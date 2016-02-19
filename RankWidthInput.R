RankWidthInput <- function (decop,M)
{
  #INPUT: decomposition and M
  #M(n x n)= incidence matrix, |V|x|V|, 1 is there is an egde from Vi to Vj, 0 otherwise
  

  n <- length(decop) + 1
  vertices <- vector()
  vertices <- c(vertices, 1:n) #vector consisting of n element defining the vertices
   
  #we want to find the rank-width of decop
  
  node <- vector()
  other <- vector()
  maxrank <- 0 #initialize maxrank
  
  for (s in 2:length(decop)){ #length(decop)= number of internal nodes
    #i begins with 2, we do not examine rankwidth of the root
    
    #find the "children" of that internal node
    node <- sort(c(decop[[s]][[1]],decop[[s]][[2]])) #sort them
    other <- sort(setdiff(vertices,node)) #sort them
    #we will construct a matrix node x other for each i (internal node)
    
    a <- vector()
    k <- 1
    for (i in node){ #loop through values of node
      for (j in other){ #loop through values of other
        #for each i, find if there is 1 or 0
        a[k] <- M[i,j]
        k <- k + 1
      } 
    }
    mat <- matrix(a, ncol=length(other), byrow=TRUE) #create matrix by row!
    #compute rank of the matrix
    y <- qr(mat)
    mrank <- y$rank
    
    #keep the maximum
    if (mrank>maxrank) maxrank <- mrank 
  }
  return (maxrank)
}