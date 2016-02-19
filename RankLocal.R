RankLocal <- function(graph){
  #improves the initial random decomposition by local search
  start.time <- Sys.time() #keep track of time
  
  n <- nrow(graph)
  #creates a random decomposition
  decop <- TreeDecomposition(n)
  first <- decop #keep the original
  DrawDecopWithInput2(decop)
  #the initial decop & rank of nodes (ranknodes):
  
  rank <- RankWidthInput(decop,graph)
 
  rankfirst <- vector() #keep the ranks
 
  rrank <- list()
  rr <- 1
  times <- 0
  
  while(TRUE){
  
  find <- 0
  CONT <- 0
  times <- times + 1
  #the initial decop & ranknodes:
  
  rank <- RankWidthInput(decop,graph)
  if (times==1) rankfirst <- rank
  #find where the max rank is achieved & change it
  vertices <- vector()
  vertices <- c(vertices, 1:n) #vector consisting of n element defining the vertices
  #we want to find the rank-width of decop
  
  node <- vector()
  other <- vector()
  maxrank <- 0 #initialize maxrank
  ranknodes <- vector()
  M <- graph
  
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
    ranknodes[1] <- 0
    ranknodes[s] <- mrank
    #keep the maximum
    
    if (mrank>maxrank) maxrank <- mrank
  }
   rrank[[times]]<- ranknodes
  ranknodesbest <- ranknodes
  decopbest <- decop
  s1 <- sum(ranknodes)
  position <- vector()
  position <- which(ranknodes == max(ranknodes))
  
  for (po in 1:length(position)){
    if (CONT==1) break()
    #for each node that achieves the max rank, finds its children:
    node <- position[po]
    
    #find all the children-nodes of the node:
     kids <- c(decop[[node]][[1]],decop[[node]][[2]])
    children <- vector()
    h <- 1
    for (j in 1:(n-1)){
      if (length(intersect(kids,decop[[j]][[1]]))>=1 && length(intersect(kids,decop[[j]][[2]]))>=1){
        
        children[h] <- j
        h <- h + 1 
      }
    }
    children <- setdiff(children,node)
    if (length(children)<1) break()
    
    for (sh in 1:length(children)){
      if (find==1) { #i have found a better decomposition
        decop <- decopbest #continue 
       CONT <- 1
        
        break()
      }
      #for each children, find the available leaves to replace:
      #not the leaves-brothers, the ones under the same parent
      ch1 <- c(decop[[children[sh]]][[1]],decop[[children[sh]]][[2]])
      avail <- setdiff( c(1:n),ch1)
      #find parent:
      end <- 0
      j <- n-1
      info <- sort(c(decop[[children[sh]]][[1]],decop[[children[sh]]][[2]]))
      
      while (end==0){
        if (isTRUE(all.equal(info,sort(decop[[j]][[1]]))) || isTRUE(all.equal(info,sort(decop[[j]][[2]])))) {
          parent <- j
          end <- 1
        }
        j <- j - 1
      }
      brothers <- c(decop[[parent]][[1]],decop[[parent]][[2]])
      avail <- setdiff(avail,brothers)
      
      for (kl in 1:length(avail)){
        #for each available leaf, find new decop with replace
         
        newdecop <- Replace(decop,children[sh],avail[kl])  
         
        vertices <- vector()
        vertices <- c(vertices, 1:n) #vector consisting of n element defining the vertices
       
        #we want to find the rank-width of decop
        
        node <- vector()
        other <- vector()
        maxrank <- 0 #initialize maxrank
        ranknodes <- vector()
        M <- graph
        
        for (s in 2:length(newdecop)){ #length(decop)= number of internal nodes
          #i begins with 2, we do not examine rankwidth of the root
          
          #find the "children" of that internal node
          node <- sort(c(newdecop[[s]][[1]],newdecop[[s]][[2]])) #sort them
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
          ranknodes[1] <- 0
          ranknodes[s] <- mrank
          #keep the maximum
          
          if (mrank>maxrank) maxrank <- mrank
        }
        rrank[[rr]] <- ranknodes
        rr <- rr + 1
        
        #if the new decop is better,keep it:
        if (max(ranknodes)<rank){
          s1 <- sum(ranknodes)
          decopbest <- newdecop
          find <- 1
          ranknodesbest <- ranknodes
          rankfirst <- c(rankfirst,max(ranknodes))
        }
        if (find==1) break()
      }
    }
    }
  if (find==0 || length(children)<1) break()
  }
  #draw first and last decomposition
  DrawDecopWithInput2(first)
  DrawDecopWithInput2(decopbest)
  
  #keep track of time
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  return(rankfirst)
}