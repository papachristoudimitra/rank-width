Replace <- function(decop,node,leaf){
  #replaces a node to a leaf in a decomposition
  
  copy <- decop #keep a copy of the original decomposition
  
  #find the parent of node
  #find the node that has as childe c(decop[[node]][[1]],decop[[node]][[2]])
  
  n <- length(decop) + 1
  end <- 0
  j <- n-1
  info <- sort(c(decop[[node]][[1]],decop[[node]][[2]]))
  
  while (end==0){
    if (isTRUE(all.equal(info,sort(copy[[j]][[1]]))) || isTRUE(all.equal(info,sort(copy[[j]][[2]])))) {
      parentn <- j #parent of node
      end <- 1
    }
    j <- j - 1
  }
  
  #find the parent of leaf
  end <- 0
  j <- n-1
  info <- leaf
  
  while (end==0){
    if (isTRUE(all.equal(info,sort(copy[[j]][[1]]))) || isTRUE(all.equal(info,sort(copy[[j]][[2]])))) {
      parentl <- j #parent of leaf
      end <- 1
    }
    j <- j - 1
  }
  
#change anchestors of node, leaf
#find ancestors of node:
  parentsn <- parentn
  
  i <- 2
  info <- sort(c(copy[[parentsn[i-1]]][[1]],copy[[parentsn[i-1]]][[2]]))
  while (length(info)!=n){
    for (j in (n-1):1){
      if (isTRUE(all.equal(info,sort(copy[[j]][[1]]))) || isTRUE(all.equal(info,sort(copy[[j]][[2]])))) {
        parentsn[i] <- j
        i <- i + 1
      }
    }
    info <- sort(c(copy[[parentsn[i-1]]][[1]],copy[[parentsn[i-1]]][[2]])) #elements of previous parent
  }
  
  
  #find ancestors of leaf:
  parentsl <- parentl
  i <- 2
  info <- sort(c(copy[[parentsl[i-1]]][[1]],copy[[parentsl[i-1]]][[2]]))
  while (length(info)!=n){
    for (j in (n-1):1){
      if (isTRUE(all.equal(info,sort(copy[[j]][[1]]))) || isTRUE(all.equal(info,sort(copy[[j]][[2]])))) {
        parentsl[i] <- j
        i <- i + 1
      }
    }
    info <- sort(c(copy[[parentsl[i-1]]][[1]],copy[[parentsl[i-1]]][[2]])) #elements of previous parent
  }
  #i have to change ancestors, the parents have changed
  
  #first change parents of leaf, then node, then share ones:
  leafparents <- setdiff(parentsl, parentsn)
  nodeparents <- setdiff(parentsn, parentsl)
  
  #find the shared parents of node,leaf
  #sharedparents
  sharedparents <- vector()
  sharedparents <- intersect(parentsl,parentsn)
  
  #keep decop[[node]]
  replace <- c(decop[[node]][[1]],decop[[node]][[2]]) #all children of the node
  
  #put as leaves the decomposition of node after leaf
  #change parentleaf
  
  if (isTRUE(all.equal(leaf,copy[[parentl]][[1]]))) decop[[parentl]][[1]] <- replace
  if (isTRUE(all.equal(leaf,copy[[parentl]][[2]]))) decop[[parentl]][[2]] <- replace
  
  #change parentnode
  
  info <- sort(c(decop[[node]][[1]],decop[[node]][[2]]))
  if (isTRUE(all.equal(info,sort(copy[[parentn]][[1]])))) decop[[parentn]][[1]] <- leaf
  if (isTRUE(all.equal(info,sort(copy[[parentn]][[2]])))) decop[[parentn]][[2]] <- leaf
  
  if (length(leafparents)>1){
    
    for (i in 2:length(leafparents)){
      rep <- sort(c(decop[[leafparents[i-1]]][[1]],decop[[leafparents[i-1]]][[2]]))
      before <- sort(c(copy[[leafparents[i-1]]][[1]],copy[[leafparents[i-1]]][[2]]))
      #where?
      if (isTRUE(all.equal(before, sort(decop[[leafparents[i]]][[1]])))) {
        decop[[leafparents[i]]][[1]] <- rep
      } else {
        decop[[leafparents[i]]][[2]] <- rep
      } 
    }
  }
  #for nodes:
  #change them
  if (length(nodeparents)>1){
    for (i in 2:length(nodeparents)){
      rep <- sort(c(decop[[nodeparents[i-1]]][[1]],decop[[nodeparents[i-1]]][[2]]))
      before <- sort(c(copy[[nodeparents[i-1]]][[1]],copy[[nodeparents[i-1]]][[2]]))
      
      if (isTRUE(all.equal(before, sort(decop[[nodeparents[i]]][[1]])))){
        decop[[nodeparents[i]]][[1]] <- rep
      } else {
        decop[[nodeparents[i]]][[2]] <- rep
      } 
    }
  }
  
  for (i in 1:length(sharedparents)) { 
    children <- vector()
    #find its children nodes
    k <- 1 
    for (j in 1:(n-1)){ 
      ex <- sort(c(copy[[j]][[1]],copy[[j]][[2]]))
      if (isTRUE(all.equal(sort(copy[[sharedparents[i]]][[1]]),ex)) || isTRUE(all.equal(sort(copy[[sharedparents[i]]][[2]]),ex))){
        children[k] <- j
        k <- k + 1  
      }
    }
    #replace them
    
    #what if are the LEAF
    if (length(decop[[sharedparents[i]]][[1]])==1 && decop[[sharedparents[i]]][[1]]==leaf) {
      decop[[sharedparents[i]]][[2]] <- c(decop[[children[2]]][[1]],decop[[children[2]]][[2]])
    }else if (length(decop[[sharedparents[i]]][[2]])==1 && decop[[sharedparents[i]]][[2]]==leaf)  {
      decop[[sharedparents[i]]][[1]] <- c(decop[[children[1]]][[1]],decop[[children[1]]][[2]])
    }else {
      if (copy[[sharedparents[i]]][[1]]==decop[[sharedparents[i]]][[1]] && copy[[sharedparents[i]]][[2]]==decop[[sharedparents[i]]][[2]]){  
        #there are two nodes as children
    if (length(children)>1){    
    decop[[sharedparents[i]]][[1]] <- c(decop[[children[1]]][[1]],decop[[children[1]]][[2]])
    decop[[sharedparents[i]]][[2]] <- c(decop[[children[2]]][[1]],decop[[children[2]]][[2]])
    } else {
      #exei leaf
      if (length(copy[[sharedparents[i]]][[1]])==1) decop[[sharedparents[i]]][[2]] <- c(decop[[children[1]]][[1]],decop[[children[1]]][[2]])
      if (length(copy[[sharedparents[i]]][[2]])==1) decop[[sharedparents[i]]][[1]] <- c(decop[[children[1]]][[1]],decop[[children[1]]][[2]])   
    }
      } else {
        if (isTRUE(all.equal(copy[[sharedparents[i]]][[1]],decop[[sharedparents[i]]][[1]]))) decop[[sharedparents[i]]][[1]] <- c(decop[[children[1]]][[1]],decop[[children[1]]][[2]])
        if (isTRUE(all.equal(copy[[sharedparents[i]]][[2]],decop[[sharedparents[i]]][[2]]))) decop[[sharedparents[i]]][[2]] <- c(decop[[children[1]]][[1]],decop[[children[1]]][[2]]) 
      }
  }
  }
  #draw it
  DrawDecopWithInput2(decop)
  return(decop)
}