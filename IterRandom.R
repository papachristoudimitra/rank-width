IterRandom <- function(graph){
#finds the rank of 'iter' random decompositions
  start.time <- Sys.time() #keep track of time
  
n <- nrow(graph)  

data <- vector()
data <- list(list(0,0))
depth <- vector()
position <- vector()
rankwidth <- vector()

#iter= number of iterations I want
iter <- 1000

for (i in 1:iter){
  
  decop <- TreeDecomposition(n)
  rank <- RankWidthInput(decop,graph)
  
  #keep track of rank-width and decompositions
  #kepp rank-width in a vector
  
  if (i==1) { 
    data[[i]][[1]] <- rank
    data[[i]][[2]] <- decop  #data[[1]][[1]] is the rank-width of the
                             #decomposition in data[[1]][[2]
    rankwidth[1] <- rank
  } else {
    
    data[[i]]<-list(0,0)
    data[[i]][[1]] <- rank
    data[[i]][[2]] <- decop
    
    rankwidth[i] <- rank
    #rankwidth[i] is the rankwidth of decomposition in data[[i]][[2]]
    #we use a vector so we can use it as numeric
  }
}
#find minimum rank-width
minrank <- min(rankwidth)
#returns the position(s) of min in rankwidth
position <- which(rankwidth == min(rankwidth))
#draws a decomposition achieving the smallest rank
ep <- data[[position[1]]][[2]]
DrawDecopWithInput2(ep)

#keep track of time
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
return(minrank)
}