RandomSets <- function (arr)
{
#arr= array consisting of the vertices of the graph
#vertices are indicated as 1,2,..
#there is set returned as NULL like no splitting at all

size <- length(arr) #number of elements
A <- vector() #creating empty vectors
B <- vector()

randarr <- sample(arr) #sample the element in the array

sizeA <- floor(runif(1, min=1, max=size)) #randomly choose the size of set A
sizeB <- size-sizeA #set B has n-sizeA size 

#if (sizeA==0) return(NULL) #in case A is empty set

for (i in 1:sizeA){ #the "sizeA" first elements goes to set A
  A[i] <- randarr[i]
}
for (i in 1:sizeB){ #the remaining to set B
  B[i] <- randarr[i+sizeA]
}
return(A) #Returns the set A, set B has the remaining vertices
}