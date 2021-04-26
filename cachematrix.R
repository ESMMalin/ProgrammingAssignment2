## The two functions below interact to create, cache, and solve a given matrix.
## The first function creates a matrix object and assigns a placeholder for the
## inverse of the matrix until it's calculated by the second function. The 
## second function checks to see if that placeholder is 'full' and either gets
## the cached inverted matrix or calculates and stores a new inverse matrix.

## Creates a generic matrix object and assigns a placeholder for the matrix's
## inverse until it is assigned a value by the second equation.

makeCacheMatrix <- function(x=matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
    }
    get<-function(){x}
    setInverse<-function(inverse){inv<<-inverse}
    getInverse<-function(){inv}
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## When the solution is not yet cached, this function solves the matrix 
## object created by the first function and then stores it. Otherwise, it 
## simply gets the stored solution.

cacheSolveMatrix<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data...")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}

## Note: the author used the sample matrix from Alan Berger to test each 
## function. https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg