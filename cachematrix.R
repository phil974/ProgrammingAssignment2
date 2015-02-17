## Two functions for caching the inverse of a matrix 
## rather than compute it repeatedly 
 

## This function create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  mat <- x
  setinv <- function(the_inv) inv <<- the_inv
  getinv <- function() inv
  list(mat = mat,setinv = setinv,getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated, 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  the_inv <- x$getinv()
  if(is.null(the_inv)){
## to see that computation are only done once    
    message("compute matrix inverse")
    the_inv=solve(x$mat)
    x$setinv(the_inv)
  }
  the_inv
}
