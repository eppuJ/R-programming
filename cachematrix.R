## There are two functions created in this script. makeCacheMatrix creates  a
## matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }

    get <-function()x
    setInverse <- function(inverse) inv<<-inverse
    getInverse <- function() inv
    list(set=set, get=get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## cacheSolve caclucates the Inverse of the matrix crated with makeCacheMatrix
## function. First, it checks if the Inverse has already been calculated. If it
## is, it skips the computation. If it is not, then it caclulates the inverse of
## the matrix and sets the value of the inverse in the cache via the setInverse
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
