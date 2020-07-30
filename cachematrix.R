## Here  is a pair of functions that cache the 
## inverse of a  matrix  to reduce a computation

## The first function creates a "special" matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x<<-y
    inver <<- NULL
  }
  get <- function() x
  set_inver <- function(inverse)  inver <<- inverse
  get_inver <-function() inver
  list(set = set, get = get, 
       set_inver = set_inver,get_inver = get_inver)

}


## The second function calculates the inverse of the matrix
## returned by the makeCacheMatrix function above.
## If the inverse has already been computed, the cacheSolve
## retrieves the inverse from the  cache.
## Assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$get_inver()
    if (!is.null(inver)) {
      message("getting cached inverse")
      return (inver)
    }
    ## Compute the inverse if it has not been cached
    matr <- x$get()
    inver <- solve(matr,...) 
    x$set_inver(inver)
    inver
  }
