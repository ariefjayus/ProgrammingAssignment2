## Put comments here that give an overall description of what your
## functions do 
## the functions is to cache the inverse of a matrix, so we don't need to compute it repeatedly.

## Write a short comment describing this function
## We can get 4 result by using this function
##      set: we set the value of the matrix that assigned 
##      get: we will get the value of the matrix that assigned
##      setinverse: we will calculate the inverse of the matrix that assigned
##      getinverse: we will get the result (inverse of the matrix that assigned



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## This functions is to calculate the inverse of the matrix. but, it will check wether the inverse is already calculated or not
## if the inverse was calculated, it will get the result and skip the calculation. Otherwise it will calculate,
## and sets the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
  
}
