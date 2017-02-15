## makeCacheMatrix creates an object that is used as input for cacheSolve. cacheSolve calculates the inverse of a matrix and stores it in cache to be retrieved next time.
## The input should be like this:
## > yourMatrix <- MakeCacheMatrix(<your matrix here>)
## > cacheSolve(yourMatrix)


## makeCacheMatrix creates a list of functions to set and get the values of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache_content <- NULL
  set <- function(y) {
    x <<- y
    cache_content <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cache_content <<- inverse
  getinverse <- function() cache_content
  list <- list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## cacheSolve checks if there was an inverse matrix stored in cache, in which case it returns it from cache, otherwise calculates it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache_content <- x$getinverse()
  if(!is.null(cache_content)) {
    message("getting cached data")
    return(cache_content)
  }
  data <- x$get()
  cache_content <- solve(data)
  x$setinverse(cache_content)
  cache_content
}
