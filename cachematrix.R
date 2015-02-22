## Matrix inversion is usually a costly computation.
## Caching the inverse of a matrix rather than compute it repeatedly
## may yield potential benefits, particularly for large matrices. 
## The following functions cache the inverse of a matrix.

## The first function makeCacheMatrix creates a special "matrix" vector, and
## caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(x) m <<- solve (x)
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
    
}


## The second function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix ()) {
        ## Return a matrix that is the inverse of 'x'
  
  x.list <- makeCacheMatrix (x)
  
  m <- x.list$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x.list$get()
  
  m <- solve(data)
  
  x.list$setinverse (m)
  
  m
  
}
