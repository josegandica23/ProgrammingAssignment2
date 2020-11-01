## The following functions are used to create a special object that stores a 
## matrix and caches its inverse. 

##The first function, makeCacheMatrix creates a special “matrix”

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  
  set <- function(y) {
    
    x <<- y
    k <<- NULL
    
    
  }
  
  get <- function() x
  setinversa <- function(inversa) k <<- inversa
  getinversa <- function() k
  list(
    setinversa = setinversa,
    getinversa = getinversa,
    set = set,
    get = get
  )
  
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  k <- x$getinversa()
  if (!is.null(k)) {
    
    
    message("getting cached data")  
    return(k)
    
  }
  Pr <- x$get()
  k <- solve(Pr,)
  x$setinversa(k)
  k
}

