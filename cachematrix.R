  ## Put comments here that give an overall description of what your
  ## functions do
  
  ## makeCacheMatrix: This function creates a 
  ## special "matrix" object that can cache its inverse.
  
  makeCacheMatrix <- function(x = matrix()) {
    M <- NULL
    set <- function(y){
      x <<- y
      M <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) M <<- inverse
    getinverse <- function() M
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
  }
  
  
  ## This function computes the inverse of the 
  ## special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and 
  ## the matrix has not changed), then the cachesolve 
  ## should retrieve the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    M <- x$getinverse()
    if(!is.null(M)){
      message("getting cache data")
      return(M)
    }
    data <- x$get()
    M <- solve(data)
    x$setinverse(M)
    M
  }
