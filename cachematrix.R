## The makeCacheMatrix function creates a list containing a function to
## set the value of the matrix, get the value of the matrix, 
## set the inverse of the matrix, get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

## Set value of Matrix
    i <- NULL
    set <- function(y) {
      x <<- y                  
      i <<- NULL
    }
    
## Get value of Matrix    
    get <- function() x
    
## Set Inverse of Matrix
    setinverse <- function(solve) i <<- inverse
    
## Get Inverse of Matrix
    getinverse <- function() i
    
## return list of above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
  

## The cacheSolve function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
## Use if statement to determine if inverse already exists, return 'cached' inverse if so
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
## Compute inverse where it does not already exist and return newly computed inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
