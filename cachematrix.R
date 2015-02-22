## Takes a matrix and creates a list of functions
##        set : sets the value of the matrix
##        get : returns the value of the matrix
## setinverse : sets the value of the inverse matrix
## getinverse : returns the value of the inverse matrix
##
## The inverse is stored in the internal variable 'inv'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                    ## initializes 'inv' to NULL, which indicates that no inverse has been set
  set <- function(y) {           ## sets the matrix 'x' to the argument 'y' given when set is called 
    x <<- y
    inv <<- NULL
  }
  get <- function() x            ## returns the matrix 'x'
  
  ## sets the inverse matrix 'inv' to the argument 'inverse' given when setinverse is called
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv   ## returns the inverse matrix 'inv'  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes a list 'x' created by the function makeCacheMatrix and returns a matrix 
## that is the inverse of the matrix data in 'x'. If the inverse has already been 
## calculated, it gets it from cache via x$getinverse function. Otherwise, calculates
## the inverse and sets the inverse matrix in 'x' by use of x$setinverse function.

cacheSolve <- function(x, ...) {

  ##get the variable 'inv' in 'x'
  inv <- x$getinverse() 
  
   
  if(!is.null(inv)) {                   ## check if inv is not NULL, which indicates that the inverse is already calculated
    message("getting cached data")
    return(inv)                         ## return the cached inverse
  }
  matrix_x <- x$get()                   ## inv is NULL, so the inverse is computed and stored in the variable 'inv' of 'x'
  inv <- solve(matrix_x, ...)
  x$setinverse(inv)
  inv
}
