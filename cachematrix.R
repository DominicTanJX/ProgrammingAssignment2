## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
## This function creates a list that contains four member functions: set, get, 
## setinverse and getinverse. The <<- assignment operator is used to assign a 
## value to an object in an environment that is different from the current 
## environment.

makeCacheMatrix <- function(x = matrix()) {
       
        m <- NULL
        set <- function(y) {
          x <<- y     ## sets the input matrix into the x matrix variable 
          m <<- NULL  
        }
        
        get <- function() x  ## returns the input matrix
        setinverse <- function(inverse) m <<- inverse  ## sets the inversed matrix
        getinverse <- function() m  ## returns the inversed matrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        m <- x$getinverse()   ## gets the inversed matrix object of x to be assigned to m
        
        ## checks if the m variable is cached by fulfilling the is not null statement before executing the following
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        
        data <- x$get()   ## gets the get function of the x object to be assigned to data variable
        m <- solve(data, ...)  ## solves the data by inversing the values in data variable and assigning it to the m variable
        x$setinverse(m)
        m  ## returns the value of the m variable
        
}
