## Assignment : Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object
##  that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## instantiate matrix and inverse variables
    m.m <- x
    m.i <- NULL
        
    ## return matrix
    getmatrix <- function() m.m
    
    ## cache the matrix inverse
    setinverse <- function(inv) m.i <<- inv
    
    ## retrieve inverse from the cache
    getinverse <- function() m.i
    
    ## return list of "matrix" access functions
    list(getmatrix = getmatrix, setinverse = setinverse, 
         getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special
##  "matrix" returned by makeCacheMatrix above. If the inverse has
##  already been calculated (and the matrix has not changed), then the
##  cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    ## get the cached inverse
    invs <- x$getinverse()
    
    ## if cached inverse is not null, return it
    if(!is.null(invs)) {
        message("using cached inverse")
        return(invs)
    }
    ## otherwise, retrieve matrix and calculate inverse
    invs <- solve(x$getmatrix())
    
    ## cache calculated inverse in x
    x$setinverse(invs)
    
    ## Return a matrix that is the inverse of 'x'
    invs
}
