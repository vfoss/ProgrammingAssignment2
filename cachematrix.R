## makeCacheMatrix: This function creates a list which contains the input matrix
## (which will be cached) and can also store the inverse of that matrix after 
## the cacheSolve function is called. It contains functions for re-setting the
## cached matrix's contents, viewing the matrix, setting the inverse (not 
## recommended to do manually), and viewing the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Note that the input x must be a square invertible matrix.
        
        iom <- NULL                  ## iom is the inverse of the matrix
        set <- function(y) {         ## here you can set the matrix to be stored
                x <<- y
                iom <<- NULL     ## Also clears the cache when new matrix is set
        }
        get <- function() x          ## just returns the currently stored matrix
        setinverse <- function(inverse) iom <<- inverse  
                                     ## set the matrix's inverse
       
        getinverse <- function() iom         ## returns inverse of matrix 
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        iom <- x$getinverse() ## gets cached inverse of the matrix if it exists
        
        if(!is.null(iom)) {
                message("getting cached data")
                return(iom)
        }
        
        mymatrix <- x$get()          ## gets the cached matrix,
        iom <- solve(mymatrix, ...)  ## finally calculates the matrix's inverse,
        x$setinverse(iom)            ## then caches the result, which will be a
                             ## matrix with the same dimensions as the original,
        iom                          ## and returns the now cached inverse!
}


## Examples:

## > trialmatrix <- matrix(c(4,3,3,2),2,2)

## > testcache <- makeCacheMatrix(trialmatrix)

## > cacheSolve(testcache)
##       [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## > cacheSolve(testcache)
## getting cached data
##       [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## > testcache$set(matrix(c(1,3,2,4),2,2))

## > testcache$get()
##       [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## > cacheSolve(testcache)
##       [,1] [,2]
## [1,]  -2.0  1.0
## [2,]   1.5 -0.5

## > cacheSolve(testcache)
## getting cached data
##       [,1] [,2]
## [1,]  -2.0  1.0
## [2,]   1.5 -0.5

