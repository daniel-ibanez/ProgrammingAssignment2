## Coursera R Programming - Assignment 2
## Pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        imatrix <- NULL #Initialise inverse matrix holder
        #Define set
        set <- function(y) {
                x <<- y
                imatrix <<- NULL
        }
        #Define get
        get <- function() x        
        #Define setinverse
        setinverse <- function(solve) imatrix <<- solve
        #Define getinverse
        getinverse <- function() imatrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        imatrix <- x$getinverse()
        if(!is.null(imatrix)) {
                message("getting cached data")
                return(imatrix)
        }
        matrixdata <- x$get()
        imatrix <- solve(matrixdata)
        x$setinverse(imatrix)
        imatrix   
        
}

## Sample run and test
##
## > m <- matrix(c(1, 2, 3, 4), 2,2)
## > x <- makeCacheMatrix(m)
## > x$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > inv <- cacheSolve(x)
## > inv
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > inv <- cacheSolve(x)
## getting cached data
## > inv
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
