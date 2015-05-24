## The first function, makeCacheMatrix, does the following;
## makes a matrix that is invertible, and then defines functions that
## will store the value of the inverse of that matrix once it has been
## computed using the CacheSolve function, defined in part 2.
## The second function, looks for the cached value using the functions
## defined in makeCacheMatrix, returns the inverse if the cache value is 
## found. If the value cannot be found, it computes the inverse of the 
## matrix from the function makeCacheMatrix using the solve function
## and returns it. 

## caches the value of the inverse of a matrix and creates a matrix that 
## has to be invertible

makeCacheMatrix <- function(x = matrix()) { 
        #note, must define something as square invertible matrix,
        # example; a <- makeCacheMatrix(matrix(c(1,3,3,5),2,2))
        invert = NULL
        #makes invert undefined
        set = function(y){
                x <<- y
                #assigns x as y
                invert <<- NULL
        }
        get = function() x
        # retrieves the matrix
        setinverse = function(inverse)
                invert <<- inverse
        getinverse = function() invert
        list(set = set,get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        # a list that puts the created functions into the working environment
}


## looks for the cached value of the inverse of the matrix found by 
## makeCacheMatrix, otherwise, calculates the inverse of the matrix using the
## solve function.

cacheSolve <- function(x, ...) {
        # x is the output of the makeCacheMatrix function, example would be;
        # cacheSolve(a), would return the inverse of the matrix(c(1,3,3,5),2,2)
        ## because that is was "a" is from the example in makeCacheMatrix()
        invert = x$getinverse()
        # uses the getinverse function through scoping to find an output
        if (!is.null(invert)){
                # if invert is not null aka has already been caluclated,
                # returns the inverse from the cached data
                message("getting cached data")
                return(invert)
        }
        
        matrix.data = x$get()
        #if the invert is null, then recalculate the inverse, 
        #by getting the data using scoping from the makeCacheMatrix function
        invert = solve(matrix.data, ...)
        #invert is now the solved created matrix from makeCacheMatrix 
        x$setinverse(invert)
        #sets the value of invert using the setinverse function via scoping
        return(invert)
}
