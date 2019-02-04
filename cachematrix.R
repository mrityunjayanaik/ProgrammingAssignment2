## The below function gives the inverse of the square matrix and stores it in the cache and returns the inverse

## Create the function to calculate the inverse of the given square matrix and store the output in cache.

makeCacheMatrix <- function(x = matrix()) {


        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x #x is matrix that is the input argument
        setinverse <- function(inverse) invr <<- inverse #calls the inverse of the matrix to be calculated
        getinverse <- function() invr  #return the inverse of the input matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## Check whether the inverse of the matrix is already in the cache and return the output accordingly.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
