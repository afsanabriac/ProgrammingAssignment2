## makeCacheMatrix() and cacheSolve() receive a matrix, then they calculate its
## inverse if it was not calculated before. Otherwise, they look for it in cache.

## makeCacheMatrix() receives a numeric matrix, then creates an special list
## with many functions: the first sets the value of the matrix; second one gets
## the value; the third sets the inverse of the matrix; finally, the last one
## gets the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() receives the special list created by makeCacheMatrix(). If
## inverse was calculated before, it looks for it in cache. Otherwise, the
## function calculate the inverse and then saves it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        x$set(data)
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}