# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of a matrix, it first checks if the inverse of the matrix is in the cache otherwise it computes the inverse and returns it

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data.")
        return(i)
        }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
