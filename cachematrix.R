## The following function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The following function computes the inverse of the matrix
## returned from the funtion above

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
## Sample Run
## x <- rbind(c(1, 2), c(3, 4))
## m = makeCacheMatrix(x)
## m$get()
##     [,1] [,2]
##[1,]    1    2
##[2,]    3    4
## cacheSolve(m)
##     [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5