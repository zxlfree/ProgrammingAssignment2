## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
## set the inversed matrix into cache      
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## examine if the inverse exsits
        inv <- x$getInverse()
        ## if the inversed matrix exsits, then extract the matrix from cache
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if the inversed matrix doesn't exsits, then solve the equation
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
