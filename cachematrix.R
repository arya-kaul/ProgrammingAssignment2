##makeCacheMatrix sets and gets the value of a matrix and then sets/gets the inverse of said matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(matrix) {
                x <<- matrix
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##cacheSolve gets the inverse of a matrix; if the inverse has already been computed it simply gets that and skips the computation. 
##If the inverse hasn't been computed it then computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
