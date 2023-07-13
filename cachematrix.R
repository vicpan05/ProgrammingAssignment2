## a pair of functions that cache the inverse of a matrix

## a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set the matrix
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }

        ## get the matrix
        get <- function() {
                m
        }

        ## set the inverse
        setInverse <- function(inverse) {
                i <<- inverse
        }

        ## get the inverse
        setInverse <- function() {
                i
        }
                

        ## return methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

        ## the inverse of this matrix returned by makeCacheMatrix
        cacheSolve <- function(x, ...) {
               m <- x$getInverse() 
                if( !is.null(m) ) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data) %*% data
                x$setInverse(m)
                m
}
