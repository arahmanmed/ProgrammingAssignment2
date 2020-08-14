makeCacheMatrix <- function(x = matrix()) {
        #This function creates a special "matrix" object that can cache its inverse.
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat,...)
        x$setInverse(m)
        m
}
