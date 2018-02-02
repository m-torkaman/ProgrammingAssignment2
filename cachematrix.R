## These functions cache the inverse of a square invertable matrix

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y=matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setrev <- function(inverse) m <<- inverse
        getrev <- function() m
        list(set = set, get = get,
             setrev = setrev,
             getrev = getrev)

}


## This function computes the inverse of the special "matrix" returned by  
## makeCacheMatrix above. Retreive the inversed matrix, if already calcualated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getrev()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setrev(m)
        m
}
