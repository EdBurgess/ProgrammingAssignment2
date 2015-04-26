## These two functions will first cache a matrix that you pass into the first
## function (makeCacheMatrix) along with functions for accessing it in a list,
## then take that list as input (cacheSolve) and calculate and store the 
## inverse of the cached matrix.  If the inverse of the cached matrix has 
## already been calculated, a second call to cacheSolve will return the 
## stored value of the inverse matrix.
## 

## This function takes a matrix as input and stores it in an environment along
## with functions to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function takes a list functions to get and set a matrix and its inverse
## and returns the inverse.  If the inverse has already been calculated and is
## stored in "m" then the inverse is returned without being recalculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
