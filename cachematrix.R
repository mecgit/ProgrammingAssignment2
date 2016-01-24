## Implements a new type of matrix that improves the performance of 
##  reutnring its inverse by caching the original computation.

##
## Constructs a caching matrix from a user supplied invertable matrix.
##  Parameters: x - An invertable matrix.
##  Return: A list of setters and getters for the matrix and its cached inverse.
##
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(
        set=set,
        get=get,
        setSolve=setSolve,
        getSolve=getSolve
    )
}

##
## Return a matrix that is the inverse of 'x', computing and caching this value if required. 
##  Parameters: x   - An invertble inverse caching maxtrix created via makeCacheMatrix.
##              ... - Additional parameters to pass through to solve(), the function 
##                      which computes the inverse.
##  Return: The cached inverse of the matrix computed via solve(x,...)
##  
cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    # Compute the inverse.
    data <- x$get()
    s <- solve(data,...)
    x$setSolve(s)
    s
}
