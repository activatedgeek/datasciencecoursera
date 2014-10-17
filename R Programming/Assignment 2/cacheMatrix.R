# This function handles the caching
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    # Inverse taken using the solve() function
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# This function must be called for computations
# Returns the resultant matrix
# Caches if result not found earlier
# Using solve() function
cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}