## Following two functions are used to solve a given invertible matrix's inverse, the first function is to cache 
## the input matrix and the inversed matrix, the second function is to get the cached inverse of the input matrix if it exists, or solve 
## from the input matrix

## makeCacheMatrix: create a list to cache input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve: get the inverse matrix from cache or solve it from input matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached inverse matrix")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setsolve(s)
    s
}
