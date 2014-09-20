## Result of "makeCacheMatrix" <- list of 4 functions, to call each one use - "%makeCacheMatrix object name%"$"%function name%" 
## "set" <- assigns the given value of matrix to the variable x and set cached meaning of solve to NULL
## "get" <- returns the current matrix 
## "setsolve" <- calculates the solve value for current matrix
## "getsolve" <- returns the value of solve for current matrix (NULL if not yet calculated)

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Function "cacheSolve" return a matrix that is the inverse of 'x' 
## Works in two steps - if there is no cached value, it calculates and sets to the given makeCacheMatrix object

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}
