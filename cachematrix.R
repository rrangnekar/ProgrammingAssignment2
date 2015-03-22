## makeCacheMatrix accepts a matrix as argument x
## cacheSolve accepts an object of the matrix type created above
## and return the inverse of the matrix obtained using the solve() function
## If the inverse already exists, it returns that. If not, it computes the
## inverse and saves it internally
## 
## USAGE Example:
## > abc = makeCacheMatrix(x= rbind(c(1, -1/4), c(-1/4, 1)) )
## > abc$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## ## Note: Inverse is not cached
## > abc$getsolve()
## NULL
## > cacheSolve(abc)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## #############
## > abc$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## ## Note: Inverse is cached
## > abc$getsolve()
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


## Create a wrapper around a matrix object with these internal functions:
## set() - accepts a matrix and saves it
## get() - returns the saved matrix, if any
## setsolve() - creates the matrix inverse and saves it
## getsolve() - returns the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(z) m <<- z
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Accepts a matrix created by makeCacheMatrix and computes the inverse
## If the inverse is already cached, it returns that
## If not, it calls solve on the matrix and then setsolve() to save the matrix

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
