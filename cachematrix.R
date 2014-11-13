## Pair of functions to calculate the inverse of a matrix
## and get/retrieve inverse from the cache if it exists

## Create a special "matrix" containing a list of functions to
## 1. set (cache) the matrix
## 2. get the matrix
## 3. set (cache) the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) I <<- inv
    getinverse <- function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}    


## Calculate the inverse of the matrix and cache it
## ... or retrieve cached value if it exists
cacheSolve <- function(x, ...) {
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I<- solve(data, ...)
    x$setinverse(I)
    I ## Return a matrix that is the inverse of 'x'
}
