## cache the matrix and the inverse of the matrix

## create the a special vector, which is really the list containing function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## i will store the inverse of matrix x
    i <- NULL
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Calculate the inverse of the matrix
## first of all, it checks to see if the inverse has already been calculated and stored
## in the cache.  If yes, it returns it from the cache.
## Otherwise, it calculates the inverse of the matrix and store it in the cache via the 
## setInverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return (i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
