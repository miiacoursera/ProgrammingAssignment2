## Creates a list that contains the following functions:
## 1. set, which sets the value of the matrix
## 2. get, which gets the value of the matrix
## 3. set_inverse, which sets the value of the inverse of the matrix
## 4. get_inverse, which gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) m <<- inverse
    get_inverse <- function() m
    
    list(set = set, 
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## Calculates the inverse of the specified matrix. 
## Checks if the inverse has already been calculated and if so,
## gets it from the cache. If the inverse has not been calculated yet, 
## calculates it and adds it to the cache.
cacheSolve <- function(x, ...) {
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$set_inverse(m)
    m
}

## Usage: 
## 1. Create matrix m.
## 2. Create cache: cache <- makeCacheMatrix(m)
## 3. Calculate inverse: cacheSolve(cache)
## 4. Recalculate inverse: cacheSolve(cache)
## In the output of the second call to cacheSolve the message "getting cached data" is printed.
