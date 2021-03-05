## Manage caching of matrices and their inverse to avoid unnecessary use of memory

## Create a cacheable matrix with setters and getters
makeCacheMatrix <- function(matr = matrix()) {
    inv <- NULL
    
    set <- function(m) {
        matr <<- m
        inv <<- NULL
    }
    
    get <- function() matr
    
    setInverse <- function(i) inv <<- i
    
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return a matrix that is the inverse of matr, from cache of possible
cacheSolve <- function(matr, ...) {
    inv <- matr$getInverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- matr$get()
    inv <- solve(data, ...)
    matr$setInverse(inv)
    inv
}