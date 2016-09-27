## PThe functions make a special matrix object that caches its inverse.
## makeCacheMatrix creates the special matrix object; cacheSole returns the
## inverse.

## makeCacheMatrix creates a special matrix object that caches its inverse

makeCacheMatrix <- function(A = matrix()) {
    Ainv <- NULL
    set <- function(B) {
        A <<- B
        Ainv <<- NULL
    }
    get <- function() A
    setInv <- function(Minv) Ainv <<- Minv
    getInv <- function() Ainv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve returns a matrix that is the inverse of x. If the inverse has
## already been calculated, it returns the inverse from the cache.

cacheSolve <- function(A, ...) {
    ## Return a matrix that is the inverse of 'A'
    Ainv <- A$getInv()
    if(!is.null(Ainv)) {
        message("getting cached data")
        return(Ainv)
    }
    M <- A$get()
    Ainv <- solve(M, ...)
    A$setInv(Ainv)
    Ainv
}
