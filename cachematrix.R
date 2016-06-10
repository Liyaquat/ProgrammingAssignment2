## We will use lexical scoping principles to cache and retrieve cached
## data in our functions

## This function will return a list of functions associated with 
## inversion of a matrix, x.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x<<-y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(i) inv <<- i
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
        
        
}


## This function will check to see if the Inverse Matrix is stored
## in the cache. if it is, it will return it right away, if not it will
## compute it using the functions in the list "matrixed" generated in the
## previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        return(inv)
}
