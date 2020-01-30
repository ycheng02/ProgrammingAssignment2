## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i<<-inv
        getinv <- function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat)
        x$setinv(i)
        i
}
