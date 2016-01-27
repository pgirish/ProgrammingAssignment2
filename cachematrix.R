## Matrix inversion is usually a costly computation and there may be some benefit to ##caching the inverse of a matrix rather than compute it repeatedly. In this project ##i am trying to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.


## The first function, makeVector creates a special “matrix”, which is really a list ##containing a function to

##set the  matrix
##get the  matrix
##set the inverse
##get the inverse

makeCacheMatrix <- function(x = matrix()) {

	 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() inv
        list(set = set, get = get,setinv = setinv, getinv = getinv)            
             
}


## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). ##If the inverse has already been calculated and the matrix has not changed, it’ll ##retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
         inv <- x$getinv()
	
	#if the inverse is calculated get it from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
	
	#if not, calculates the inverse
        resdata <- x$get()
        inv<- solve(resdata, ...)
        x$setinv(inv)
        return(inv)
}
