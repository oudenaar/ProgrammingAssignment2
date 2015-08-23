## Creating of cachable matrix with Inverse placeholder.
# calculating inverse matrix is expensive, this special function create a cachable object

## Here we give an example of calculating the inverse of a matrix and show the usages of this special functions:
#
# B = matrix( c(4,2,7,6),nrow=2, ncol=2)
# C <- makeCacheMatrix(B)
# Binv <- cacheSolve(C)

## Calculating true matrix multiplication:
# B %*% Binv 
#
# must be I identity matrix (ones on diagonal, rest zeros)
#
#[,1] [,2]
# [1,]    1    0
# [2,]    0    1
#

## Create a cached matrix, with inverse matrix placeholder
makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL
	set <- function(z) {
	   x <<- z
	   m <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverseMatrix <<- inv
	getinverse <- function() inverseMatrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculating the inverse Matrix of the special cached Matrix object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
