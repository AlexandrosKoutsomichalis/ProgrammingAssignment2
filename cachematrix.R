## https://github.com/xiangxing98/ProgrammingAssignment2.git
## 1st commit SHA-1 hash identifier: b05b5096e135ae3ae46020952cc914cbce168841
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix
## makeCacheMatrix function creates a "matrix"
## which is a list containing a function to
## -set the value of the matrix
##-get the value of the matrix
##-get the value of the inverse of the matrix
## -set the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
x <<- y
i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(
set = set,
get = get,
setinverse = setinverse,
getinverse = getinverse)
}
## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.
## If so, it gets the inverse from the cache and skips the computation.
## Or it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
i <- x$getinverse()
if(!is.null(i)) {
message("getting cached data")
return(i)
}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
}