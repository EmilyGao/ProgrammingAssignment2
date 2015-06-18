## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Get a matrix as input to akeCacheMatrix function, and set its inverse, 
## for example:
## > a <- makeCacheMatrix()
## > a$set(matrix(1:4, nrow = 2))
## > a$get() returns the original matrix
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > a$setinverse(solve(matrix(1:4, nrow = 2)))
## > a$getinverse() returns the inverse of the original matrix
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## From a list which contains the objects returned from makeCacheMatrix function, 
## cacheSolve function will first check whether x$getinverse() returns a value, 
## if yes, print a message ""getting cached data" and display the cached value
## if no, calculate the inversed matrix and store it in x$setinverse(m)
## A example: 
## > cacheSolve(a)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
