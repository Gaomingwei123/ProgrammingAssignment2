## Put comments here that give an overall description of what your
## functions do

## The first function makeCacheMatrix should be able to create a 
## special "matrix", which can cache its inverse. The second function 
## computes the inverse of the special "matrix" returned by function
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

## Write a short comment describing this function

## This function creates a specail "matrix", which is really a list
## containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of a square matrix, which can be done by "solve" function in R
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list( set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse of 
## the matrix has already been computed. If so, it gets the mean from the 
## cache and skips the computation. Otherwise, it computes the inverse of 
## the data and sets the value of the inverse in the cache via the setinverse function.

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
