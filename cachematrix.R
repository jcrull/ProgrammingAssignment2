## The two functions below can be used to cache the inverse of a matrix

## makeCacheMatrix creates a list which is used to cache the inverse 
## of the input matrix
## The list contains functions which
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse
## 4.get the inverse

## makeCacheMatrix input should be an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
        mx <- NULL
        ## 1.set the value of the matrix
        setmx <- function(y) {
                x <<- y
                mx <<- NULL
        }
        ## 2.get the value of the matrix
        getmx <- function() x
        ## 3.set the inverse
        setinverse <- function(matrixInverse) mx <<- matrixInverse
        ## 4. get the inverse
        getinverse <- function() mx
        ## Create the list
        list(set = setmx, get = getmx,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks to determine if the inverse of the 
## makecacheMatrix input matrix has been calculated
## if it has already been solved, then the inverse is retrieved from the cache
## if not, the function computes the inverse 

## the input for cacheSolve is the list ouput from the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Check to see if the inverse has been cached previously
        mx <- x$getinverse()
        ## If so, get the cached data
        if(!is.null(mx)) {
                message("getting cached data")
                return(mx)
        }
        ## Else calculate the inverse
        data <- x$get()
        mx <- solve(data, ...)
        ## set the inverse
        x$setinverse(mx)
        ##output the result
        mx
}

## test the code
z0 <- matrix(rnorm(100), nrow=10, ncol=10)
z1 <- makeCacheMatrix(z0)
cacheSolve(z1)
cacheSolve(z1)