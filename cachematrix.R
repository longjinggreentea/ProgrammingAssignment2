## Assignment 2 ##
## This pair of functions are used to catch the inverse 
## of a matrix. Assuming the matrix supplied here is 
## always invertible.

## makeCacheMatrix : This function creates a special 
## 'matrix' object that can catch its inverse.

makeCacheMatrix <- function(x = matrix()) {
              inv <- NULL
              set <- function(y) {
                    x <<- y
                    inv <<- NULL
                    }
              get <- function() x
              setinv <- function(inverse) inv <<- inverse
              getinv <- function() inv
              list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)
}


## cacheSolve : This function computes the inverse of the 
## special 'matrix' returned by makeCatchMatrix above.

## If the inverse has already been calculated, the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }## Retrieves the inverse 
                 ## from the catche and return a 
                 ## matrix that is the inverse of 'x'
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)  ## Return a matrix that is the 
                     ## inverse of 'x'
}

## Test on a matrix of 2 by 2 filled with 4 random 
## normal values.
n=4
set.seed(24681)
r = rnorm(n); m<-sqrt(n)
mat1 <- matrix(r, nrow = m, ncol = m);mat1
temp <- makeCatchMatrix(mat1)
cacheSolve(temp) ## The calculated inverse is returend.
cacheSolve(temp) ## Since the inverse is alreaday 
                 ## calculated, cacheSolve retrieves   
                 ## inverse from the catche. 
