
##R Programming - Week 3
#Assignment 2
#Amanda Powell
#Due 3/18/2019

###LEXICAL SCOPING
#write a pair of R functions that cache the inverse of a matrix
#assume the matrix supplied is always invertible




## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      a <- NULL
      set <- function(y) {
            x <<- y
            a <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) a <<- inverse
      getInverse <- function() a
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special matrix made by makeCacheMatrix above
## If inverse has already been calculated (and matrix hasn't changed), then it should retrieve inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      a <- x$getInverse()
      if(!is.null(a)) {
            message("getting cached data")
            return(a)
      }
      data <- x$get()
      a <- solve(data, ...)
      x$setInverse(a)
      a
}


##TESTING

X <- matrix(1:4,2,2)
X

X_cache <- makeCacheMatrix(X)

cacheSolve(X_cache)
