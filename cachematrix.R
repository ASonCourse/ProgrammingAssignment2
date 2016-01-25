## makeCacheMatrix creates a list of functions that are going to be used
## for creating (the inverse of) a matrix, or retrieving a previously cached
## (inverse) matrix. The functions can be called as elements form a list:
## [listname]$set([matrix]), [listname]$get([matrix], [listname]$setinverse(
## [matrix]), and [listname]$getinverse([matrix]).


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cachesolve will retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}

# The functionality has been tested using the following commands:
#         
# > M <- matrix(c(10, 4, 8, 2, 5, 9, 6, 7, 1), 3, 3)
# > M
# 
# [,1] [,2] [,3]
# [1,]   10    2    6
# [2,]    4    5    7
# [3,]    8    9    1
# 
# > functionlist <- makeCacheMatrix()
# > functionlist$set(M)
# > cacheSolve(functionlist)
# 
# [,1]   [,2]   [,3]
# [1,]  0.116 -0.104  0.032
# [2,] -0.104  0.076  0.092
# [3,]  0.008  0.148 -0.084
# 
# > cacheSolve(functionlist)
# 
# getting cached data
# [,1]   [,2]   [,3]
# [1,]  0.116 -0.104  0.032
# [2,] -0.104  0.076  0.092
# [3,]  0.008  0.148 -0.084