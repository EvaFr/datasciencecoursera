#The first function, makeMatrix creates a special "matrix", which
#is really a list containing a function to
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse matrix
#   get the value of the inverse matrix

makeMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function calculates the inverse of the special "matrix" created with the above function. 
# It calculates the inverse of the matrix and sets the value of the inverse matrix in the cache 
# via the setinverse function.

cacheInv <- function(x, ...) {
    inv <- x$getinverse()
    t <- 3
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# ************SAMPLE RUNNING************
# x <- matrix(1:7, nrow=4, ncol=4)
# m = makeMatrix(x)
# m$get()
# cacheInv(m)

# Input data: x
# x <- matrix(1:7, nrow=4, ncol=4)
# x
#     [,1] [,2] [,3] [,4]
#[1,]    1    5    2    6
#[2,]    2    6    3    7
#[3,]    3    7    4    1
#[4,]    4    1    5    2

# m = makeMatrix(x)
# m$get()
#     [,1] [,2] [,3] [,4]
#[1,]    1    5    2    6
#[2,]    2    6    3    7
#[3,]    3    7    4    1
#[4,]    4    1    5    2


# cacheInv(m)
#           [,1]       [,2]       [,3]       [,4]
#[1,] -4.0000000  3.5714286 -0.1428571 -0.4285714
#[2,] -0.1428571  0.1428571  0.1428571 -0.1428571
#[3,]  3.2857143 -3.0000000  0.1428571  0.5714286
#[4,] -0.1428571  0.2857143 -0.1428571  0.0000000

# Retrieving from the cache in the second run
# cacheInv(m)
#           [,1]       [,2]       [,3]       [,4]
#[1,] -4.0000000  3.5714286 -0.1428571 -0.4285714
#[2,] -0.1428571  0.1428571  0.1428571 -0.1428571
#[3,]  3.2857143 -3.0000000  0.1428571  0.5714286
#[4,] -0.1428571  0.2857143 -0.1428571  0.0000000


# m0 <- cacheInv(m)
# round( x %*% m0, 0)

#      [,1] [,2] [,3] [,4]
# [1,]    1    0    0    0
# [2,]    0    1    0    0
# [3,]    0    0    1    0
# [4,]    0    0    0    1


