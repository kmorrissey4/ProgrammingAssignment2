## Katherine Morrissey 
## 12-Jun-2016 
## Programming Assignment 2: Lexical Scoping

#############################################################################
############################## Description ##################################

## The first function, "makeCacheMatrix," takes in a matrix and creates a
## special "matrix" object that can cache its inverse. In order to use the 
## second function, "cacheSolve," it is recommended that the output of the
## first function be stored as an object (a <- makeCacheMatrix()). The second
## function takes the list from the first function (cacheSolve(a)) and outputs
## the inverse of the matrix from the first function. If the inverse is already
## solved and stored in the cache, it outputs a message and the stored inverse
## matrix.

#############################################################################
############ Function 1: MakeCacheMatrix ####################################

## This function creates a special "matrix" object that can cache its inverse.
## A matrix is input, and a list of elements is output.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse.matrix) i <<- inverse.matrix
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##############################################################################
############ Function 2: CacheSolve ##########################################

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cacheSolve" should retrieve the inverse from the cache.
## The list created above is input, and an inverse matrix is output.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

############################# END ############################################
