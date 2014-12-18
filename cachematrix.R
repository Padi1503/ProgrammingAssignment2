## Functions to cache the inverse of an invertible matrix

#makeCacheMatrix: function creates "matrix" object that can cache its inverse

makeCacheMatrix = function(X = matrix()){       # input is a matrix X
    s <- NULL                                   # s is the inverse of matrix X, first set s to 0
    set <- function(Y){                         
        X <<- Y
        s <<- NULL                              # store new input matrix Y as X and set s to 0
    }
    get <- function() x                         # return the input matrix X
    setinverse <- function(solve) s <<- solve   # store inverse in s 
    getinverse <- function() s                  # return the inverse s
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)               # creates list with output of functions
}

#cacheSolve: computes the inverse of the "matrix" returned by makeCacheMatrix.
#If the inverse has already been calculated then it retrieves the inverse from the cache.

cacheSolve = function(X,...) {                  # input is the list of makeCacheMatrix
    s <- X$getinverse()                         # return cached inverse s of matrix X 
    if(!is.null(s)){                            # if cached inverse is already calculated (is not 0)
        message("getting cached inverse")       # then return message and inverse s and stop function 
        return(s)
    }
    matrix <- X$get()                           # if not then get matrix X
    s <- solve(matrix,...)                      # calculate the inverse s
    X$setinverse(s)                             # and store it
    s                                           # return inverse s
}