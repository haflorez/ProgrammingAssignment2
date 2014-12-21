## This functions allows to calculate the inverse of a square matrix
## and store it in cache in order to only calculate it once.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(input_matrix = matrix()){
        inversed_matrix <- NULL
        setmatrix <- function(y){
                input_matrix <<- y
                inversed_matrix <<- NULL
        }
        getvalue <- function() input_matrix
        getinverse <- function() inversed_matrix
        setinverse <- function(solved){
                inversed_matrix <<- solved  
        } 
        list(setmatrix = setmatrix,
             getvalue = getvalue,
             getinverse = getinverse,
             setinverse = setinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix via the function solve and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(input_matrix, ...){
        ## Return a matrix that is the inverse of 'x'
        inversed <- input_matrix$getinverse()
        if(!is.null(inversed)){
                message("getting cached data")
                return(inversed)
        }
        temporal_matrix <- input_matrix$getvalue()
        inversed <- solve(temporal_matrix)
        input_matrix$setinverse(inversed)
        inversed
}
