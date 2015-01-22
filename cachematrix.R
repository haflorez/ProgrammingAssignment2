## This functions allows to calculate the inverse of a square matrix
## and store it in cache in order to only calculate it once.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(inputmatrix = matrix()){
        inversedmatrix <- NULL
        setmatrix <- function(y){
                inputmatrix <<- y
                inversedmatrix <<- NULL
        }
        getvalue <- function() inputmatrix
        getinverse <- function() inversedmatrix
        setinverse <- function(solved){
                inversedmatrix <<- solved  
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
## for this function, there is the assumption that the supplied matriz
## is always invertible, so no checked if invertible is implemented.
cacheSolve <- function(inputmatrix, ...){
        ## Return a matrix that is the inverse of 'x'
        inversed <- inputmatrix$getinverse()
        if(!is.null(inversed)){
                message("getting cached data")
                return(inversed)
        }
        temporalmatrix <- inputmatrix$getvalue()
        inversed <- solve(temporalmatrix)
        inputmatrix$setinverse(inversed)
        inversed
}
