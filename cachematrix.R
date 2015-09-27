## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Assuming that matrix is reversable, basically this function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
        {
                MatInv <- NULL
                set <- function(y) 
                {
                        x <<- y
                        MatInv <<- NULL
                }
                get <- function() 
                        x
                setInverse <- function(inverse) 
                        MatInv <<- inverse
                getInverse <- function() 
                        MatInv
                list(set = set,
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }
        


## Write a short comment describing this function
## This function finds the inverse of the matrix created in makeCacheMatrix function.
## Assuming that the matrix has not been changed, it retrives the inverse of the matrix calculated  from cache.
cacheSolve <- function(x, ...) 
        ## Return a matrix that is the inverse of 'x'
        {
                MatInv <- x$getInverse()
                
                if (!is.null(MatInv))
                        
                {
                        message("Here is the cached data: ")
                        return(MatInv)
                }
                Mtrx <- x$get()
                MatInv <- solve(Mtrx, ...)
                x$setInverse(MatInv)
                MatInv
        }



