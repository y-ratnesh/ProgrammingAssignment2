## Put comments here that give an overall description of what your
## functions do

## Makes a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_sol <- NULL
        set <- function(y){
                x <<- y
                inv_sol <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) inv_sol <<- inverse
        getInverse <- function() inv_sol 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## Finds the inverse of the above matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_sol <- x$getInverse()
        if(!is.null(inv_sol)){
                return(inv_sol)
        }
        mat <- x$get()
        inv_sol <- solve(mat,...)
        x$setInverse(inv_sol)
        inv_sol
}
