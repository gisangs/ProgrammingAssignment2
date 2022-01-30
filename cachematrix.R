## This function creates a special "matrix" object 
## that can cache its inverse.

## creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                   # make inverse matrix NULL
        setMatrix <- function(y) {    # you can change original matrix
                x <<- y
                inv <<- NULL
        }

        getMatrix <- function() return(x)

        # from 'cacheSolve()' to 'makeCacheMartix()'
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() return(inv)
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.
## If the inverse has already been calculated
## (and the matrix has not changed),
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting CACHED data")
                return(inv)
        }

        ## computes the inverse of the special “matrix” 
        ## returned by 'makeCacheMatrix()' 
        mat <- x$getMatrix()
        inv <- solve(mat, ...)

        ## return inverse matrix to 'makeCacheMartix()' to cache
        x$setInverse(inv)

        return(inv)

}
