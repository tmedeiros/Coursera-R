## This function creates a special "matrix" object that caches its inverse
##


makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inversematrix) inv <<- inversematrix
    getinverse <- function() inv
    list(set = set, get = get,
           setinverse = setinverse,
               getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...)
{    
    ## Get cached matrix inverse and store it in inv
    inv <- x$getinverse()

    ## If the inv exists, prints a message and return the 
    ## cached matrix inverse, and exit teh function
    inv
    if (!is.null(inv))
    {
        message("getting cached inverse matrix")
        return(inv)
    }

        ## If the inverse matrix had not been cached before,
        ## get the matrix, invert and cache it
        ## Then return the inverted matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}
