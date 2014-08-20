## These functions are for the Coursera Rprog-006 course, Assigment 2
# They are used to create an matrix based object and to calculate the inverse of it.
# The differential is that the cacheSolve function caches the answer of the inverse, 
# which can be used to speed-up some repeating computation.

## This function takes as parameter a matrix and returns a new object.
# This new object can store (cache) values associated with the given Matrix.
# The cacheSolve function uses this caracteristic to cache the inverse of the given matrix.
makeCacheMatrix <- function(x = matrix()) {
    res <- NULL; #The result of the "computation" (NULL by default)
    set <- function(y) { #Used to update the "base" matrix
        x <<- y;
        res <<- NULL; #New matrix, so the result is void
    }
    get <- function() x; #Function to return the base matrix
    setResult <- function(newRes) res <<- newRes; #Function to update the result associated with the matrix
    getResult <- function() res; #Get the result stored, associated with this matrix
    list(set = set, get = get,
            setResult = setResult,
            getResult = getResult);
}


## Used in conjunction with the object returned from makeCacheMatrix to calculate 
# and cache the inverse of the given matrix
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    r <- x$getResult(); #Get the current result stored on the cacheMatrix
    if(!is.null(r)) { #if it is not null, its the value we are looking for
        message("getting cached data");
        return(r);
    } #we end here if the cached value was null
    data <- x$get(); #So get the original matrix
    r <- solve(data, ...); #calculate the inverse of it
    x$setResult(r); #Store the calculated value in the cacheMatrix, so it can be used latter
    r; #and return the calculated value
}
