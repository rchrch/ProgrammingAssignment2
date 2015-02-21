# The cachematrix.R module provides a cachable matrix generator method that
# allows for caching the inverse matrix solution after the first time it
# is solved.

# Cachable matrix solver factory function.  It takes an (optional)
# initial matrix and returns an object that has getter and setter functions
# for the original matrix and the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    # Private variable for holding the cached matrix value
    cachedInvMatrix <- NULL
    
    # Define functions to set and get the original matrix
    set <- function(new_matrix) {
        x <<- new_matrix
        cachedInvMatrix <<- NULL          # Reset cached value
    }
    get <- function() x

    # Define a function to get the inverse
    # Note: The reval argument may be set TRUE to force the inverse matrix to
    # be re-solved.  This is useful if the optional parameters to the solve
    # function are changed.
    getInverse <- function(reval=FALSE, ...) {
        if (is.null(cachedInvMatrix) || reval) {
            # Cached matrix not yet calculated (or caller requested a - do it now
            cachedInvMatrix <<- solve(x, ...)
        }
        cachedInvMatrix
    }

    # There is no 'setInverse' function.  Disallow external users from directly
    # modifying the inverse value of the matrix.
    list(set=set, get=get, getInverse=getInverse)
}


# This is a wrapper function around the 'solve' function.  It takes a cachable
# matrix object and returns the inverse matrix.  If the cached inverse matrix
# has not been calculated, then the calculation is made and set for the object.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x$getInverse()
}


