# The first function allows you to create an object in wich you can save a matrix and its inverse matrix.
# The second function gets the inverse matrix from an object created with the first function.
# If it can find a cached inverse matrix, it returns it, otherwise it computes it and saves it in the object.



## makeCacheMatrix
## A function to create an object composed by a matrix and its inverse matrix.
makeCacheMatrix <- function(x = matrix()) {

    # Set the inverse of the matrix to NULL
    inv <- NULL

    # Functio nthat Assigns the new passed matrix to x. and the inverse to NULL.
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }

    # Function that returns the matrix
    get <- function() {
        x
    }

    # Function that assign the passed inverse matrix to the inv variable.
    setinv <- function(inverse) {
        inv <<- inverse
    }

    # Returns the inverse matrix.
    getinv <- function() {
        inv
    }

    # Return a list with the different methods.
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## cacheSolve
## Return the inverse matrix of a "makeCacheMatrix" object.
## If there is no cached one, it computes it and save it into the object.
cacheSolve <- function(x, ...) {

        # Get the inverse matrix from x
        inv <- x$getinv()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }

        # If it is NULL, compute it
        mat <- x$get()
        inv <- solve(mat, ...)

        # Save it
        x$setinv(inv)

        # Return it
        inv

}