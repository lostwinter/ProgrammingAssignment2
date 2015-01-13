# These are two functions to determine the inverse of the matrix x. 
# If the inverse of x is known, x is returned. This saves on computing power

##############################################################################
# makeCacheMatric creates a special R object that will: 
# 1. Initialize a variable called m. m will store the inverse matrix
# 2. Create the function get() to obtain the raw matrix 
# 3. Create the function setInverseMatrix() to assign the inverse matrix of x to m;
# 4. Creates the function getInverseMatrix() to retrieve the cached inverse matrix.

##########################################################################

makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        get <- function() x
        setInverseMatrix <- function(matrix) m <<- matrix
        getInverseMatrix <- function() m
        # return a list of functions as an R object
        list(
                get=get, 
                setInverseMatrix=setInverseMatrix, 
                getInverseMatrix=getInverseMatrix
        )
}

########################################################################

# This function makes the inverse of x. It will: 
# 1. Check if there is an inverse. 
# 2a. If yes, returns the result. 
# 2b. If no, it calculates the inverse.

########################################################################
cacheSolve <- function(x) {
        m <- x$getInverseMatrix()
        if(!is.null(m)){
                message("Cached data found.")
                return(m)
        }
        else {
                message("No cached data found. Here is the inverse.")
                data <- x$get() # get matrix 
                m <- solve(data) # find inverse 
                x$setImatrix(m) # assign to x
                return(m)
        }
}
