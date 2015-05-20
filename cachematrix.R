## R Programming Assignment 2
## Yukai Zou
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## Define "makeCacheMatrix" as a function of "x", the default value of x should be a 1x1 matrix with a "NA" element
        inv <- NULL ## Initialize "inv" with NULL
        set <- function(y) { ## Define "set" as a function of "y"
                x <<- y ## Assign "y" to "x"
                inv <<- NULL ## Assign NULL to "inv"
        }
        get <- function() x ## Define "get" as a function, upon the call of which will display "x"
        setinv <- function(inverse) inv <<- inverse ## Define "setinv" as a function of "inverse", and assign "inverse" to "inv"
        getinv <- function() inv ## Define "getinv" as a function, upon the call of which will display "inv"
        list(set = set, get = get, setinv = setinv, getinv = getinv) ## Create a list containing four function: 1)set the value of the matrix; 2)get the value of the matrix; 3)set the value of the inverse of the matrix; 4)get the value of the inverse of the matrix.
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Define "cacheSolve" as a function of "x" and pass the unspecified parameters to this function
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() ## Assign the cached inverse matrix obtained from calling the "getinv" function of the list obtained by "makeCacheMatrix"
        if(!is.null(inv)){ ## Check the condition: the cached inverse matrix is not a NULL
                message("getting cached data") ## Print a message "getting cached data" at the console window
                return(inv) ## Return the cached inverse matrix 
        }
        data <- x$get() ## Assign the matrix obtained from calling the "get" function of the list obtained by "makeCacheMatrix"
        inv <- solve(data, ...) ## Calculate the inverse of the obtained matrix, with the unspecified parameters passing to the "solve" function
        x$setinv(inv) ## Call the "setinv" function to set the value of the inverse matrix
        inv ## Display the calculated inverse matrix in console window
}
