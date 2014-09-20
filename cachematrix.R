## cachematrix.R - Utility functions to deal with invertible matrices 
##  	These functions speed up the calculation of the inverse of a matrix
## 	by returning a cached copy if the inverse has already been found 
## 	this saves time in looping cases where the inverse of a matrix needs to 
## 	re-used several times.
## 
##   usage example :
##      M <- makeCacheMatrix() 'create the functions object returns list of functions' 
##   	M$get()                'returns empty matrix'
##      M$set(matrix(1:4,2,2)  'load a matrix'
##      M$get()                'returns the matrix'
##      cacheSolve(M)  	       'calculates and returns inverse' 
##      cacheSolve(M)          'returns cached copy of inverse'
##
## creates a list object that contains functions for working with cached matices
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
 	set <- function(y) {
            # store copy of matrix in x  
            x <<- y
            m <<- NULL
        }
        # get will returns the copy of the matrix 
        get <- function() x

        # set will set solves to find the inverse of the matrix
        setInverse <- function(solve) m <<- solve

        # get inverse returns the inverse of the matrix
        getInverse <- function() m

        # this defines the sub funtions created in a list
        list(set = set, get = get, 
                 setInverse = setInverse,
                 getInverse = getInverse)

}


## Solves to find a matrix inverse. If the inverse has already been found returns 
## a cached copy of the inverse. If inverse has not been found then inverse is found
## and that is returned. 

cacheSolve <- function(x, ...) {
        # returns the value of the inverse of the matrix
 	m <- x$getInverse()

        # if the inverse is already calculated not NULL then return it  
        if(!is.null(m)) {
             message("*** Returning Cached Inverse *** ")
	     return(m)
        }

        # Since inverse was not calculated need to find the inverse
        # use get to return the matirx
        message("*** Finding Inverse ***")
        data <- x$get()
        # use solve to find the inverse of the matrix 
        m <- solve(data, ...)
        # use setInverse to set the inverse        
        x$setInverse(m)
        # use getInverse to get inverse this will check setInverse
        m <- x$getInverse()
        #return the inverse of the matrix 
        return(m)

}
