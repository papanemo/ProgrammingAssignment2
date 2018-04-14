## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #set NULL to variable m
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolvem <- function(solvem) m <<- solvem
        getsolvem <- function() m
        list(set = set, get = get,
             setsolvem = setsolvem,
             getsolvem = getsolvem)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #Assign Matrix to variable m
         m <- x$getsolvem()
        #Check Matrix m already calculated inverse matrix or not, if yes return calculated inverse matrix 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #Get New Matrix to calculate inverse matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setsolvem(m)
        m
}
