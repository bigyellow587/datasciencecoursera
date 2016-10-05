# datasciencecoursera
## Put comments here that give an overall description of what your
## functions do
## This functions are used to invert a matrix and cache the result after the
## first run

## Write a short comment describing this function
## The function makeCacheMatrix will take an invertible matrix as argument and 
## it will cache the result if exists

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        i<-NULL
        
        ## set the matrix
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        
        ## get the matrix
        get <- function() {
                x
        }
        
        ## set the inverse of the matrix
        setInverse <- function(inv) {
                i <<- inv
        }
        
        ## get the inverse of the matrix
        getInverse <- function() {
                i
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## The cacheSolve function will check if the inverted matrix has already been cached.
## If not it will invert the matrix passed from the makeCacheMatrix function and 
## it will save for later use (if the matrix didn't change)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        ## Return the inverse if exists in cache
        ## or print that the data are not from the cache
        if( !is.null(i) ) {
                message("getting cached data")
                return(i)
        }
        else {
                message("not cached")
        }
        
        ## Get the matrix from our object
        my_matrix <- x$get()
        
        ## Calculate the inverse 
        i <- solve(my_matrix) %*% my_matrix
        
        ## Set the inverse to the object
        x$setInverse(i)
        
        ## Return the matrix
        i
}
