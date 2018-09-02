## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix object with functions as callable 
## attributes to preseve object values such as the inverse calculations. 

makeCacheMatrix <- function(x = matrix()) {
    #set the matrix
    invr <- matrix(nrow=nrow(x),ncol=ncol(x))
    setmat <- function(y){
        y <<- x
        invr <<- NULL
    }
    #get the matrix
    getmat <- function(){
        return(x)
    }
    
    #get the inverse calculation
    getSolve <- function() invr
    setSolve <- function(y) {
        invr <<- y
    }
    
    #output a list of functions 
    list(setmat = setmat, getmat = getmat,
         getSolve = getSolve,setSolve=setSolve )   
}


## Write a short comment describing this function
## cacheSolve function takes a matrix as an argument and calculates it's inverse
## using the solve() function. If the object already has the inverse calculated then
## it retrieves it, elese calculates it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    #if inverse doesn't exist calculate a new one
    invr <- x$getSolve()
    if(any(is.na(invr))){
        message("Getting cached data")
        invr <- solve(x$getmat())
        x$setSolve(invr)
        return(invr)
    }
    #else return inverse
    return(invr)
    
}
