## This functions package should be used for acceleration in matrix calculations.
## In particular it save the previously computed result of the matrix inversion operation.

## This function is used to create object for matrix manipulations.
## In particular uses for getting, setting the matrix itself and the inversion value of it.

makeCacheMatrix <- function(x = matrix()) {
    invM  <-  NULL
    modidied  <- TRUE
    
    ## This function used for setting the value of the matrix.
    ## The set() method doing the same.
    setMatrix  <- function(newMatrix){
        x  <<- newMatrix
        invM <<- NULL
        modified  <<- TRUE
    }
    
    ## This function returns the object's matrix value.
    ## The get() method doing the same.
    getMatrix  <- function() x
    
    ## This method should be used to setting the inverse matrix value.
    ## The setInverse() method doing the same.
    setInv  <- function(newInv) {
        invM <<- newInv
        modified <<- FALSE
    }
    
    ## This method calculates the inverse matrix value.
    ## Warrning: reassings the value of the matrix inversion.
    inv  <- function(){
        invM  <<- solve(x)
        modified <<- FALSE
    }
    
    ## This method returns the object's value of the inverse matrix.
    ## The getInverse() method doing the same.
    ## Warrning: the value could differ from the real inverse matrix of the object. See setInv() specification! 
    getInv  <- function() invM
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         set = setMatrix,
         get = getMatrix,
         setInv = setInv,
         getInv = getInv,
         setInverse = setInv,
         getInverse = getInv,
         inv = inv,
         inverse = inv)
}


## This function returns the inversion matrix of the x, uses cached result or compute the new if necessary.

cacheSolve <- function(x, ...) {
    inv  <- x$getInverse()
    
    # Here function check the matrix for previously computed result and and original matrix 
    # modifications since last computation.
    if(!is.null(inv) && !x$modified){
        message("Getting cached data.")
        return(inv)
    }
    matrix  <- x$getMatrix()
    inv  <- solve(matrix)
    x$setInverse(inv)
    inv
}
