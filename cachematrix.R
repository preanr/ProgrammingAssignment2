## This file contains functions to create a matrix object that can cache its 
## inverse value, as well as the function to calculate and cache said inverse

## This function builds the setters and getters for a matrix object that can 
## cache its inverse.
## I returns a list of setter and getter functions for the obj

makeCacheMatrix <- function(x = matrix()) {
        #make sure the input is a matrix
        if (class(x)!= "matrix"){
                message("Error! This function expects a matrix input")
                return()
        }
        
        #initialise inverse variable 
        inv <- NULL
        
        #setter function that is essentially used to reset the matrix obj
        set <- function(input_matrix){
                #pass the value of the new input matrix to parent env
                x <<- input_matrix
                #reset the inverse variable and pass to the parent env
                inv <<- NULL
        }
        #getter function for the matrix obj
        get <- function() x
        #setter function for the inverse of the matrix obj
        setinv <- function(inv_matrix) inv <<- inv_matrix
        #getter function for the inverse of the matrix obj
        getinv <- function() inv
        
        #return a list of the getter and setter functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## this function checks the matrix obj create by MakeCacheMatrix for non-NULL
## inverse. If it exists it returns the cached inverse and prints a message.
## If no inverse exists then it calculates the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## try to get the value of the inverse from the matrix obj
        inv <- x$getinv()
        ## check if non-null cached inverse exists
        if(!is.null(inv)){
                ## if so - then return the cached value
                message("Returning cached value...")
                return(inv)
        } else {
                ## otherwise, fetch the original matrix 
                myMatrix <- x$get()
                ## calculate the inverse
                inv <- solve(myMatrix)
                ## and store it in the cache for the matrix
                x$setinv(inv)
                ## return the inverse
                inv
        }
        
}
