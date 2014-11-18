## Two main funcitons are shown below. The first one (makeCacheMatrix) creates a 
## matrix object 'x'. The function contains other functions as well that are later called
## by the cacheSolve() function.
## The second function mainly checks if there is a stored value for the inverse
## of the input matrix. If it doesnt find a value, it calculates it and
## stores it for later use. 


## Write a short comment describing this function
## makeCacheMatrix contains a set of 4 other funcitons (get(), set(),
## getinverse() and setinverse()) that can be called on the object that is created 
## in this function. Details on each subfunction are provided right next to each function.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL   ## i here is a local variable within the function and symbolizes the inverse
                ## which is set initially to NULL. 
                
    
    
    get <- function() {x}           ## returns the value of the orginal matrix
    
    
    setinverse <- function(inverse) {i <<- inverse}     ## this function sets the value of i as 
                                                        ## the input. This function gets called in
                                                        ## cacheSolve function to set the value of 
                                                        ## i outside of the function.
    
    
    getinverse <- function(){i}     ## This function returns the value of i. In the cacheSolve() 
                                    ## function, this is used to first check if there is a stored
                                    ## value for the inverse.
    
    
    set <- function(y){             ## This function sets the the value of the origrinal matrix 'x'
        x <<- y                     ## to the input matrix value 'y'. It also resets the value of 
        i <<- NULL                  ## i to NULL so that the cacheSolve() function no longer returns
    }                               ## a previously stored i value for the previous matrix
    
    
        
    list(get = get,                 
         set = set, 
         getinverse = getinverse,
         setinverse = setinverse)   ## the list acts as a guide to cacheSolve() funciton to access 
                                    ## the different internal functions within makeCacheMatrix()
}


## This function mainly checks if there is a value for the inverse
## of the input matrix stored somewhere. If it doesnt find a value, it calculates it and
## stores it for later use. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()         ## calls the getinverse() funciton on the object x that was made
                                ## in makeCacheMatrix()
    
    if(!is.null(i)){                        ## calls logical if statement to check if i is already
                                            ## in cache
        
        message("getting cached data")      ## if the value of i is present, a message is printed
        
        return(i)                           ## and the value of inverse 'i' is printed
    }
    
    data <- x$get()             ## if no value of i exists, the get() function is called on x 
                                ## and the returning value is assigend to a local variable (data) 
                                ## inside the funciton.
    
    i <- solve(data, ...)       ## the solve() is called on local variable 'data' and assigned to 'i'
                                ## inside the function
    
    x$setinverse(i)             ## setinverse() function is called on x and the calculated inverse 'i'
                                ## is assigned outside the function
    
    i                           ## 'i' is finally called and printed.
}
