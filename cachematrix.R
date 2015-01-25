## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function is composed of member functions
# 1) Set 2)get 3) setinv 4) getinv


makeCacheMatrix <- function(x = matrix()) {
	    xinverse <- NULL
        set <- function(y) {
        	#setting deep assignment variables for matrix x
                x <<- y
            #Setting value of xinverse to NULL to overide current value if any    
                xinverse <<- NULL
        }
        #Function to retrieve  matrix x
        get <- function() x   

        # Function to set inverse of x with deep assignment since we want to override the previous value of xinverse
        setinv <- function(inv) xinverse <<- inv  

        #Function to retrieve x inverse 
        getinv <- function() xinverse

        #Creating list to call "child" functions (eg. x$getinv())
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function
# Retrieve inverse of the matrix if it is set 
# If inverse is not set, it will calculate the inverse using solve function and store it  in "xinverse" variable 
#Notice the argument "..." which specifies that number of arguments is unknown after x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #Calling getinv function from the list
         xinverse <- x$getinv()
        if(!is.null(xinverse)) {
                message("getting cached data")
                #Return value of xinverse since it already set. 
                #Function exists as soon as it reads return statement and no further processing takes place
                return(xinverse)
        }
        
        # Store matrix x in data variable retrieved from the makeCacheMatrix member function get()
        data <- x$get()
        #Solving the matrix if xinverse is not already set
        xinverse <- solve(data, ...)
        #Setting(storing) new value of inverse x in xinverse via member of funtion of makeCacheMatrix function. 
        x$setinv(xinverse)
        xinverse
}
