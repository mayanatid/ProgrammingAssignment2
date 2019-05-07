## Put comments here that give an overall description of what your
## functions do
## Adding comments

## 'makeCacheMatrix' initializes the makeCacheMatrix 'object' using the matrix
## given in the function argument

makeCacheMatrix <- function(x = matrix()) {
          ## Initialize an empty variable for the inverse
          inv <- NULL
            
          ## 'set' creates a setter function which can be used to set 'x'
          ## (a matrix which has a value in the parent scope i.e. makeCachMatrix)
          ## to a new value. It also sets the 'inv' variable (also in parent scope)
          ## to NULL
          set <- function(y) {
              x <<- y
              inv <<- NULL
          }
          
          ## 'get' creates a getter function for the current value of 'x'
          get <- function() x
          
          ## 'setinverse' creates a setter function which takes an argument
          ## (i.e. 'inverse') and assigns it to the 'inv' variable (in the parent scope). 
          ## this function will be used cacheSolve function to store 
          ## the inverse calculated there
          setinverse <- function(inverse) inv <<-inverse
          
          ## 'getinverse', like 'get' retrieves the current value of 'inv'
          getinverse <- function() inv
          
          ## the returned list below allows other environments to access the values 
          ## within makeCacheMatrix using names
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## 'cacheSolve' takes a 'makeCacheMatrix' object as an argument and checks if an inverse
## has been calculated. If it has (i.e. the 'inv' parameter is not set to NULL) then that 
## value is returned. If not, it is calculated and then stored in the object

cacheSolve <- function(x, ...) {
          ## Get 'inv' parameter from 'x'
          inv <- x$getinverse()
          
          ## Check if an inverse value exists in 'x'
          if(!is.null(inv)){
                    ## if yes, retrieve and return this value
                    message('retrieving cached inverse')
                    return(inv)
          }
          
          ## if an inverse value isn't found, get the matrix from 'x'...
          matrix <- x$get()
          
          ## ... then calculate its inverse...
          inv <- solve(matrix)
          
          ## ...and store it back in 'x's 'inv' parameter...
          x$setinverse(inv)
          
          ##... and finally display to user
          inv
          
}
