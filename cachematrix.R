## Matrix inversion is usually a costly computation. Therefore, the following pair of functions 
## make use of a cache containing the inverse of a matrix, in order not to calculate it repeatedly.

## The "makeCacheMatrix" function takes as an argument a numeric matrix called "x", and initializes (to NULL)
## its inverse matrix called "inv". It then defines 4 functions and returns a list containing them.
## This returned object (a list of 4 functions) can be considered as a special "matrix", because it is not
## a usual matrix object, but thanks to its 4 functions it can access and modify such a matrix.
## The 1st function "set", sets the special "matrix" to contain new values.
## The 2nd function "get", returns the special "matrix".
## The 3rd function "setinv", sets the stored inverse of the special "matrix".
## The 4th function "getinv", returns the stored inverse of the special "matrix".
## The "inv" object plays the role of a cache memory which stores its inverse matrix
## (if already calculated in the past). If a usual matrix object was used instead, then its inverse
## would unavoidably have to be recalculated everytime it was needed.

makeCacheMatrix <- function(x = matrix())
{
   inv <- NULL
   
   set <- function(y)
   {
      x <<- y
      inv <<- NULL
   }
   
   get <- function() x
   
   setinv <- function(inverse) inv <<- inverse
   
   getinv <- function() inv
   
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The "cacheSolve" function takes a special "matrix" (created with the "makeCacheMatrix" function) as an 
## argument, and retrieves the value of its "inv" object, which corresponds to its inverse matrix.
## It then checks if that value has indeed been already calculated or not. If it has, then "cacheSolve"
## simply returns it. If it hasn't, then "it "cacheSolve" computes it using the "solve" function, 
## sets it for future fetching, and then returs it.

cacheSolve <- function(x, ...)
{
        inv <- x$getinv()
        
        if (!is.null(inv))
        {
           message("getting cached data")
           return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        inv
}