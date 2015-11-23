## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{

##This function is for set, get, set inverse and get inverse matrix methods 
cach_Inverse_Matrix <- NULL
	  set <- function(y) {
	    x <<- y
	    cach_Inverse_Matrix <<- NULL
	  }
	  get <- function() x
	  set_Inverse_Matrix <- function(inverse) cach_Inverse_Matrix <<- inverse
	  get_Inverse_Matrix <- function() cach_Inverse_Matrix
##now creating a list of all methods in function makeCacheMatrix
	  list(set = set, get = get,
	       set_Inverse_Matrix = set_Inverse_Matrix,
	       get_Inverse_Matrix = get_Inverse_Matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
## Method to check chached inverse matrix 
inv_Func <- x$get_Inverse_Matrix()
	  if(!is.null(inv_Func)) {
	    message("getting cached data")
	    return(inv_Func)
	  }
	  data <- x$get()
##method to compute new inverse matrix
	  inv_Func <- solve(data, ...)
	  x$set_Inverse_Matrix(inv_Func)
	  inv_Func
}
