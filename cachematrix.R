# These functions compute and cache the inverse of a given matrix  
# Caching allows for faster computation, because it can be costly 
# to find the inverse of matrices with larger dimensions
# They make the assumption that the matrix is invertible



# This function returns a list containing setters and getters for
# The cached matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
      }
      
	get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
	
  
}


# This function takes the list returned by makeCacheMatrix(M) and
# returns its inverse. If the operation has already been performed,
# it will simply return the cached inverse matrix, otherwise
# it will solve the matrix and return its inverse.



cacheSolve <- function(x=matrix(), ...) {

	m <- x$getInverse()            
    
      if(!is.null(m)) {
		print("Fetching cached data. . .")
		return(m)
      }
    
      this_matrix <- x$get() 	
      m <- solve(this_matrix, ...)
    
	x$setInverse(m)
      m
      
}  

M = matrix(
	c(1,0,5,2,1,6,3,4,0),
	nrow = 3,
	ncol = 3)


#An example of this code that shows functionality on a 3x3 matrix
mCache <- makeCacheMatrix(M)
invM <- cacheSolve(mCache)
M
invM
invM%*%M 
