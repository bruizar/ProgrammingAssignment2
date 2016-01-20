## Put comments here that give an overall description of what your
## functions do
####

# ?matrix
# a <- matrix(c(3, 1, 2, 3, 4, 5, 2, 6, 4), nrow = 3, ncol = 3)
# solve(a)




## Write a short comment describing this function
# makeCacheMatrix stores:
# [1] a matrix
# [2] an inverse of a matrix (cached)
# The function holds 4 sub-functions:
# [1] setMatrix		 sets the values for the matrix
# [2] getMatrix		 retrieves the matrix stored by setMatrix
# [3] cacheInverse	 caches the inverse of the matrix
# [4] getInverse	 retrieves the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # Initialize cache as an empty var. start with clean slate after calling makeCacheMatrix
  cache <- NULL
  
  # store matrix
  setMatrix <- function(value) {
    x <<- value
    # flush cash after matrix received a value
    cache <<- NULL
  }
  
  # return matrix x
  getMatrix <- function() x
  
  # cache the argument inverse
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value back
  getInverse <- function() cache
  
  # return list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       cacheInverse = cacheInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

# cacheSolve calculates the inverse of a matrix (for reference: https://www.youtube.com/watch?v=pKZyszzmyeQ)
# cacheSolve checks whether or not the inverse of the matrix has been calculated before or not
# if so, it simply returns the cached inverse without computing it
# if not, it computes, caches and returns it


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  # check if there is a value contained in var inverse
  if(!is.null(inverse)) {
  	# if it is, return it
    message("getting cached data")
    return(inverse)
  }
  # if not, get the matrix
  data <- x$getMatrix()
  # and calculate the inverse of the matrix using solve
  inverse <- solve(data, ...)
  x$cacheInverse(inverse)
  # return inverse of the matrix
  inverse
}