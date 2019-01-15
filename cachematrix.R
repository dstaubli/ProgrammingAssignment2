## Put comments here that give an overall description of what your
## functions allow to cache objects and to retrieve previously cached objects. The functions apply this feature
# to store inverted matrices to save time. If a matrix has already been inverted before, the cached result can be retrieved
# rather than recomputed. 


## Write a short comment describing this function

# This function takes a matrix as input. It returns a list with four functions, to (1) set the values of the input (x, which is a matrix),
# to (2) get the value of the input, to (3) set the values of the inverse, and to (4) get the value of the inverse (returns NULL if not yet computed) 
# The functions allow to cache objects and to retrieve previously cached objects.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv 
  return(list(set = set, get = get,
              setinv = setinv,
              getinv = getinv))
}


## Write a short comment describing this function

# The function takes the output list of the previous function as input. 
# It checks, first, if the 4th element (getinv) is null or has a cached value assigned to it.
# If it has a value assigned to it, it will return that value (the message ""Getting Cached Invertible Matrix" will appear in the consol)
# If it is null it will compute the inverse of the matrix stored in the 2nd element (get) of the input list. 
# in other words: it retrieves the cached value of the inverse if it exists, otherwhise, the inverse is computed. 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}


########################################
# test the functions
########################################

# generate some random (large 1000x1000) matrix
d <- 1000
matr <- matrix( rnorm(d*d,mean=0,sd=1), d, d) 

# 
matr_cache <- makeCacheMatrix(matr)

# first time: compute the inverse (has to be computed since it is not yet stored in the cache)
cacheSolve(matr_cache)
# second time: stored in cache, can be retrieved from there 
# (given the size of the matrix, on my conputer, you can already see the computation time saved)
cacheSolve(matr_cache)