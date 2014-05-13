## Put comments here that give an overall description of what your
## functions do


## The functions below create a vector pointing to a list of functions, and then caches its mean.

## makeCacheMatrix creates a "vector" that points to a list of functions:
##   set = sets matrix value if not yet defined
##   get = obtains matrix value
##   set_inv = computes matrix inverse
##   get_inv = obtains matrix inverse


## Write a short comment describing this function

## makeCacheMatrix creates a "vector" that points to a list of functions:
##   set = sets matrix value if not yet defined
##   get = obtains matrix value
##   set_inv = computes matrix inverse
##   get_inv = obtains matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {  x <<- y ; inv <<- NULL  }
  get <- function() x
  set_inv <- function(solve) inv <<- solve
  get_inv <- function() inv
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)}



## Write a short comment describing this function


## cacheSolve computes the inverse of the "vector" that makeCacheMatrix created using the 'solve' function.
## If inverse was already computed and its value exists in the cache, then it fetches its value from the cache.


cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) { message("getting cached data") ;  return(inv) } ## if calculated then return it
  data <- x$get() ## else calc it
  inv <- solve(data)
  x$set_inv(inv)   						
  inv
}

