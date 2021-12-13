## Put comments here that give an overall description of what your
## functions do

## This is our week 3 assignment of coursera R programming, Here we will be working on a functions to find an inverse.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## The <<- operator will be introduced in this assignment,
## which can be used to assign values to objects that are in environments other than the one currently being used.
## We will create a square matrix data structure and its inverse by using two functions specified below.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.




makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                  ## we have initialized inv as NULL and it will hold a value of matrix inverse 
  set <- function(y) {                    
  x <<- y                            
  inv <<- NULL                    
  }
  get <- function() x          ## This function is used to get matrix x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
  }




## As a result of creating the special matrix with the above function, the following function calculate its inverse.
## However, it first checks whether the inverse has already been computed.
## Otherwise, it calculates the inverse of the matrix and sets it in the cache with the setinverse function.
## If not, it retrieves the inverse from the cache and skips the computation.




cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

