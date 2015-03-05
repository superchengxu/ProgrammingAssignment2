## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
## Write a short comment describing this function
#

#the function construct a environment?? output is a list contains 4 functions,used to get the original data, set the original data
# get the cached matrix and set the inverse to the cache.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(sol) m <<- sol
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#after running the above funcion, we will get a list as the input of the below funcion
#if it is the first running, we will calculate the inverse, that is ,not go into the IF segment.And save the inverse into the cach(another environment)using setinverse funciont
#if it is not the first running, we will get the inverse from cache(IF segment code)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
