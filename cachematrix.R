## This was a challenging routine for me, not so much to modify the existing program to work. 
## but rather to truly understand how each of these functions operate and the logic stream behind the process.
## I can't recall covering any of these aspects of R via the actual training course. So I had to essentially 
## try to understand the two sample functions from the assignment on my own, and then apply similar logic 
## to these two funtions - as best as I understand them. 

## Function “makeCacheMatrix” creates a special matrix object 'x' that is expected to be 'inverse-able'. 
## makeCacheMatrix contains 4 functions: 
## -  set is a function that sets the matrix x stored in the main function and sets the inv variable to null.
## -  get is a function that returns the matrix x stored in the main function.
## -  setinverse is a function that sets the inv varaiable to be equal to the current instance of x. 
## -  getinverse is a function that returns the pre-calculated value of inv - which is set by setinverse. 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Function “cacheSolve” computes the inverse of the special “matrix” (which is the input of cacheSolve) 
## returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cacheSolve should retrieve the inverse from the cache. If the inverse has not 
## been calculated, data gets the matrix stored with makeCacheMatrix, inv calculates the inverse, and 
## x$setinverse(inv) stores it in the object inv in makeCacheMatrix.

cacheSolve <- function(x, ...) {
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
