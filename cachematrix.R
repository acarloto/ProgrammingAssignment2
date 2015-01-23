## This solution includes two functions that work in association to compute, 
## in an efficient way, the inverse of a matrix.

## This first function receives a matrix and caches it, creating a special "matrix" 
## wich is a list containing a function to a) set the matrix, 
## b) get the matrix , c) set the inverse matrix and
## d) get the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
	  i_matrix <- NULL
      set <- function(y) {
            x <<- y
            i_matrix <<- NULL
            
      }
      get <- function() x
      setinverse <- function(solve) i_matrix <<- solve
      getinverse <- function() i_matrix
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This second function computes the inverse of the special
## matrix returned by the previous function above. If the inverse has
## already been calculated (and the matrix has not changed), then
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      i_matrix <- x$getinverse()
      if(!is.null(i_matrix)) {
            message("get cached data")
            return(i_matrix)
      }
      data <- x$get()
      i_matrix <- solve(data, ...)
      x$setinverse(i_matrix)
      i_matrix
}
