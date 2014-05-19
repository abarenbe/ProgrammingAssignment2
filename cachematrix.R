# Returns a inverse of a matrix; when used in a loop cacheSolve solves for the inv the first time
# then caches the result. In future iterations it uses the cached result instead of solving again. 

## This function creates a vector that stores the inverse of x 
## This allows us to reuse the inverse of x without have to recalc it 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## this either gets the cached inverse of a matrix from makeCache Matrix or calculates it 
## and save it in the makeCacheMatrix vector for later use. 

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
