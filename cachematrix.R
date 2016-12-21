makeCacheMatrix <- function(x = matrix()) {  #stores x as matrix and caches it
                                             #in parent enviroment
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {           #looks if cachesolve already has a
					   #value and calculates if not
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cache value:")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
