## This set of functions calculates the inversion of square, invertible matricies.
## It will fail with non-invertible matrices
## It caches results, such that if it is fed a matrix it has previously calculated an inverse for,
## it will use the cached inverse and bypass calculation

## MakeCacheMatrix generates a list of functions that write and read matrices and their inverses.
## It does no calculation, just sets to and fetches from an outside environment (cache)

makeCacheMatrix <- function(x = matrix()) {
  MatInv <- NULL
  
  set <- function(y) {
    x <<- y
    MatInv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(solve) MatInv <<- solve
  
  getInv <- function() MatInv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cachesolve checks if the inverse for a matrix already exists in cache
##If it does, it returns the cached version to avoid unnecessary calculation
##Otherwise, it calculates it, saves that to cache, and returns it.

cacheSolve <- function(x, ...) {

  MatInv <- x$getInv() #Gets the inversion from the cached environment
  
  if(!is.null(MatInv)) { #if the cached inversion exists, it uses the cache and exits the function with return.

    message("getting cached data")
    return(MatInv)
    
  }
  
  data <- x$get()
  MatInv <- solve(data, ...)
  x$setInv(MatInv)
  
  MatInv
}
