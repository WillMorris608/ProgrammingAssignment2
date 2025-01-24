## these functions take a matrix input, store it in a cache, 
## and return the inverse


## this function is used to calculate the inverse of squared and 
## non-squared matricies
makeCacheMatrix <- function(x = matrix()) {
    i = NULL ## initializes inverse as NULL
    set = function(y){
      x <<- y
      i <<- NULL
    }
    get <- function () x ## this function gets matrix x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function () i
    list (set = set, 
          get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## this function is used to obtain the cached data
cacheSolve <- function (x,...){ ## gets cached data
  i <- x$getinverse()
  if(!is.null(i)){   ## checks if inverse is null
    message("getting cached data")
    return (i)  ## returns inverse value
  }
  data <- x$get()
  i <- solve(data,... ) ## calculates inverse value
  x$setinverse(i)
  i  ## returns a matrix that is the inverse of x
}

