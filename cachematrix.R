## The functions written below read a inveriable matrix, calculate its inverse and store it in    
## cache for reuse using <<- oprerator. If same matrix is entered again it will return result from
## cache instead of again calculating it.

## The first function, makeCacheMatrix creates a special "vector", which is really 
## a list containing a function to, set the value of the matrix, get the value of the matrix,
## set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  invmat<-NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invmat <<- solve
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special "vector" created with the 
## above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
 invmat
}