
#The first function, makeCacheMatrix creates a special "matrix" object that can 
#cache its inverse
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}

#The second function calculates the inverseof the special matrix created above.
#But it checks for a cached value, and uses it in case it is avaliable.

cacheSolve <- function(x, ...) {

    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
