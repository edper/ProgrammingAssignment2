## This program will demo the concept of Lexical Scoping
## using the <<- operators which allows you to store and alter
## values of an object from a parent function inside the
## enclosure (i.e. a function written inside a function)

## This function allows you to store and retrieve the matrix
## along with the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will return the inverse of a matrix
## if it already existed in memory otherwise it will be computed
## and set the value of the inverse matrix in MakeCacheMatrix function

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


