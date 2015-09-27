## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly. The
## following two functions cache the inverse of a matrix.

## makeCacheMatrix is a function that creates a special 
## "vector", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # setting the value of theverse to NULL
  inv <- NULL
  # 1. set the value of the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # 2. get the value of the matrix
  get <- function() x
  # 3. set the inverse of the matrix
  setInverse <- function(solve) inv <<- solve
  # 4. get the inverse of the matrix
  getInverse <- function() inv
  # return a list containing the 4 functions defined above
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function that calculates the inverse
## of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverce from the cache
## and skips the computation. Otherwise, it calculates the
## inverse of the matrix and sets  the inverse in the cache 
## via the setInverse function (defined in makeCacheMatrix).

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInverse(inv)
        inv
}
