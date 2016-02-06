##The following pair of functions permit the computation/retrieval of the inverse of a 
##given matrix. Particularly, if the inverse of the matrix has already been compluted, it is
##simply retrieved from the cache, otherwise it is calculated.

##The following function creates a matrix object that caches its inverse.It's been constructed
##using the "makeVector" function as a template entering minor changes as the replacement of the
## "mean" function with the "solve" one.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The following function calculates the inverse of a given matrix if it has not already been
##computed or alternatively retrieves it from the cache.Also, it's been constructed using the
##"makeCacheMatrix" function as a template.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
##A simlpe implementation of the above functions follows :
##>y<-matrix(1:4, 2, 2)
##>B<-makeCacheMatrix(y)
##> cacheSolve(B)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##When executed at a second time retrieves the inverse from the cache:
##> cacheSolve(B)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
