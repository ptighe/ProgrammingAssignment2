## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Many thanks to Fu Sheng Wang (https://class.coursera.org/rprog-002/forum/thread?thread_id=696) for helping describe the original set of functions. This information was instrumental in completing the assignment. 

#This function, makeCacheMatrix, first initializes M to NULL, and then creates a list of functions which actually perform the work within the subsequent cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) M <<- solve
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## Write a short comment describing this function
## This function uses the matrix from makeCacheMatrix to calculate the inverse of the matrix. The core function is the solve() function, which is what serinverse is calling. If this has already been done, then !is.null(M) will be false, indicating there is cached data, which will be directly returned instead.

cacheSolve <- function(x, ...) {
  M <- x$getinverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setinverse(M)
  M
}
 


#Here is some test code.
a <- makeCacheMatrix(matrix(1:4,2))
a$get()
a$getinverse()
a$set(matrix(5:8,2))
a$get()
cacheSolve(a)
cacheSolve(a)
b = a$getinverse()
a$get() %*% b
