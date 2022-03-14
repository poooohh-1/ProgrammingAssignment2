## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                   #initializing inverse as NULL         
  set <- function(y) {                    
    x <<- y                             
    inv <<- NULL
  }                            

  get <- function() x           #function to get matrix x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {           #Checking whether inverse is NULL 
    message("getting cached data")
    return(inv)                 #returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)       #calculates inverse value
  x$setinverse(inv)
  inv                           #Returns a value that is the inverse of 'x'
  }
