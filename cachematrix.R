## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mI <- matrix(nrow=dim(x)[1],ncol=dim(x)[2])
  set <- function(my) {
    x <<- my
    mI <<- matrix(nrow=dim(x)[1],ncol=dim(x)[2])
  }
  get <- function() x
  setInv <- function(matInv) mI <<- matInv
  getInv <- function() mI
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mI <- x$getInv()
  if(!all(is.na(mI))) {
    message("getting cached matrix")
    return(mI)
  }
  data <- x$get()
  mI <- solve(data)
  x$setInv(mI)
  mI
  
}
