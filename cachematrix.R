## creates and returns list with 4 elements:
## inv - variable for inverse matrix of x, set to NULL by default
## get - function for get matrix from this list
## setinv - function for store inverse matrix to inv variable
## getinv - function for get inverse variable

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix which is the inverse of matrix in x list
## uses getinv function of x, to get cached value. If it is null, comuputes 
##    inverse matric by solve function and store it in x using setinv function of x

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    inv
  } else {
    mtrx <- x$get()
    inv <- solve(mtrx,...)
    x$setinv(inv)
  }
  inv
}


## ------------   yet another more OOP approach -----------------

OOPMatrix<-function(x = matrix()) {
  inv <- NULL

  getInverse <- function() {
    if(is.null(inv)) {
      inv <<- solve(x)
    }
    inv
  }  
  
  list(getInverse = getInverse)
}

## TRY
##
## mm<-OOPMatrix(matrix(c(1,1,1, 3,4,2,5,1,4), nrow = 3, ncol = 3))
## mm$getInverse()



