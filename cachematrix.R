## Coursera - Data Science 2 - Prog. Assignment 2
## Caching the Inverse of a Matrix

## makeCacheMatrix creates functions set, get, setinverse, getinverse and return as list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve delivers the inverse matrix. 
## On the first run it goes the else path and calculates the inverse.
## On subsequent runs the inverse is simply returned


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

#### Tests Matrix

# ?solve()
# v <- 1:4
# v
# mat <- matrix(v,2,2)
# mat
# imat <- solve(mat)
# imat
# t <- solve(mat) %*% mat
# t
# 
# w <- 11:14
# mat2 <- matrix(w,2,2)
# mat2
# imat2 <- solve(mat2)
# imat2
# u <- solve(mat2) %*% mat2
# u
# 
# 
# a <- makeCacheMatrix (mat)
# a$get()
# a$set()
# a$getinverse()
# a$setmean()
# cacheSolve(a)
# a$set(mat2)
# 
# 
# 
# 
# 
