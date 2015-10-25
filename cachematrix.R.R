## these functions will take a matrix, entered by the user in the makeCacheMatrix function, and will first store then matrix and enable caching.
## The cacheSolve function will return the inverse of the matrix stored using makeCacheMatrix. If an inverse already exists, the cacheSolve function will return the cached version. 

## This function creates a variable that holds all of the functions necessary to calculate and cache the inverse of the provided Matrix. 

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function (y) {
    x<<- y
    m<<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<- solve
  getinverse<- function() m
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}





## This function first assigns the getinverse() value to m. If the m value is NULL (the fist time the functions is called on "x")
## then it will calculate the inverse and cache that value. If the m value is not NULL, then this function returns the cached value. 

cacheSolve <- function(x, ...){
  m<- x$getinverse()
  if(!is.null(m)){
    message("getting chached data")
    return(m)
    
  }
  data<-x$get()
  m<- solve(data,...)
  x$setinverse(m)
  m
}