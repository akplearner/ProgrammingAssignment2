## Put comments here that give an overall description of what your functions do
## Write a short comment describing this function

#------------------------------------------------------------

#makeCacheMatrix creates a list containing a function to
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # 1. set the value of the matrix
  set <- function(x) {
     x <<- y
   inv <<- NULL
  }
  #2. get the value of the matrix
  get <- function() x
  #3. set the value of inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  #4. get the value of inverse of the matrix
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#This function above give the inverse of the matrix by first checking if it was given before.
#if the inverse was given before, it gets result and stops process.
#otherwise it computes the inverse and sets the new value to the cache in setinverse function.




#The second function below performs inverse without checking if its invertible or not. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#testing1
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get() #the matrix I am working with
cacheSolve(my_matrix) #its inverse

#testing2
 x = rbind(c(1,3), c(2, 4))
 m = makeCacheMatrix(x)
m$get()#the matrix I am working with
cacheSolve(m)#its inverse



