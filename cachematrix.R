- ## Put comments here that give an overall description of what your
  -## functions do
  +## These functions are being written for the completion of Programming Assignment 2 in accordance with the partial completion of Courseera Data Science Specialization : R Programming
  +## Programming Assignment 2; Week 3, GitHub user: kgujjula0426
  
  -## Write a short comment describing this function
  
  makeCacheMatrix <- function(x = matrix())
    +## The above function is used to create a special "matrix" object that can cache its inverse
    
    +makeCacheMatrix <- function(x=matrix()) {  ## defining the argument with a default mode of "matrix"}
      +  inv <- NULL                              ## initializing inverse as NULL; holds the value of matrix value
      +  set <- function(y) {                     ## define set function to assign new
        +  x <<- y                                  ## value of matrix in parent environment
        +  inv <<- NULL                             ## if a new matrix is there, inverse it to NULL
      }
      
      get <- function()x                           ## define the get function - returns the value of the matrix argument
      +
        + setinverse <- function(inverse) inv<<-inverse   ## assign inverse value in parent
      + getinverse <- function() inv               ## returns the inv value
      + list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## needed inorder to refer to functions with $ operator
    }
    
    ## Write a short comment describing this function
    +## This particular function can be used to compute the inverse of the special "matrix" returned by makeCacheMatrix above.
      +## If the inverse is already calculated (and the matrix didn't change),
      +## then inverse is retrived by cacheSolve from the cache
      
      cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        +    inv <- x$getinverse()
        +    if(!is.null(inv)) {                                         ## If inverse matrix is null
          +    message("getting cached invertible matrix with data")       ## Type Message : "getting cached invertible matrix with data"
          +    return(inv)                                             ## Return invertible matrix value
        }
        
        - ## If the value of expected invertible matrix is NULL then
          
          +    data <- x$get()                     ## get the original matrix data
          +    inv <- solve(data, ...)             ## solve function 
          +    x$setinverse(inv)                   ## set the invertible
          +    inv
          + ## Return a matrix which is inverse of x
    }
      