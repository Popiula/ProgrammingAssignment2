## The first function, makeCacheMatrix, creates a special "matrix", 
## which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of its inverse
## get the value of its inverse

## And then, the second function, cacheSolve, calculates the inverse of the special "matrix"
## created with the first function. It first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setInverse function.
########################################################################################

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## in order for this function to work, x needs to be a square matrix

makeCacheMatrix <- function(x = numeric()) { 
  i <- NULL       ## initializes i (x is initialized as a function argument)
  set <- function(y) {
    x <<- y       ## assigns the input argument y to the x object in the parent environment 
    i <<- NULL    ## resets the i object in the parent environment. This line of code clears 
    ## any value of i that had been cached by a prior execution of cacheSolve()
    ## if there is already an inverse matrix cached in i, whenever x is reset, the value of i
    ## cached in the memory of the object is cleared, forcing subsequent calls to cacheSolve() 
    ## to recalculate the inverse matrix rather than retrieving the value from cache.
    }
  get <- function() x ## getter for the matrix x
  setInverse <- function(solve) i <<- solve ## setter for the inverse matrix i
  getInverse <- function() i  ## getter for the inverse matrix i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ## assigns each of these functions as an element within a list(), 
                                ## and returns it to the parent environment.
}

## the function returns a fully formed object of type makeCacheMatrix().
## Each element in the returned list is named. 
## ------------------------------------------------------------------------------------------

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## x has to be a object created by makeCacheMatrix()
  i <- x$getInverse() ## The function attempts to retrieve an inverse matrix from 
  ## the object passed in as the argument. First, it calls the getInverse() function 
  ## on the input object.
  
  ## Then it checks to see whether the result is NULL. Since makeCacheMatrix() 
  ## sets the cached inverse matrix to NULL whenever a new matrix is set into the object, 
  ## if the value here is not equal to NULL, we have a valid, cached inverse matrix 
  ## and can return it to the parent environment
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    ## If the result of !is.null(m) is FALSE, cacheSolve() gets the matrix from the input 
    ## object, calculates an inverse matrix using solve(), uses the setInverse() function 
    ## on the input object to set the inverse matrix in the input object, and then returns 
    ## the inverse matrix to the parent environment by printing it.
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
## the function returns a matrix that is the inverse of 'x' 
## (x is the input matrix of makeCacheMatrix)
