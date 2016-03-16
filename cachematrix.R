#Creates a Matrix object where the solution to solve() is cached

#makeCacheMatrix() 
#Returns a list containing four functions
#set(Matrix) - sets the Matrix and clears the cache
#Matrix <- get() returns the value of the Matrix 
#setSolve(Matrix) sets the value of Sove for the Matrix loaded with set()
#Matrix <- getSolve() returns the value of set with setSolve()


makeCacheMatrix <- function(theMatrix = matrix()) {
  theSolve <- NULL

  set <- function(aMatrix) {
    theMatrix <<- aMatrix
    theSolve <<- NULL
  }

  get <- function() theMatrix
  
  setSolve <- function(aSolve) theSolve <<- aSolve
  
  getSolve <- function() theSolve
  
  list(set = set,get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  

}



# caschSolve()
# returns the value of Solve() given a Matrix Object created with makeCacheMatrix
#checks to see if Solve variable is set if so, returns value, if not claculates and sets it
#options to solve function can be provided in argument list after the Matrix Object 
cacheSolve <- function(mObj, ...) {
        ## Return a matrix that is the inverse of 'x'
  aSolve <- mObj$getSolve()
  if(!is.null(aSolve)) {
    message("getting cached data")
    return(aSolve)
  }
  aMatrix <- mObj$get()
  aSolve <- solve(aMatrix, ...)
  mObj$setSolve(aSolve)
  aSolve

}
