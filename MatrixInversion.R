##This function is used to catche the inverse of a matrix
##We assume that the matrix will be a square invertible matrix

makeCacheMatrix <- function(x=matrix()){
  
  ##Initialize the values
  inverse <-NULL;
  
  get <- function() {
      x;
  }
  
  set <- function(y){
    
      ##Set the value of the matrix globally
      x <<- y;
    
      inverse <<- NULL;
  }
  
  getinverse <- function() {
      inverse;
  }
  
  setinverse <- function(inv){
    
      inverse <<- inv;
  }
  
  ##List of functions that are available to perform
  list(get = get, set = set, getinverse = getinverse, setinverse = setinverse);
}

## This function computes inveser of a matrix and return.
## If the inverse of a matrix is already calculated then
## return the value from cache.

cacheSolve <-function(x){
  
    inverse <- x$getinverse();

    ##If the inverse already available
    if(!is.null(inverse)){
      
      message("Returning cache value:");
      print(inverse);

      return(inverse);
    }
    
    ##For fresh data, calculate the inverse of the matrix
    data <- x$get();
    inverse <- solve(data);
    
    ##Set the value of the inverse into the cache
    x$setinverse(inverse);
    
    message("Returning inverse value after fresh calculation:");
    print(inverse);
    
    inverse;
}

##This function is added here with an intention to unit test
## the caching of the matrix inversion.

testTheInverse <- function(testMatrix){
  
    temp <- makeCacheMatrix(testMatrix);

    cacheSolve(temp);
  
    ## From this case onwards, the value should be taken
    ## from the cache and sent back.
    cacheSolve(temp);

    cacheSolve(temp);

}




