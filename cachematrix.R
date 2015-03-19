## This script containts two functions; one creates an object "matrix" in the form
## of a list capable of caching a matrix and its inverse, the second function will utilize
## this object.  If the object doesn't have an inverse stored for a matrix, the inverse is
## calculated and stored; if the object already holds the inverse of the matrix, the cached
## version is returned.



## this function creates an object in the form of a list.  The list contains getters and
## setters to be able to store and retrieve a matrix and/or its inverse.
## this is basically just the constructor to create the desired structure
makeCacheMatrix <- function(x = matrix()) {
    xInv<- NULL                   
    set<- function(y) {
      x<<-y
      mInv<<-NULL
    }
    get<-function() x
    setInv<- function(matrix) xInv<<-matrix
    getInv<- function() xInv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## this function will utilize the structure created.  it accepts two parameters:
## the structure created, and a matrix you wish to find the inverse of.  if the inverse
## has not yet been found and stored, it will calculate and store the matrix and the inverse
## if the inverse has already been found and stored, it will quickly return the cached version
cacheSolve <- function(x, ...) {
  xInv<-x$getInv()                        ## create variable to hold cached matrix
  
  if(!is.null(xInv) && identical(x$get(), ...)){  ## if the cached matrix is not null,
    print("used cache")                           ## and the matrix remains unchanged,    
    xInv                                          ## then return the cached version
  }
  else {                                  ## else the cached matrix is null, or has been changed:
    xInv<-solve(...)                      ## then we calculate the inverse...
    x$set(...)                            ## store the original matrix in object
    x$setInv(xInv)                        ## store the inverse matrix in the object
    print("calculated straight up")
    xInv                                  ## return the inverse: next time it will be cached!
    
  }
  
}

