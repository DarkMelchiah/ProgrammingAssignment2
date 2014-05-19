###############################################################################################
# The following functions generates a Matrix x and then generates the inverse of this matrix. #
# To make the process as efficient as possible, the inverse matrix is generated only once,    #
# in the makeCacheMatrix function. The cacheSolve function verifies if the inverse matrix     #
# was generated before and, iff it wasn't generated, get the matrix x and generates the       #
# inverse of it.                                                                              #
###############################################################################################


###############################################################################################
# the following function generates a "special" matrix that holds setters and getters to it    #
# and generate the inverse matrix of x, storing it in the Inv variable                        #
###############################################################################################
makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y){
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) Inv <<- solve
  getInvMatrix <- function() Inv
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}
###############################################################################################

###############################################################################################
# the following function verifies if the Inv variable have the inverse matrix (!in.null(Inv)).#
# If the inverse matrix is in the Inv variable, it returns the matrix and, if it's not there, #
# the function generates the inverse matrix and stores it in Inv                              #
###############################################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInvMatrix()
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInvMatrix()
  Inv
}
###############################################################################################