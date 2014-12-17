## Put comments here that give an overall description of what your
## functions do

#  function makeCacheMatrix 
#       creates a matrix object with storage for the matrix and its inverse
#       defines internal functions applicable to the matrix

#  cacheSolve
#       Operates on the matrix object created by makeCacheMatrix to calculate the 
#       matrix inverse if it has not already been calculated, or the stored value if it has.
   

# FUNCTION makeCacheMatrix
#   stores matrix 
#   stores inverse after it is computed
#   CONTAINS 3 internal functions or methods applicable to a matrix
#       getMTRX: return matrix elements
#       setIVRS: solves for the inverse and stores using superassignment
#       getIVRS: returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {

     #  ivrs is the inverse and its elements are initially set to NA
     ivrs <- x    
     ivrs[,]<-NA
     
     # FUNCTION getMTRX returns the original matrix
     getMTRX <- function() { x }   
     
     # FUNCTION setIVRS solves for the inverse and stores using superassignment
     setIVRS <- function(solve)  { ivrs <<- solve }
     
     # FUNCTION returns the cached inverse 
     getIVRS <- function() { ivrs } # this will return the cached value to cachemean() on

     #  List of Internal Functions
     list(getMTRX = getMTRX, setIVRS = setIVRS, getIVRS = getIVRS)        
}


# FUNCTION cacheSolve
#     Returns the inverse of the object x matrix
#     If it has previously been calculated, the cached value is returned
#     Otherwisethe inverse is calculatede cahed in object x and return to calling program

cacheSolve <- function(x, ...) {
     
     # get inverse from object x
     ivrs <- x$getIVRS()               
     
     if(!anyNA(ivrs)) {             
     # object x inverse has previously been defined; return cached value     
          message("getting cached data")  # Issue message that cached inverse is avialable
          return(ivrs)                    # returned with cached inverse
     }
     
     # Inverse has not been previously defined; compute
     data <- x$getMTRX()        # get matrix elements of object x
     ivrs <- solve(data, ...)   # calculate inverse
     x$setIVRS(ivrs)            # cache inverse in object x
     ivrs                       # return the inverse to the code that called this function
}
