## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

## Store x into cache and make get funcs for both x and x_inverse (InvM)
## Make Set funcs to set x and x_inverse

makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL
	Set <- function(y) {
   		x <<- y
    		x_inv <<- NULL
  	}
	Get <- function() x
	SetInv <- function(Inv) InvM <<- solve(x)
	GetInv <- function() InvM
  	list (Set = Set,
      	Get = Get,
      	SetInv = SetInv,
      	GetInv = GetInv)
}


## Get x_inverse, check it it's NULL, compare x w/ stored x, 
## - if Inv=NULL, fill it w/ x_inverse
## - if x is the same as before, exit the function w/o calculation
## - if x is updated, update x_inverse as well

cacheSolve <- function(x, ...) {
	Inv <- mf$GetInv()
	## print(mf$Get())
	if (!is.null(Inv)) {
		message("Getting cached data")
		if (identical(mf$Get(),x)) { 
			return("There'n no changes")
		}
      }
      m <- mf$Get()
      Inv <- solve(m)
	mf$SetInv(Inv)
	mf$Set(x)
	return("Inverse matrix has been updated")
}


## for check:
x<-matrix(rexp(100, rate=.1), ncol=10) ## generate random matrix
makeCacheMatrix() 
mf <- makeCacheMatrix() 
mf$Set(x) ## Store x
mf$Get()
x
mf$SetInv(x) ## Store x_inverse
mf$GetInv()
cacheSolve(x) # Run 2nd func to compare and fill
cacheSolve(x) # For second run it has to skip any calculation 
x<-matrix(rexp(100, rate=.1), ncol=10) ## change matrix
cacheSolve(x) # check whether it use the new matrix
Inv
