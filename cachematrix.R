## makeCacheMatrix uses scoping rules and stores matrices in memory

makeCacheMatrix <- function(X = matrix()) {
inverse <- NULL
set <- function(Y){
	X <<- Y
	inverse <<- NULL
	}
get <- function() X
setinverse <- function(Inverse) inverse <<- Inverse
getinverse <- function() inverse
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal decomposition
## this function will corpcor library and if it's not installed will install the library

cacheSolve <- function(X, ...) 
{
if(require("corpcor")){
	print("corpcor is loaded")
	} else {
		print("installing corpcor")
		install.packages("corpcor")
		if(require(corpcor)){
			print("corpcor installed")
			} else {
			stop("corpcor installation failed")
			}
		}
inverse <- X$getinverse()
if(!is.null(inverse)){
	message("matrix is in memory")
	return(inverse)
	}
message("inverse is being computed")
data <- X$get()
inverse <- pseudoinverse(data, ...)
X$setinverse(inverse)
inverse
}

