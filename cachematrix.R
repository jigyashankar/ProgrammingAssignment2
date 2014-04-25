##  makeCacheMatrix computes a special matrix that provides
## an interface to cache the inverse of a matrix passed to it
## cacheSolve takes the matrix object created using 
## makeCacheMatrix which wraps the matrix whose inverse is to 
## along with functions to cache the results, 
## computes the inverse and stores it for future calls to 
## inverse.

## Create a matrix object with functions to cache results of 
## inverse computation
makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y) {
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setinv <- function(i) inv<<-i
	getinv<-function() inv

	## Return the list of functions that can be invoked to 
	## Get and set state of matrix object
	list(set=set, get=get, 
		setinv=setinv, getinv=getinv)
}


## Take a special matrix object and use its interface to fetch 
## cached value of the inverse if already calculated. If not, ## calculate the inverse and store it in the matrix object
## Calling cacheSolve on the same matrix object to compute 
## inverse, should return the cached value and print a message 
## to indicate the same.

cacheSolve <- function(x, ...) {
        ## Get cached inverse if available
	inv<-x$getinv()
	if(!is.null(inv)){
		message("Returning cached inverse of matrix")
		return(inv)
	}
	## First time inverse is being computed, solve and cache 
	##results	
	mat<-x$get()
	i <-solve(mat)
	x$setinv(i)
	i
}
