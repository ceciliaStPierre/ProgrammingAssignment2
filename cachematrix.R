## Here are 2 function, the first one create an "special" matrix which save in data caché
## in order to be used for another function. The second one, compute the inverse of
## a matrix, but using the first funcion, so, the first one save information in caché and the 
## second one using it geting it from caché


## First function, take a matrix and save in cache some results that will be calculated and 
## used for the other function

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <-function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list (set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function solve a inverse of a matrix, but is implemented using the cache memory

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}

## the use of those function is:
## > cacheSolve (makeCacheMatrix(x))
## so, the first execution is the nested function that define the functions set, get, setinv 
## and getinv saving those results in the cache in order to be used later.
## When the first funcion is used, the reference to the first funcion, like x$getinv, bring 
## variables saved in cache memory, and over it, perform the problem excecution.