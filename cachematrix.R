## The makeCacheMatrix function takes a matrix (square matrix only) as an input and returns a special list vector
## with 3 functions as its elements.
## The cacheSolve function takes as its argument the special vector returned by makeCacheMatrix and returns the inverse
## of the matrix we provided makeCacheMatrix. If we call cacheSolve for the 2nd time, it returns the cached value
## instead of 'calculating' the inverse again.

## makeCacheMatrix functions takes a square matrix (only) as its input and returns a special list vector with 3 functions
## as its element in return. 

makeCacheMatrix <- function(x = matrix()) {
  ## initially set m to null. Everytime we call this function, the mean value is reset to null.
	m <- NULL
	get <- function() {
		x
  	}
  ## this function sets the value of inverse to m. this would be the cached value. t
  	setinverse <- function(inverse) {
		m <<- inverse
  	}
  ## returns the cached value of m --> which is the inverse of our original matrix.
  	getinverse <- function() {
		m
  	}
  	list(get = get,setinverse = setinverse,getinverse = getinverse)
}


## this function takes the special list vector (output from the makeCacheMatrix function) as its input and
## produces the inverse of the original matrix. if this function is called more than once without change the 
## original vector, it returns the cached inverse instead of calculating it again.

cacheSolve <- function(x, ...) {
  ## call the getinverse function using $name format to see what the value of m already is
	m <- x$getinverse()
  ## if m is not null - which can only mean that m is already cached with the inverse value. return m.
	if(!is.null(m)) {
		message("getting cached data")
	    	return(m)
  	}
  ## if next line is getting executed, it can only mean that m is not already cached.
  ## therefore, get the original matrix into data variable
	data <- x$get()
  ## calculate the inverse of the matrix using solve function
	m <- solve(data)
  ## set the inverse value
	x$setinverse(m)
	m
}
