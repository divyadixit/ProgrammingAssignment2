## These set of functions take a resource intensive operation like calculating
## the "Inverse of a matrix" in this case and make it more efficient.
## It does so by checking if the inverse has been calculated previosly and if so,
## the function cacheSolve would return the inverted matrix from the cache instead
## of re-doing the whole calculation. 

## makeCacheMatrix takes a matrix as input and returns a matrix like special object.
## It contains four functions : get, set, getinv, setinv

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y){
				x <<- y
				m <<- NULL
		}
		get <- function() x
		setinv <- function(inv) m <<- inv
		getinv <- function() m
		list( set = set, get = get,
				setinv = setinv,
				getinv = getinv) 

}


## cacheSolve takes a matrix as an input, checks if it has the inverted matrix 
## in the cache memory. If it does, it returns the inverted matrix from the cache
## or else, it goes on to calculate the inverse of the new matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
		if(!is.null(m)){
				message("getting cached data")
				return(m)
		}
		data <- x$get()
		m <- solve(data)
		x$setinv(m)
		m
}

## makeCacheMatrix(A)
## [,1] [,2]				<<----- Matrix A
## [1,]    1    3
## [2,]    2    4

###cacheSolve(am)     
##  getting cached data
##     [,1] [,2]			<<-----Inverted matrix being retreived from cache
##	[1,]   -2  1.5
##	[2,]    1 -0.5
