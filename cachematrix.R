## The following functions are used to obtain the inverse of a matrix
## therafter followed by saving it in cache so that next time when the
## user desires to perform inverse matrix calculation, the previously 
## saved value is retrieved thus avoiding recalculation


## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
	## create a matrix object x and associated sub-fuctions/methods
	
	## define the cache m
	m <- NULL
	set <- function(y){
		x <<- y ## assign the input matrix y to the variable x in the
				## parent environment
		m <<- NULL ## re-intialize m in the parent environment to null
	}
	get <- function() x ## return the matrix x
	setinverse <- function(inverse) m <<- inverse ## set the cache m
	## to the inverse of the matrix x
	getinverse <- function() m ## return the cached inverse of x

        list(set = set, get = get,

             setinverse = setinverse,

             getinverse = getinverse)

}

## The following function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. At first it checks whether the
## inverse has been calculated. In this case it retrieves the value
## from cache thereby avoiding the calculation. If this is not
## the case then it computes the inverse of the matrix and then sets
## the value of the inverse in cache via the "setinverse" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()

        if(!is.null(m)) {

                message("getting cached data")

                return(m)

        }

        data <- x$get()

        m <- solve(data, ...)

        x$setinverse(m)

        m

}
