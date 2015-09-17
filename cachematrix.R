## Debooh Hughes
## 9/16/2015

# R Programming - Week3 Assignment



## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	    ## set local m to null
	  	m <- NULL

	  	## set function, gets data and stores in x, clears m
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ## get function gets the matrix
        get <- function() x

        ## setinverse function runs solve on data and sets global m
        setinverse <- function(solve) m <<- solve

        ## getinverse function returns the inverse stored in m
        getinverse <- function() m

        ## creates list of functions
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}

##cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

##Computing the inverse of a square matrix can be done with the solve function 
##Example, if X is a square invertible matrix, then solve(X) returns its inverse

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	  m <- x$getinverse()

	  ## if inverse is already created just go get it from m
      if(!is.null(m)) {
              message("getting cached data")
              return(m)
      }

	  ## inverse not saved yet so get the data 
      data <- x$get()

      ##then compute inverse
      m <- solve(data, ...)

      ## save the inverse using setinverse
      x$setinverse(m)
      m
}

##################SAMPLE RUN#####################################
##> amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##> amatrix
##$set
##function (y) 
##{
##    x <<- y
##    m <<- NULL
##}
##<environment: 0x0000000009e988c0>
##
##$get
##function () 
##x
##<environment: 0x0000000009e988c0>
##
##function (solve) 
##m <<- solve
##<environment: 0x0000000009e988c0>
##
##$getinverse
##function () 
##m
##<environment: 0x0000000009e988c0>
##
##> amatrix$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(amatrix)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> amatrix$getinverse()
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(amatrix)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##################SAMPLE RUN#####################################
