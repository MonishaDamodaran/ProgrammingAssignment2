

## A pair of functions are written to cache the inverse of a matrix

## makecacheMatrix creates a matrix to cache the inverse

makeCacheMatrix <- function(x = matrix()) {    # an argument with object of type matrix is defined
                solve_matrix <- NULL           # the object solve_matrix is initialized
                set <- function(y) {           # a new function called set is created with argument y
                        x <<- y                # the value in  variable y is set to the object x
                        solve_matrix <<- NULL
                }
                get <- function() x            # get function is defined and it returns the elements of the matrix
                setinverse <- function(inverse) solve_matrix <<- inverse # assigns the value of inverse to solve_matrix
                getinverse <- function() solve_matrix # gets the values of solve_matrix by calling it
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }


## cachesolve function calculates the inverse for the matrix returned by makecacheMatrix function

cacheSolve <- function(x, ...) {          # a function is defined with x and ellipsis as its arguments
        ## Return a matrix that is the inverse of 'x'
                solve_matrix <- x$getinverse() 
                if(!is.null(solve_matrix)) {       #it checks if the inverse has been already calculated
                        message("retriving cached data")
                        return(solve_matrix)       # if already calculated it returns the inverse from the cache
                }
                data <- x$get()
                solve_matrix <- solve(data,...)
                property <- solve(data) %*% data
                x$setinverse(solve_matrix)
                solve_matrix
                property
        }

