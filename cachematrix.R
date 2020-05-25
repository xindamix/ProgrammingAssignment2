## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The function, makeCacheMatrix creates a special "vector", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = numeric()) {
        # set the inverse to null
        i <- NULL
        # set the matrix to what is in memory using the scoping function
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # get the matrix
        get <- function() x
        # set the inverse 
        setinverse <- function(inverse) i <<- inverse
        # get the inverse
        getinverse <- function() i
        # list 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Check the cache for the inverse of the matrix
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
         # Return the value of the inverse
                return(i)
        }
        # Calculate the value of the inverse by getting the value of the matrix
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        # List the value
        i
}
