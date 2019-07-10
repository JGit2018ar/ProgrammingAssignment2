## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##function to create a matrix that cache its invers
makeCacheMatrix <- function(x = matrix()) {
    
    
    inver <- NULL
    
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    
    get <- function() x
    
    setinver <- function(inverse) inver <<- inverse
    
    getinver <- function() inver
    
    list(set = set, get = get, setinver = setinver, getinver = getinver)
    
}

## Write a short comment describing this function
## function to calculate the inverse of the matric created above

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inver <- x$getinver()
    
    if(!is.null(inver)) {
        
        message("getting cached data")
        
        return(inver)
        
    }
    
    data <- x$get()
    
    inver <- solve(data, ...)
    
    x$setinver(inver)
    
    inver
}

## Functions check
mtx <- matrix(rnorm(16),4,4)
mtxa <- makeCacheMatrix(mtx)
cacheSolve(mtxa)
