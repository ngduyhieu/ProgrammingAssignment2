## These two functions compute and cache the inverse of a matrix

#------------------------------------------------------#
# This function is to cache a matrix and return a special matrix 
# with all defined functions: get, set, getinverse, and setinverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #initialize a null value for the mean
    set <- function(y) {
        x <<- y  #set a varialbe x = y in the parent enviroment, i.e., the enviroment of makeCacheMatrix
        m <<- NULL #reset the mean to NULL
    }
    get <- function() x #alr cache x -> return x
    setinverse <- function(solve) m <<- solve #compute and assign the inverse
    getinverse <- function() m #alr cache the inverse -> return the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) #return the special matrix with all defined functions
}


#------------------------------------------------------#
#This function is to compute and cache the inverse of a matrix, or return the inverse
# if it was alr cached

cacheSolve <- function(x, ...) {
    m <- x$getinverse() #check to see whether the inverse was cached
    if(!is.null(m)) { #if m is different from NULL -> alr cache the inverse
        message("getting cached data") #inform that the ivnerse was alr cached
        return(m)
    }
    data <- x$get() #if the inverse was not cached, then get the data from x
    m <- solve(data, ...) #compute the inverse and assign it to m
    x$setinverse(m) #set the cached inverse of x to m
    m #return m as the cached data
}
