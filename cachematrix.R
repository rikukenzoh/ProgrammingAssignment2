## The first funcion will output a list which has four things(I don't know how to say about [[1]],[[2]]... ?).
## And when we input x(which is matrix) ,the first function will finally store the x's value into x$getinverse for cache.
## The second function will check the data whether have been stored into the makeCacheMatrix function(the data is cached
## inverse matrix),if the data has already been stored in the cache, and just return the data which is in the cache.
## If not,the function will compute the inverse of the data(second function's argument) and store the result into the x.
## So, we can use these two function to make a cache and check whether the data has been computed.
## They can help us to improve the computer's running speed.(Can I say it like this?)

## makeCacheMatrix function convert the input x(which is data of matrix) into a cache as stored data.

makeCacheMatrix <- function(x = matrix()) {
        m = NULL
        set = function(y){
                x <<- y
                m <<- NULL
        }
        get = function() x
        setinverse = function(inverse) m <<- inverse
        getinverse = function() m
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## cacheSolve function will check the input whether has already in the cache, if not, the function will compute the input
## and store the output into the makeCacheMatrix function['setinverse'] to be the new stored data and update the data of cache.


cacheSolve <- function(x, ...) {
        m = x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data = x$get()
        m = solve(data,...)
        x$setinverse(m)
        m
}
