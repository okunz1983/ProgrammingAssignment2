
## The following function creates a special "matrix" object that can cache its inverse.




makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setmean <- function(mean) inv <<- mean
        getmean <- function() inv
        list(set = set,get = get,setmean = setmean,getmean = getmean)

}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        z <- x$getmean()
        if (!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        mat <- x$get()
        z <- solve(mat, ...)
        x$setmean(z)
        z

}


