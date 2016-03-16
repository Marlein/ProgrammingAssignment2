
## For this function I followed the example. 
## set the value of the matrix, get the value of the matrix, 
## set the value of the inverse and get the value of the inverse. 

## makeCacheMatrix will create a matrix-object where it can cach its inverse.

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y){
                x <<- Y
                m <<- NULL
        }
        
        get <- function() x
        setsolvemtrx <- function(solve) m <<- solve
        getsolvemtrx <- function() m
        
        list(set=set, get=get, setsolvemtrx=setsolvemtrx,
             getsolvemtrx=getsolvemtrx)
} 




## CacheSolve computes the inverse, but only if the value of the inverse did
## not already exist.


cacheSolve <- function(x, ...) { 
        m <- x$getsolvemtrx()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x' 
        mtrx <- x$get()
        m <- solve(mtrx, ...)
        x$setsolvemtrx(m)
        m
} 

## I have tested this function with:
## tstmtx <- (c(1,1,4,0,3,1,4,4,0), nrow=3)
## tst <- makeCacheMatrix(tstmtx)