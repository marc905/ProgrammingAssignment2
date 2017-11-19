## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will calcuate the inverse of a matrix that is passed via the function call.  It contains
## functions for getting/setting the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
       m <- NULL

    ## Set, Get methods for storing the matrix
       set <- function(y) {
           x <<- y
           m <<- NULL
        }

        get <- function() x

    ## Similar methods are used to define the inverse 
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
                
    ##Values above are returned as a list object.
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }
    


## cacheSolve is passed a matrix previously run through makeCacheMatrix.  The procedure will
## determine if the matrix is unchanged since the inverse was last computed.  If so, it will
## returned the cached inverse.  If not, it will calculate the inverse.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()  

    if(!is.null(m)){  
        message("getting cached data")
        return(m)    ##return the cached inverse value.
    }
        data <- x$get()   ##inverse is computed if it wasn't previously cached.
        m<- solve(data,...)
    x$setinverse(m)
        m
    
     
   
    }



##The first function, makeVector creates a special "vector", which is really a list containing a function to

##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean


makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL

    }

    get <- function() x

    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


##The following function calculates the mean of the special "vector" created with the above function. 
##However, it first checks to see if the mean has already been calculated. If so, it gets the mean from 
##the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value 
##of the mean in the cache via the setmean function.

cachemean <- function(x, ...){
    m<- x$getmean()

        if(!is.null(m)){
                message("getting cached data")
                return(m)
    }
    data <- x$get()
    m<- mean(data,...)
    x$setmean(m)
    m
}

