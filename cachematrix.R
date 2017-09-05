##Functions
##makeCacheMatrix - creates a list of pointers to functions that store and retrieve the inverse of a matrix
##cacheSolve  - uses the list of vectors created by makeCacheMatrix to store the inverse


##makes a "special vector" which is really just a list of functions
##there are four functions, get/set the input vector, get/set the mean of the vector
##This is similar to the vtable in COM if being created in C++
##essentially, this is a list of pointers where the pointers are to a function
##although, the actual matrix is persisted inside this function which differs from vtable
makeCacheMatrix <- function(x = matrix()) 
{
        inverse <- NULL  ##init
        set <- function(y)  ##set function
        {
                x <<- y           ##stores y at parent level
                inverse <<- NULL  ##stores inverse at parent level
        }
        get <- function() x   ##get function
        setinverse <- function(newinverse) inverse <<- newinverse ##setinverse function, note the value of inverse is stored at parent level
        getinverse <- function() inverse  ##getinverse function
        
        ##create and return the list of pointers (functions)
        list(set = set, get = get,      
             setinverse = setinverse,
             getinverse = getinverse)
}


##The following function calculates the inverse of the matrix held by the special "vector" 
##created with the function makeCacheMatrix. However, it first checks to see if 
##the inverse has already been calculated. If so, it gets the inverse from the 
##cache and skips the computation. Otherwise, it calculates the inverse of 
##the data and sets the value of the inverse in the cache via the setmean function.

##Note: the input to this function is a vector created with makeCacheMatrix()
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()   ##calls the getinverse() from the list in x
        if (!is.null(inverse))    ## if NOT null, then just retrieve the previously stored inverse
        {
                message("getting cashed inverse")
                return (inverse)    ##return breaks out of function in R
        }
        ##if get here, then it means inverse WAS null
        
        data <- x$get()  ##get the data from the function 
        inverse <- solve(data, ...)  ##solve for the inverse
        x$setinverse(inverse)        ##set the inverse so we don't have to calculate it again
        inverse                      ##return the inverse 
}
