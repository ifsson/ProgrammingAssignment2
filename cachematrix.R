
## With this function we create a list of functions performing following operations
##  set -> stores the matrix
##  get -> retrieves the matrix
##  setsolve -> stores the inverted matrix
##  getsolve -> retrieves the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL    ## clearing inverted matrix
        set <- function(y) {
                x <<- y  ## assigning in global environment
                inverted <<- NULL ## assigning in global environment
        }
        get <- function() x
        setsolve <- function(solve) inverted <<- solve
        getsolve <- function() inverted
        list    (
                set = set, 
                get = get,
                setsolve = setsolve,
                getsolve = getsolve
                )
}


## This function creates inverted matrix in case the cache is empty 
## if cache is not empty we simply return cached matrix
## 
## If cache is empty then 'getsolve' function will return NULL then 'data' gets the matrix values by calling get function 
## then calculates inverted matrix with solve('data') 
## 'inverted' gets assigned inverted variable value
## using 'setsolve' we cachce the inverted matrix
##
## If cache already contains inverted matrix i.e. 'inverted' value is not null
## function simply returs inverted matrix



cacheSolve <- function(x, ...) {
inverted <- x$getsolve() 
    if(!is.null(inverted)) {
        message("getting cached data.")
        return(inverted)
    }
    data <- x$get()
    inverted <- solve(data)
    x$setsolve(inverted)
    inverted   
   
}

## sample output
##
## creating matrix
##> b <- c(1, 2, 3, 1, 5, 7, 4, 11, 8)
##> x <-matrix(b,3,3)
## caching matrix
##> testmatrix = makeCacheMatrix(x)

##> testmatrix$get()
##     [,1] [,2] [,3]
##[1,]    1    1    4
##[2,]    2    5   11
##[3,]    3    7    8
## chcecking that 'getsolve' function is working
##> testmatrix$getsolve()
##NULL
## since it is the first run it correctly returns NULL
## inverting matrix for the first time
##> cacheSolve(testmatrix)
##            [,1]       [,2]   [,3]
##[1,]  1.54166667 -0.8333333  0.375
##[2,] -0.70833333  0.1666667  0.125
##[3,]  0.04166667  0.1666667 -0.125
##inverting matrix for the first time
##> cacheSolve(testmatrix)
##getting cached data.
## since inverted matrix was already cached function returns cache
##            [,1]       [,2]   [,3]
##[1,]  1.54166667 -0.8333333  0.375
##[2,] -0.70833333  0.1666667  0.125
##[3,]  0.04166667  0.1666667 -0.125
