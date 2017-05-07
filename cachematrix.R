## An overall description of our functions:

## Essential for understanding this programming assignement was 
## scoping rules in R for the <<- assignment operator.  
## The operators  <<- and ->> are normally only used in functions, and cause
## a search through parent environments for an existing definition
## of the variables being assigned.  If such a variable is found (and its
## binding is not locked), then its value is redefined.  Otherwise, the 
## assignment takes place in the global environment.

## The function below is similar to the example provided in the assignment 
## with the difference that we are calculating the inverse of a matrix
## instead of the mean of a vector.

## The function makeCacheMatrix returns an object consisting of four functions:
## set, get, setinverse, getinverse.
## The function cacheSolve calculates the inverse of the matrix using 
## the makeCacheMatrix function.  If the inverse has been previously calculated 
## and the original matrix has not changed, then cacheSolve returns the 
## inverse that was previously stored in cache (instead of recalculating it).

## So, the result of makeCacheMatrix is used as the argument of the function 
## cacheSolve.

## Here is how this function works:

## Suppose I use makeCacheMatrix to create an object called mat, then 
## mat$get() would return the original matrix
## 
## If this is the first time mat is encountered, 
## mat$getinverse() is set to NULL
##
## If we run cacheSolve(mat), the inverse of the matrix is returned.
## 
## Now, the inverse has been cached, so if we run mat$getinverse(), the
## inverse of our matrix is returned.
## 
## Now, if we use mat$set() and use the same matrix as the argument
## then running cacheSolve(mat) will retreive the inverse from cache
## potentially saving a costly recalculation.

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get= get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve's argument is the object list(set, get, setinverse, getinverse)
## that makeCacheMatrix returns.  If the inverse has already been calculated,
## that is, x$getinverse object is not null, then it displays the existing 
## value.  Otherwise, it uses the x$get object to calculate the inverse using
## solve function.

## This program works only if our matrix is invertible, with a few more lines
## of code, we could have added a condition to cacheSolve that 
## uses the det() function to calculate the determinant of our [square] 
## matrix to decide whether or not it is invertible.  

## To demonstrate the output, at the end of this file, I have included a nicely 
## engineered invertible 3x3 matrix that our functions use to invert.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}


## > myMatrix <- matrix(c(1,2,1,2,1,0,1,0,0), nrow = 3, ncol = 3, byrow = TRUE)
## > mat <- makeCacheMatrix(myMatrix)
## > mat$get()
## [,1] [,2] [,3]
## [1,]    1    2    1
## [2,]    2    1    0
## [3,]    1    0    0
## > mat$getinverse()
## NULL
## > cacheSolve(mat)
## [,1] [,2] [,3]
## [1,]    0    0    1
## [2,]    0    1   -2
## [3,]    1   -2    3
## > mat$getinverse()
## [,1] [,2] [,3]
## [1,]    0    0    1
## [2,]    0    1   -2
## [3,]    1   -2    3
## > mat$set(myMatrix)
## > mat$get()
## [,1] [,2] [,3]
## [1,]    1    2    1
## [2,]    2    1    0
## [3,]    1    0    0
## > mat$getinverse()
## NULL
## > cacheSolve(mat)
## [,1] [,2] [,3]
## [1,]    0    0    1
## [2,]    0    1   -2
## [3,]    1   -2    3
## > cacheSolve(mat)
## getting cached data
## [,1] [,2] [,3]
## [1,]    0    0    1
## [2,]    0    1   -2
## [3,]    1   -2    3









