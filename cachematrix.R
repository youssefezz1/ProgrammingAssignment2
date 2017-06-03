rm(list=ls())
setwd("/Users/yo39958/Desktop/Rcourse/RonlineCourse")


## The function caches the value of the inverse matrix and uses it
# instead of recalculating it every time the function is executed


## This function resets the value of n (clear the cache) and creates a list
## that set and gets a value for the matrix
## and sets and gets a value for the inverse

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        setmatrix <- function(y){
                x<<-y
                n <<-  NULL 
        }
        getmatrix <- function() x
        setinversemtrx <- function(inverse) n <<- inverse 
        getinversemtrx<- function() n 
        list (set =setmatrix, get = getmatrix, setin = setinversemtrx, getin = getinversemtrx)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        n <- x$getin()
        if(!is.null(n)){
                message("value in chache memory")
                return(n)
        }
        data <- x$get()
        n <- solve(data)
        x$setin(n)
        n
}

m1 = matrix(c(1/2,-1/4,-1,3/4), nrow = 2, ncol = 2)
mymatrixobj <- makeCacheMatrix(m1)
cacheSolve(mymatrixobj)        
