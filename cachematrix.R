## Put comments here that give an overall description of what your
## functions do

#These pair of functions cache the inverse of a matrix 
#rather than compute it repeatedly every time you need it

## Write a short comment describing this function

#The first one has four functions that assign and get the values to cache
#set() set matrix into cache
#get() get matrix from cache
#set_inverse() set inverse matrix into cache
#get_inverse() get inverse matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        inv.x <- NULL
        set <- function(y) {
                x <<- y
                inv.x <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv.x <<- inverse
        get_inverse <- function() inv.x
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

#This function computes the inverse of the object matrix returned by  makeCacheMatrix().
#First it validates if the inverse already exist  and returns its value from cache
#or else, it computes the inverse and uses function set_inverse() to cache inverse and
#returns its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.x <- x$get_inverse()
        if(!is.null(inv.x)) {
                message("getting cached data")
                return(inv.x)
        } else{
        data <- x$get()
        inv.x <- solve(data, ...)
        x$set_inverse(inv.x)
        return(inv.x)  }
}


#Example:
j<-makeCacheMatrix()
j$set(matrix(rnorm(9),3))
j$get()

cacheSolve(j)
cacheSolve(j)