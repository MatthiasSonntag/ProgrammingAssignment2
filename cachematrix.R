## Descriptions and functions in this sprict are heavily based on the course 
## materials (Roger D. Peng, Jeff Leek, Brian Caffo, https://class.coursera.org/
## rprog-014/human_grading/view/courses/973495/assessments/3/submissions, 23.05.2105).


## The first Function makeCacheMatrix creates a list. The list
## contains the functions to
## 1. set the value of the matrix
## 2. get the above matrix value
## 3. set the value of the inverted matrix
## 4. get the inverted matrix value

## makeCacheMatrix takes a matrix as input. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = solve(matrix(c(2,1,5,3),2,2)),
         setinv = setinv,
         getinv = getinv)
}
## The second function, cacheSolve, checks wether the inverted matrix 
## has been calculated or not. If so, it gets the inverted matrix from the 
## cache. If the inverted matrix has not been calculated it gets the matrix,
## works the inverse out and returns the result.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
