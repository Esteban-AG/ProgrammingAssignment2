## So, we want to cache the inverse of a Matrix once that is calculated 
## in order to save he calcuation effort later

## Example: 
## > pyer<-matrix(c(2,6,3,9,6,5,3,1,8),3,3)
## > sombrero<-makeCacheMatrix(pyer)
## > cacheSolve(sombrero)
## [,1]        [,2]        [,3]
## [1,] -0.15194346  0.20141343  0.03180212
## [2,]  0.15901060 -0.02473498 -0.05653710
## [3,] -0.04240283 -0.06007067  0.14840989
##> cacheSolve(sombrero)
##getting cached data
##[,1]        [,2]        [,3]
##[1,] -0.15194346  0.20141343  0.03180212
##[2,]  0.15901060 -0.02473498 -0.05653710
##[3,] -0.04240283 -0.06007067  0.14840989

## Creates a list of functions to get and set the matrix and its cached
## inverse

makeCacheMatrix <- function(camp = matrix(c(1,0,0,1), nrow=2, ncol=2)) {
        
        inv<-NULL
        
        set <- function(y) {
          camp <<- y
          inv <<- NULL}
        
        get <- function() camp
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}  


## This will recover the inverse if already calculated, or will calculate it
## and cache it otherwise. Make sure to use a variable using the 
## previous function as an argument

cacheSolve <- function(matriz) {
        inv <- matriz$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- matriz$get()
        inv <- solve(data)
        matriz$setinv(inv)
        inv
}
