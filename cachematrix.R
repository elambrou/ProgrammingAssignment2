# These functions cach the inverse of a matrix to avoid
# costly computations.

# makeCachMatrix creates a list that contains a function to 
# set the value of the matrix, get the value of the matrix, 
# set the value of the inverse of the matrix and 
# get the value of the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv <<- inverse
    getinv<-function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# cacheSolve calculates the inverse of the Matrix created using 
# makeCacheMatrix. First it checks if the inverse is cached to 
# skip the calculation. If not, it calculates the inverse   
# and sets the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <-x$get()
    inv<-solve(data)
    x$setinv(inv)
    inv
}
