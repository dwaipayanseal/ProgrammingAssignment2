## The first function, makeCacheMatrix creates a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
##Second function, cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.



##The First function
makeCacheMatrix <- function(x = matrix()) {
    # "Inv" holds the cached value or NULL if nothing is cached
    # initially nothing is cached so set "Inv" to NULL
    Inv<-NULL
    
    # store a matrix
    set<-function(NewValue){
        x<<-NewValue
        # set the cached value of Inverse as NULL
        Inv<<-NULL
    }
    
    # return the stored matrix
    get<-function() {
        x
    }
    
    # cache the given argument
    setmatrix<-function(solve) {
        Inv<<- solve
    }
    
    # get the cached value 
    getmatrix<-function() {
        Inv
    }
    
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}



##Second function
cacheSolve <- function(x=matrix(), ...) {
    # get the cached value
    Inv<-x$getmatrix()
    
    # if a cached value exists return it
    #EXECUTED ONLY WHEN CACHE <> NULL
    if(!is.null(Inv)){
        message("getting cached data")
        return(Inv)
    }
    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    #EXECUTED ONLY WHEN CACHE =NULL
    else{
        matrix<-x$get()
        Inv<-solve(matrix, ...)
        x$setmatrix(Inv)
        Inv
    }
}
