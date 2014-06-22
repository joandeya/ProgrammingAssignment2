## Put comments here that give an overall description of what your
## functions do

## It creates a special matrix.

makeCacheMatrix <- function(x = matrix()) {
	i<- NULL
	set <- function (y) {
	x<<-y
	i<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) i<<- solve
	getinverse<-function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
	
}


## The function cacheSolve check if the inverse of the given matrix has already been calculated, 
## if it has not, then it calculates the inverse and return it.
## It show also two messages just in case the given matrix is not an square matrix or it has not an inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
        	message("getting cached data")
        	return(i)
        }
        if(nrow(x)!=ncol(x)){
		message("x is not an square matrix")
        }
        if(det(x)=0){
        	message("x do not have an inverse matrix")
        }
        ## These two messages would not be necessary because in this exercise we supose that
        ## the given matrix will be invertible.
        data<-x$get()
        i<-solve(x,...)
        x$setinverse(i)
        i

}
