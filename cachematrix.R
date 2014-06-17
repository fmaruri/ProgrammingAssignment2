## Create a special Matrix

## This functions makes a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	f<-NULL
	set<-function(y){
	x<<-y
	f<<-NULL
}
get<-function() x
setmatrix<-function(solve) f<<-solve
getmatrix<-function() f
list(set=set, get=get,
	setmatrix=setmatrix,
	getmatrix=getmatrix)

}

## This function computes the inverse of the special Matrix object created above
## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
f<-x$getmatrix()
if(!is.null(f)) {
	message ("getting cached data")
	return(f)      
}
	matrix<-x$get
	f<-solve(matrix, ...)
	x$setmatrix(f)
	f
}