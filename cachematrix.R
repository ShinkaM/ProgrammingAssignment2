## Put comments here that give an overall description of what your
## functions do

## makecachematrix function caches the inverse of a matrix using solve().
##cacheSolve uses makecachematrix to calculate the inverse of a matrix. 
##If the inverse was calculated alreadt, it will use the value from the cache;otherwise
##it calculates the inverse with solve().
makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set<-function(y){
		x<<-y
		m<<- NULL
	}
##set the value of matrix
	get<- function()x
##get the value of matrix
	inverse<- function()m<- solve(x)
	getinverse<- function()m
##get the inverse of the matrix using solve()
	list(set=set, get=get,
	inverse= inverse,
	getinverse=getinverse)
##list the values of the variables when called
}


## makeCacheMatrix sets the value of a matrix, gets the value of the matris, and uses solve()
##to calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        m<- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
##if the inverse has been calculated, get it from cache
	data<- x$get
	m<- solve(data, ...)
	x$inverse(m)
	x
##if the inverse has not been calculated, calculate it using the inverse function

}
##makeCacheMatrix uses makeCacheMatrix to calculate inverses for a matrix, but if it has been
##calculated already, it will return the value from makeCacheMatrix.  If not, it wil use 
##solve() to calculate the inverse.