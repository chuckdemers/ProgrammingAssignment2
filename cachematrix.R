##
##  Chuck Demers
##  R Programming
##  Coursera - John Hopkins University
##  May 22, 2015
##
##  Programming Assigment 2: Lexical Scoping
##  
##  cachematrix.R

##  This program creates an object that contains a matrix, multiple functions
##  for the object, and once computed the inverse of the matrix.  The inverse
##  is held in memory so that it does not have to be re-computed

##  makeCacheMatrix defines the functions allowing access to the cached matrix
##  - function set    - assigns a new matrix to the cached matrix
##  - function get    - retrieves the cached matrix
##  - function setinv - assigns a new matrix to the cached inverse matrix
##  - function getinv - retrieves the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function( y ){
		x <<- y
		minv <<- NULL
	}
	get <- function() x
	setinv <- function( inv ) minv <<- inv
	getinv <- function() minv
	list( set=set, get=get, setinv=setinv, getinv=getinv)
}


##  cacheSolve returns the inverse of a makeCacheMatrix object
##  it checks first if the object already has a cached inverse.
##  if it does, 
##    it simply indicates it is use cache and returns the object
##  if it does not, 
##    it takes the matrix from the object
##    computes the inverse
##    assigns the inverse back to the cache object
##    returns the inverse

cacheSolve <- function(x, ...) {

	minv <- x$getinv()
	if( !is.null(minv) ){
		message("getting cached data")
		return( minv )
	}
	data <- x$get()
	minv <- solve(data)
	x$setinv( minv )
	minv
}
