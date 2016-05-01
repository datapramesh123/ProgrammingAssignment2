## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A pair of functions that cache the inverse of a matrix
## makeCacheMatrix:This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {	## define the argument with default mode of "matrix"
		invmatrix<-NULL						## initialize inv as NULL; will hold value of matrix inverse
		set<-function(m1){					## define the set function to assign new 
			x<<-m1							## value of matrix in parent environment
			invmatrix<<-NULL				## if there is a new matrix, reset inv to NULL
		}
		get <-function() x					## define the get fucntion - returns value of the matrix argument
		setinverse<- function(solve) invmatrix <<- solve	## assigns value of inv in parent environment
		getinverse<- function() invmatrix					## gets the value of inv where called
		list(set=set,get=get,								## you need this in order to refer to the functions with the $ operator
			setinverse=setinverse,			
			getinverse=getinverse)
}



## Write a short comment describing this function
##This function will check if inverse is already calcualted for this matrix
##If inverse exists, environment values is displayed else its calculated on the fly
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invm<-x$getinverse()
	if(!is.null(invm)){
		message("getting cached data")
		return(invm)
	}
	data<-x$get()
	invm<-solve(data,...)
	x$setinvm(invm)
	invm
}