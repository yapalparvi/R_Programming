##cretes a matrix object that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
 m<-NULL
##method to set the matrix
	set<-function(matrix){
x<<-matrix
m<<- NULL

	}
#Method to get the matrix
get<-function()
	{
#Return the matrix
		x
	}
#Method to set the inverse of the matrix
	setinverse<-function(inverse){
   m<<-inverse
	}
#Method to get the inverse of the matrix
getinverse<-function(){
     m
	}
##Returns a list of methods
list(set=set,get=get,
      setinverse=setinverse,
	  getinverse=getinverse)
}


##compute the inverse of the matrix returned by "makeCacheMatrix"
##If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve<-function(x, ...){
	## Return a matrix that is the inverse of 'x'
m<-x$getinverse()
##Retrun the inverse if already set
	
	if(!is.null(m)){
        message("getting cached data")
        return(m)
	}
#Get the matrix from the object
data<-x$get()
#calculate the invese using solve
m<-solve(data) %*% data
##set the inverse to an object
 x$setinverse(m)
##Return the matrix
 m
}
