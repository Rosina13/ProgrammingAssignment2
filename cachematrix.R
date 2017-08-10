makeCacheMatrix <- function(x = matrix()){    #formal argument x with default value empty matrix
      i <- NULL #creates empty object i to fill later
      set <- function(y){
            x <<- y # x is (re)set to the value of y
            i <<- NULL # i set back to NULL, clearing previously stored values
            }
      get <- function() x     #x is retrieved from parent environment
      setinverse <- function(inverse) i <<- inverse   #setter
      getinverse<- function() i     #getter
      list(set = set, get = get, #gives list to be used as input for cacheSolve
           setinverse = setinverse,
           getinverse = getinverse)
      }
      
cacheSolve <- function(x){
      i <- x$getinverse()
      if(!is.null(i)) {      #if i is not NULL, i can be retrieved from cache unchanged
            message("getting cached data")
            return(i)
            }
      data <- x$get()   #matrix retrieved    
      i <- solve(data)  #if i is NULL, inverse matrix is calculated with solve function
      x$setinverse(i)
      i      #prints inverse matrix
      }
