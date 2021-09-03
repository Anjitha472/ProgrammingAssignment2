
# makeCacheMatrix consist of set,get,setinv,getinv.
# library(MATRIX) calculate inverse of squared and non squared matrix.
library(MATRIX)
makeCacheMatrix <-function(x = matrix())
{
  inv<-NULL        # initializing inverse as Null.
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function()x      # function to get matrix x.
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x   #function obtain inverse of matrix.
  }
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}

#This is used to get the cache data

cachesolve <- function(x, ...) # gets cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){   # checking whether inverse is Null.
    message("getting cached data")
    return(inv) # return inverse value.
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv        ## Return a matrix that is the inverse of 'x'

}

