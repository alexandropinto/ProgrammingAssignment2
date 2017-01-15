## this function retrive the inverse of a matrix if it has been calculated otherwise calculate it.
### Use two function "makecachematrix"and CacheSolve


## The first function will  SET (1) GET (2) the matrix and SET (3) and GET it inverse

makeCacheMatrix <- function(x = matrix()) {
# the first value for m is NULL
  m<-NULL
  set<- function (y){
    x<<-y
    m<-NULL # m will be used as maker to know if the matrix has changed, so its first value should be NULL
  }
  get<- function()x
  setsolve<-function(solve) m<<-solve
  getsolve<-function ()m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This second function will compute the inverse of "matrix".  If the result has already been in cache so the function will retrive its value.
### Otherwise, it will calcule it.


cacheSolve <- function(x, ...) {
  ### To get the inverse of the matrix
  m<-x$getsolve()
  ### the function evaluate if the matrix alread has a inverse calculate. It so, it will show the cache value. Otherwise it will be calculate.
  if (!is.null(m)){
    message ("getting cache data")
    return(m)
  }
  ## getting the data
  data<-x$get()  
  ## calculating the inverse (using the solve function)
  m<-solve(data,...)
  ## setting the result
  x$setsolve(m)
  ## showing the result
  m
  }

