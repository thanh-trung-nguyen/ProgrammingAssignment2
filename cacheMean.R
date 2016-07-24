makeVector <- function(x = numeric()) 
{
  m <- NULL
  
  set <- function(y) ## function set a vector to be equatl to another vector. 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() ## function get a vector
  {
    x
  }
  
  setmean <- function(mean) ## function set mean value
  {
    m <<- mean
  }
  
  getmean <- function() ## function get mean value
  {
    m
  }
  
  ## return a list of 4 elements: set, get, setmean, getmean
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


cachemean <- function(x, ...) 
{
  m <- x$getmean()
  if(is.null(m)) 
  {
    message("calculating mean value")
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
  } 
  else 
  {
    message("getting cached data")
  }
  return(m)
}