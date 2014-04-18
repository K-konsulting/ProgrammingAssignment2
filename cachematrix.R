## makeCacheMatrix wrapps a matrix into a list containing
## methods that are used to store and retrieve not only
## the matrix itself but also any addtional data, that
## then become logically bounded to that matrix.
##
## cacheSolve utilizes this wrapper to avoid doing
## repeated matrix inverse computations upon repeated
## calls to this function with the same wrapped matrix.


## I decided to clean up the interface a little bit
## and only provide two functions; one for retrieval 
## and one for storing. If no 'name' parameter is
## supplied, then the base matrix is automatically
## returned or stored.
makeCacheMatrix <- function(x = matrix()) {
  cache = list(matrix = x)
  get = function (name = "matrix") cache[[name]]
  set = function (value, name = "matrix") {
    if (name == "matrix")
      cache <<- list(matrix = value)
    else
      cache[[name]] <<- value
  }
  list(get = get, set = set)
}


## Only performs the matrix inverse calculations if
## no cached result is available, and stores the
## computed result before it's being returned.
cacheSolve <- function(x, ...) {
  inverse <- x$get("inverse")
  if (is.null(inverse)) {
    m <- x$get()
    inverse <- solve(m, ...)
    x$set(inverse, "inverse")
  }
  inverse
}
