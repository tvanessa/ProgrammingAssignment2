#The first function, makeVector creates a special "vector", which is really a list containing a function to
#   set the value of the matrix
#  get the value of the matrix
#  set the value of the inverse
#  get the value of the inverse

makeCacheMatrix <- function(x = matrix())
{
    matrix_incache <- NULL
    
    set <- function(y)
    {
        x <<- y ## asign a value, different from the current environment
        matrix_incache <<- NULL
    }
    
    get <- function() x
    set_matrix <- function(inverse) matrix_incache <<- inverse
    get_inverse <- function() matrix_incache
    #return 
    list(set = set, get = get, set_matrix = set_matrix, get_inverse = get_inverse)
}



#Calculates the inverse of matrix_incache.

cacheSolve <- function(x, ...) 
{
    m <- x$getinverse()
    
    if(!is.null(m))
    {
        message("Getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
