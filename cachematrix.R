# The following functions cooperatively caches the inverse of a matrix

# The following function creates a list which sets up the matrix to be 
    # inverted and cached

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set_mtx <- function(mtx1){
        x <<- mtx1 
        inv <- NULL 
    }
    get_mtx <- function(){x} 
    set_inv <- function(minv){inv <<- minv}
    get_inv <- function(){inv}
    list(set_mtx = set_mtx,
         get_mtx = get_mtx,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("Getting cached inverse...")
        return(inv)
    }
    data <- x$get_mtx()
    inv <- solve(data)
    x$set_inv(inv)
    inv
}
