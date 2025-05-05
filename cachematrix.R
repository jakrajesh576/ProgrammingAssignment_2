
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function() {
  mat <- NULL
  inv_cache <- NULL
  
  setMatrix <- function(x) {
    mat <<- x
    inv_cache <<- NULL
  }
  
  getMatrix <- function() {
    mat
  }
  
  getInverse <- function() {
    if (is.null(inv_cache)) {
      inv_cache <<- solve(mat)
    }
    inv_cache
  }
  
  list(setMatrix = setMatrix, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv_cache <- x$getInverse()
  inv_cache
}


# Create a special "matrix" object
special_matrix <- makeCacheMatrix()

# Set the matrix
special_matrix$setMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# Compute and cache the inverse
inverse_matrix <- cacheSolve(special_matrix)
print(inverse_matrix)

# Retrieve the cached inverse
cached_inverse <- cacheSolve(special_matrix)
print(cached_inverse)

