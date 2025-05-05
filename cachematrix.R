
makeCacheVector <- function(x = numeric()) {
  m <- NULL # Initialize the cached mean to NULL
  set <- function(y) {
    x <<- y  # Assign the new vector to x in the parent environment
    m <<- NULL  # Reset the cached mean to NULL
  }
  get <- function() x  # Return the vector
  setmean <- function(mean) m <<- mean  # Cache the mean
  getmean <- function() m # Return the cached mean
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cacheMean <- function(x, ...) {
  m <- x$getmean() # Retrieve the cached mean
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # Return the cached mean if it exists
  }
  data <- x$get() # Get the vector
  m <- mean(data, ...) # Compute the mean
  x$setmean(m) # Cache the mean
  m # Return the computed mean
}

# Example usage
v <- makeCacheVector(c(1, 2, 3, 4))
print(cacheMean(v)) # Computes and caches the mean
print(cacheMean(v)) # Retrieves the cached mean
