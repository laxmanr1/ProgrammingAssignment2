## Put comments here that give an overall description of what your
## functions do
##--------------------------------------------------------------
## called as follows:
## MyX <- matrix (rnorm(16), 4, 4)
## MyC <- makeCacheMatrix ()
## MyC$set (MyX)
## MyC$get ()
## cacheSolve (MyC)
##--------------------------------------------------------------

## Write a short comment describing this function
##--------------------------------------------------------------
## creates the cache variable & list of functions to be used by the "cacheSolve" function
##--------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

	## create the cache variable & initialize it
	m1 <- NULL

	## function to set the value of the matrix data
	## resets the cache so a calculation HAS to be performed to set it
	set <- function (y) {
		x <<- y
		m1 <<- NULL
	}

	## returns the matrix data
	get <- function() x

	## sets the cache variable to the passed value
	setInv <- function (mx) m1 <<- mx

	## return the value from the cache
	getInv <- function () m1

	## return the created functions to the working environment
	list (set = set, get = get, setInv = setInv, getInv = getInv)
}


##--------------------------------------------------------------
## Write a short comment describing this function
## this function returns the inverse of a matrix,
## calculating & caching the inverse if called the first time,
## returning the cached value if it had already been called
##--------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## get the cached value (NULL if it was not calculated & cached before)
	cache <- x$getInv ()

	## if NOT NULL, inverse was already calculated - just return it
	if (!is.null (cache)) {
		message ("Getting cached data")
		return (cache)
	}

	## we are here because the inverse had not been calculated before
	## get the matrix to get inverse of
	data <- x$get ()

	## calculate the inverse, wrapping the whole in a try-catch block so we know something went wrong and we gracefully exit
	## errors can happen, for example, if the matrix is not invertible
	tryCatch({
		## calculate the inverse
		cache <- solve (data)
	},
	error = function(errMsg) {
		message("Error:")
		message (errMsg)
		return(NA)
	},
	warning = function (errMsg) {
		message ("Warning:")
		message (errMsg)
		return(NA)
	},
	finally = {
		## if no error was found, set the solved inverse in the cache for later use
		x$setInv (cache)
	} )

	return (cache)
}
