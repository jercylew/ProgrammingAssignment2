## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m_matSelf = matrix()) {
        m_matSolve <- NULL
        set <- function(matValue) {
                m_matSelf <<- matValue
                m_matSolve <<- NULL
        }
        get <- function() m_matSelf
        setSolve <- function(matValue) m_matSolve <<- matValue
        getSolve <- function() m_matSolve
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(m_matSelf, ...) {
        ## Return a matrix that is the inverse of 'x'
        matData <- m_matSelf$get()

        if (nrow(matData) != ncol(matData)) {
                stop("Invalid input matrix")
        }

        nSize <- nrow(matData)
        matSolve <- m_matSelf$getSolve()
        if(!is.null(matSolve)) {
                message("getting cached data")
                return(matSolve)
        }

        matIdentity <- matrix(rep(0, nSize*nSize), ncol = nSize, nrow = nSize)
        for (i in seq_len(nSize)) {
                matIdentity[i, i] <- 1
        }

        matSolve <- solve(matData, matIdentity)
        m_matSelf$setSolve(matSolve)

        matSolve
}
