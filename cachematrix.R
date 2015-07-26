## File: implementation of the Caching the Inverse of a Matrix assignment
## Link: https://class.coursera.org/rprog-030/human_grading/view/courses/975104/assessments/3
## Date: July 26th 2015
## Author: Freek de Bruijn


## This file contains two functions that together support caching the inverse of matrices, which can save time when the
## inverse of a matrix is used often and calculating the inverse takes a significant amount of time. This approach does
## however use more memory.
##
## The makeCacheMatrix function creates a matrix/inverse list object that stores a matrix, can store the inverse, and 
## has several functions that allow the matrix and the inverse to be retrieved and modified. The makeCacheMatrix
## function is supposed to be called first and returns the matrix/inverse list object.
##
## The cacheSolve function returns the inverse of a matrix (from a matrix/inverse list object), either by getting it 
## from the cache or else by determining the inverse (and putting it in the cache for a potential next time). The 
## cacheSolve function needs a matrix/inverse list object as its first parameter, which can be created by calling the 
## makeCacheMatrix function.


## A short example with both functions:
##
# > matrixWithInverse <- makeCacheMatrix(matrix(c(4, 3, 3, 2), 2, 2))
# > matrixWithInverse$getInverse()
# NULL
# > cacheSolve(matrixWithInverse)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > matrixWithInverse$getInverse()
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4


## Create a matrix/inverse list object that:
## - stores a matrix;
## - can store the inverse matrix;
## - has several functions that allow the matrix and the inverse to be retrieved and modified.
##
## Parameter 'cachedMatrix' contains the matrix to use.
##
## Return the list object that was created.
makeCacheMatrix <- function(cachedMatrix = matrix()) {
    cachedInverse <- NULL
    setMatrix <- function(newMatrix) {
        cachedMatrix <<- newMatrix
        cachedInverse <<- NULL
    }
    getMatrix <- function() cachedMatrix
    setInverse <- function(newInverse) cachedInverse <<- newInverse
    getInverse <- function() cachedInverse
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of a matrix (from a matrix/inverse list object), either by getting it from the cache or else by 
## determining the inverse matrix (and putting it in the cache for a potential next time).
##
## Parameter 'matrixWithInverse' contains the matrix/inverse list object to use.
##
## Return the inverse matrix.
cacheSolve <- function(matrixWithInverse, ...) {
    cachedInverse <- matrixWithInverse$getInverse()
    if (is.null(cachedInverse)) {
        cachedInverse <- solve(matrixWithInverse$getMatrix(), ...)
        matrixWithInverse$setInverse(cachedInverse)
    }
    cachedInverse
}






########################################################################################################################

## An alternative implementation in a single function:
##
# makeMatrixWithCachedInverse <- function(cachedMatrix = matrix()) {
#     cachedInverse <- NULL
#     setMatrix <- function(newMatrix) {
#         cachedMatrix <<- newMatrix
#         cachedInverse <<- NULL
#     }
#     getMatrix <- function() cachedMatrix
#     getInverse <- function(...) {
#         if (is.null(cachedInverse))
#             cachedInverse <<- solve(cachedMatrix, ...)
#         cachedInverse
#     }
#     list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse)
# }
