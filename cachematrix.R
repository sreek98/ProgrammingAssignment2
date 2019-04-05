    

## These two functions create a special object that stores given matrix 
## and its cached inversion as well as 
## Usage would be like:
## 1. Take an inversible matrix m
## 2. Use makeCacheMatrix function to create special object that store matrix m
## and its invertion (once it's calucated) and offers function to setting and getting
## matrix and its invertion
##    myMatrix <- makeCacheMatrix(m)
## 3. Use cacheSolve(myMatrix) to obtain cached inversion of matrix or
## if it's not been yet calculated, calculate inversion and store it in cache
## Please note that cacheSolve() function uses function (methods) created by
## makeCacheMatrix() function




##
## Function creates special object that store matrix given as an argument
## and its invertion (once it's calucated) and offers function to setting and getting
## matrix and its invertion

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL ##Placeholder for inverted matrix (cached)
        
        
        set <- function(y){
                x <<- y
        }
        get <- function() x
        setinversion <- function(i) inv <<- i
        getinversion <- function() inv
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}




##
##Function checks if there is already invertion of given matrix (please note that
##argument here is not exact a matrix, but rather a special object created by makeCacheMatrix function
## that stores an original matrix). If it is it returns this value. If not solve
## matrix invertion and stores it in the cache for the later use

cacheSolve <- function(z, ...) {
        ## Just to avoid confusing of names in the process of code analysis I changed
        ## name of argument from 'x' to 'z' (comparing with Professor Peng's draft)

        ##First of all we try to check if there is cached inversion
        ##if there is one, return it and finish the function
        
        inv <- z$getinversion()
        if (!is.null(inv)){
                message("using cache data")
                return(inv)
        }
        ##If there is no inversion in cache, calculate one... (see next comment)
        
        matrix <- z$get()
        inv <- solve(matrix, ...)
        
        ##...and set it in the cache to later use
        z$setinversion(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
