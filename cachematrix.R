## My R programming assignment 2
## There are 2 functions for matrix solving using holding in memory already solved matrix to save time
## And there is one extra function to generate solvable matrix.

## This is somehing like object with methods. 
## Operator <<-  means that we are setting value to variable on the "upper" environment 

makeCacheMatrix <- function(x = matrix()) {
   solved_x <-NULL 
   
   set <- function(y) {
     x <<- y
     m <<- NULL
   }   
      
   get <- function()  x  ##return x 
   setsolved <- function (solved_m)  solved_x <<- solved_m
   getsolved <- function ()  solved_x  ## return solved_x
   
   #returning following list  of  functions
   list(set = set, get = get, setsolved = setsolved, getsolved = getsolved ) 
   
}




## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolved()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolved(m)
        m
        
}





## This function gives us random triag matrix size n
## Trial matrix is alvays solvable
## n - dimension of matrix, r - range of random values
get_triag_matrix <- function(n,r) {
  m <- matrix ()
  m <- sample(r,n)
  
  for(i in 1:(n-1)){
    m<-rbind(m,c(rep(0,i),sample(r,n-i)))     
  } 
  
  return(m)
}

# Geting sovable matrix
m1 <- get_triag_matrix(4,4) 

# Initiating matrix object which can hold cache 
cached_m <- makeCacheMatrix(m1)

# First time calclating
cacheSolve(cached_m)

# Second time getting from cache
cacheSolve(cached_m)

