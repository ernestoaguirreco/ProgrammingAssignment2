 
# para hacer cache la inversa de la matriz y procesarla repetidamente
# las siguientes dos funciones nos sirven para hacer cache a la inversa de la matriz. 
 
   
 # makeCacheMatrix crea una lista que contiene una funcion para 
 # 1. dar los valores a la matriz:                set the value of the matrix 
 # 2. obtener el valor de la matriz:              get the value of the matrix 
 # 3. dar el valor de la inversa de la matriz:    set the value of inverse of the matrix 
 # 4. obtener el valor de la matriz inversa:      get the value of inverse of the matrix 
 makeCacheMatrix <- function(x = matrix()) { 
     inv <- NULL 
     set <- function(y) { 
         x <<- y 
         inv <<- NULL 
     } 
     get <- function() x 
     setinverse <- function(inverse) inv <<- inverse 
     getinverse <- function() inv 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
 } 
 
 # La siguiente funcion regresa la matriz inversa. Primero checa si 
 # la inversa de la matriz ya ha sido calculada. Si es asì, toma el resultada y se salta 
 # calculo. Si no, calcula la inversa, y pone el valor de la inversa en el cache mediante 
 # la funciòn setinverse. 
 
 
 # Esta funcion asume que la funciòn es invertible. 
 cacheSolve <- function(x, ...) { 
     inv <- x$getinverse() 
     if(!is.null(inv)) { 
         message("getting cached data.") 
         return(inv) 
     } 
     data <- x$get() 
     inv <- solve(data) 
     x$setinverse(inv) 
     inv 
 } 
## Soluciòn: Result
## x<-1:9
## dim(x)<-c(3,3)
## x=rbind(c(1,2,3),c(4,7,12),c(0,1,3))
## m$get()
##      [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    4    7   12
##[3,]    0    1    3
## cacheSolve(m)
##getting cached data.
##          [,1]       [,2]          [,3]
##[1,] -3.000000  1.0000000 -1.000000e+00
##[2,]  4.000000 -1.0000000  5.551115e-17
##[3,] -1.333333  0.3333333  3.333333e-01
## cacheSolve(m)
##getting cached data.
##          [,1]       [,2]          [,3]
##[1,] -3.000000  1.0000000 -1.000000e+00
##[2,]  4.000000 -1.0000000  5.551115e-17
##[3,] -1.333333  0.3333333  3.333333e-01
 

