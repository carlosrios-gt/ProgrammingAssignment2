## Proyecto Asignado: 2
## Inicio de la propiedad inversa: makeCacheMatrix
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    MatSet<- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    get <- function() {
    	m
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        i
    }
    list(MatSet= MatSet, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##Creacion de Funcion de cacheSolve
    
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if( !is.null(m) ) {
            message("Obteniendo datos")
            return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}

