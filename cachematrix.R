## Put comments here that give an overall description of what your
## functions do

## Expose les 4 fonctions qui gèrent la mise en cache (get,set,setmean,getmean)
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialisation
    m <- NULL
    ## Fonctions : on manipule des paires clefs/valeurs avec le résultat, stockées dans l'env de la fonction
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    
    ## On retourne la liste des fonctions
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## Gère effectivement la mise en cache à partir des 4 fonctions mises à dispo
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## On va chercher la valeur déjà stockée
    ## Si elle existe déjà, on a déjà fait le calcul: on retourne la valeur stockée
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Sinon on va chercher la valeur...
    data <- x$get()
    ## ...et avec solve on calcule l'inverse de la matrice
    m <- solve(data, ...)
    ## On l'enregistre et on la retourne
    x$setmean(m)
    m
}
## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667