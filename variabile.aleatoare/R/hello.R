# variabile.aleatoare
#
# This is a package that implements different functions regarding
# continous random variables
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Proiect de Andrei Mihailescu si David Bucur

constantaNormalizare <- function(f) {
  area = integrate(f,-Inf,Inf)$value #compute the area under the curve
  1/area
}

marginalX <- function(f, x) {
  ff <- function(val){f(x,val)} #helper function
  integrate(Vectorize(ff),-Inf,Inf)$value
}

marginalY <- function(f, y) {
  ff <- function(val){f(val,y)} #helper function
  integrate(Vectorize(ff),-Inf,Inf)$value
}

conditionalXY <- function(f,x,y){
  tem <- marginalY(f,y)
  if(tem==0){
    0 #undefined
  }
  else{
    f(x,y)/tem #return the computed conditional
  }
}

conditionalYX <- function(f,x,y){
  tem <- marginalX(f,x)
  if(tem==0){
    0 #undefined
  }
  else{
    f(x,y)/tem #return the computed conditional
  }
}

addXY <- function (f,s){
  fh <- function(x){ #helper function implementing the convolution
    marginalX(f,x) * marginalY(f,s-x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

diffXY <- function (f,d){
  fh <- function(x){ #helper function implementing the convolution
    marginalX(f,x) * marginalY(f,d+x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

medie <- function(f){
  fh <- function(x){ #helper function
    x * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

dispersie <- function(f){
  med = medie(f)
  fh <- function(x){ #helper function
    ((x-med)^2) * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

momentInitial1 <- function(f){
  fh <- function(x){ #helper function
    x * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

momentInitial2 <- function(f){
  fh <- function(x){ #helper function
    (x^2) * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

momentInitial3 <- function(f){
  fh <- function(x){ #helper function
    (x^3) * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

momentInitial4 <- function(f){
  fh <- function(x){ #helper function
    (x^4) * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

momentCentrat1 <- function(f){
  med = medie(f)
  fh <- function(x){ #helper function
    (x-med) * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

momentCentrat2 <- function(f){
  med = medie(f)
  fh <- function(x){ #helper function
    ((x-med)^2) * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

momentCentrat3 <- function(f){
  med = medie(f)
  fh <- function(x){ #helper function
    ((x-med)^3) * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}

momentCentrat4 <- function(f){
  med = medie(f)
  fh <- function(x){ #helper function
    ((x-med)^4) * f(x)
  }
  integrate(Vectorize(fh),-Inf,Inf)$value #return the integral
}
