# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


getbajadaprecios<- function(){
  fichero <- system.file("extdata", "precio_m2 desde 2008.csv", package = "PackageBCN2")
  x <- read.csv(file = fichero, sep = ";")
  x$Barris <- as.character(x$Barris)
  x$X2011 <- as.numeric(x$X2011)
  x$X2008 <- as.numeric(x$X2008)
  x <- x[,c("Barris","X2011","X2008")]
  x$diferencia <- x$X2008 - x$X2011
  x <- x[order(-x$diferencia), ]
  return(x)
}


getestudios<- function(){
  fichero <- system.file("extdata", "Nivel academico por barrios_.csv", package = "PackageBCN2")
  x <- read.csv(file = fichero, sep = ";")
  x <- x[,c("Barris","TOTAL","Estudis.universitaris...CFGS.grau.superior","Sense..estudis")]
  x$Estudis.universitaris...CFGS.grau.superior <- as.numeric(x$Estudis.universitaris...CFGS.grau.superior)
  x$TOTAL <- as.numeric(x$TOTAL)
  x$con_estudios_universitarios <- x$Estudis.universitaris...CFGS.grau.superior/x$TOTAL*100
  x <- x[order(-x$con_estudios_universitarios), ]
  x$Sense..estudis <- x$Sense..estudis/x$TOTAL*100
  x$Sense..estudis <- sapply(x$Sense..estudis, function(num) {round(num, 3)})
  x <- x[order(-x$Sense..estudis), ]
  return(x)
}

getelec_auto_data<- function(){
  fichero <- system.file("extdata", "elec_autonomiques_2015__.csv", package = "PackageBCN2")
  x <- read.csv(file = fichero, sep = ";")
  x$Barris <- as.character(x$Barris)
  x$porcentaje_han_votado <- x$Votants/x$Electors*100
  x$porcentaje_votos_derechas  <- (x$PP+x$C.s)/x$Votants*100
  x$porcentaje_votos_izquierdas <- (x$PSC+x$CatSique..esPot..2.)/x$Votants*100
  x$porcentaje_votos_catalanistas <- x$JxSi..1./x$Votants*100
  return(x)
}
#Función obtengo datos de puntos wifi
getPuntsWifi <- function(){
  fichero <- system.file("extdata", "punts_wifi_.csv", package = "PackageBCN2")
  x <- read.csv(file = fichero, sep = ";")
  x <- x[,c("LATITUD","LONGITUD","EQUIPAMENT")]
  x$EQUIPAMENT <- as.character(x$EQUIPAMENT)
  x$LATITUD <- as.numeric(x$LATITUD)
  x$LONGITUD <- as.numeric(x$LONGITUD)
  return(x)
}
