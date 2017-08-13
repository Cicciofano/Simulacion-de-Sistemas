inicioT <- Sys.time()    

repetir <- 1000
pasos <- 2000

library(parallel)

datos <-  data.frame()

experimento <- function(replica) 
  {
   pos <- rep(0, dimension)
   veces <- 0
   for (t in 1:pasos) 
     {
      cambiar <- sample(1:dimension, 1)
      cambio <- 1
      if (runif(1) < 0.5) 
        {
         cambio <- -1
        }
      pos[cambiar] <- pos[cambiar] + cambio
      if (all(pos == 0)) 
        {
         veces <- veces + 1
        }
     }
   return(veces)
  }

#esta sección del código se modificó para analizar el reto 2
#se eliminaron las variables relacionadas con los cluster
#se usó la función sapply para comparar qué tan ineficiente es el no aprovechar paralelismo 
for (dimension in 1:8)
  { 
   resultado <- sapply(1:repetir, experimento)
   datos <- rbind(datos, resultado)
  } 


boxplot(data.matrix(datos), use.cols=FALSE, xlab="Dimensión", ylab="Número de regresos al origen", main="Regresos al origen por dimensión")

finT <- Sys.time()
tiempo <- finT - inicioT
