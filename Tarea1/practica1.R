#se agregó este contador de tiempo que marca la fecha y hora en que el programa inició
inicioT <- Sys.time()    

repetir <- 100
pasos <- 200

library(parallel)

datos <-  data.frame()

#esta parte del código se modificó del original
#se reemplaza la variable que guarda la distancia alcanzada por una que guarda el número de veces que se regresaba al origen
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

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "pasos")
clusterExport(cluster, "experimento")

for (dimension in 1:8)
  { 
   clusterExport(cluster, "dimension")
   resultado <- parSapply(cluster, 1:repetir, experimento)
   datos <- rbind(datos, resultado)
  } 

stopCluster(cluster)


#gráfica de diagramas caja bigote que muestra las veces que se regresaba al origen por cada dimensión
boxplot(data.matrix(datos), use.cols=FALSE, xlab="Dimensión", ylab="Número de regresos al origen", main="Regresos al origen por dimensión")

#aquí el contador guarda la fecha y hora en que el programa terminó
finT <- Sys.time()

#aquí se muestra el tiempo total que tomó el programa para ejecutarse
tiempo <- finT - inicioT
