iteraciones <- 0
#este vector se usa para distribuir las celdas de inicio a distintas probabilidades
vector <- seq(0,1, by = 0.05)
names (vector) <- c(0, 0.5, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
datos <-  data.frame()
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
library(parallel)

dim <- 10
num <-  dim^2

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

#este for repite el programa tantas veces como probabilidades a comparar
for (y in vector)
  {
  #este for repite el programa para crear n juegos por cada probabilidad
  for (x in 1:50)
  {
  actual <- matrix(1*round(runif(num)>y), nrow=dim, ncol=dim)
  suppressMessages(library("sna"))
  for (iteracion in 1:15) 
    {
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)
    if (sum(siguiente) == 0){ 
      break;
     }
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    }
  iteraciones [x] <- iteracion
  }
  datos <- rbind(datos, iteraciones)
  }
stopCluster(cluster)
boxplot(t(datos), names= as.character(vector), xlab="Probabilidad", ylab="Pasos", main="Número de pasos por probabilidad")
