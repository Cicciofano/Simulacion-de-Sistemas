library(parallel)
dim <- 30
num <-  dim^2
nucleos <- 5
actual <- matrix(rep(0, num), nrow=dim, ncol=dim)
n <- round(runif(nucleos, 1, num))


for (i in 1:nucleos){
  actual[n[i]]=i
}
print(actual)

suppressMessages(library("sna"))
png("p2r1.png")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
graphics.off()

paso <- function(pos) {
  columna <- ((pos - 1) %% dim) + 1
  fila <- floor((pos - 1) / dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  
  if (actual[fila,columna]== 0){
    if((sum(vecindad)) > 0){
      vector <- c()
      for(k in 1:length(vecindad)){
        if(vecindad[k] != 0 ){
          vector <- c(vector, vecindad[k])
        }
      }
      nnucleo <- vector[1]
      return(nnucleo)
    }
    else {return (actual[fila, columna])}
  }
  else {return (actual[fila, columna])}
}


cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for (iteracion in 1:50) { 
  
  clusterExport(cluster, "actual")
  siguiente <- parSapply(cluster, 1:num, paso)
  print(siguiente)
  
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  salida = paste("p2r1_t", iteracion, ".png", sep="")
  tiempo = paste("Paso", iteracion)
  png(salida)
  plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
  graphics.off()
  
  if (all(actual != 0)) {
    break;
  }
}
stopCluster(cluster)
