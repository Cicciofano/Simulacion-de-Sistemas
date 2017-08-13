#esta sección del código se modificó para analizar el reto 2
#se usó la función sapply para comparar el qué tan ineficiente es el no aprovechar paralelismo 

for (dimension in 1:8)
{ 
  clusterExport(cluster, "dimension")
  resultado <- sapply(1:repetir, experimento)
  datos <- rbind(datos, resultado)
}
