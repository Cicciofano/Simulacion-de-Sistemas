for (dimension in 1:8)
{ 
  clusterExport(cluster, "dimension")
  resultado <- parSapply(cluster, 1:repetir, experimento)
  datos <- rbind(datos, resultado)
}