primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if (n>i && (n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

desde <- 100
hasta <-  300
original <- desde:hasta
invertido <- hasta:desde
replicas <- 5
suppressMessages(library(doParallel))
suppressMessages(library(stats))

matriz  <- data.frame()
matriz2 <- data.frame()
matriz3 <- data.frame()


for (n in 1:detectCores()){
  registerDoParallel(n)
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()
  
  
  for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
  }
  matriz2<- rbind(matriz2, as.numeric(ot), as.numeric(it), as.numeric(at))

  stopImplicitCluster()
  
}

for (p in 1:(3*(detectCores()))){ 
  matriz <- rbind(matriz, mean(as.numeric(matriz2[p,])))
}

matriz<-t(matrix(as.matrix(matriz),nrow = 3,ncol = detectCores()))

for(f in 1:detectCores()){
  f<- c(matriz[f,])
  matriz3<-rbind(matriz3,f)
}

total<-length(dimnames(matriz3)[[1]])
for(a in 1:total){
  dimnames(matriz3)[[1]][a]=paste(a, "núcleos")
}

colnames(matriz3)<-c("ot", "it", "at")

png("t3.png")
barplot(as.matrix(t(matriz3)), legend=c("Ordenados", "Invertidos", "Aleatorios"), 
        beside = T, xlab = "Número de núcleos usados", ylab = "Tiempos promedio (segundos)")

write.csv(matriz3, "tiempos.csv")

d <- density(c(t(matriz2[, 1:replicas])))
png("t3d.png")
plot(d)

graphics.off()


matrizo <- matrix()
for (o in 1:(3*(detectCores()))){
  v <- matriz2[o,]
  matrizo <- cbind((matrizo), v)
}
matrizo <- matrizo[,2:(replicas*(3*(detectCores()))+1)]

tmatrizo<-c(t(matrizo))


nucleos <- matrix()
for (n in 1:detectCores()){
  nucleo <- c(rep(n, times = (replicas*3)))
  nucleos <- c(nucleos, nucleo)
}
nucleos <- as.matrix(nucleos)[(2:(replicas*(3*(detectCores()))+1)),]
nucleos <- c(nucleos)
nucleos <- as.numeric(nucleos)

oia <-matrix()
for (ñ in 1:(replicas*(detectCores()))){
  v <- c(1,2,3)
  oia <- c(oia, v)
}
oia <- as.matrix(oia)[(2:(replicas*(3*(detectCores()))+1)),]
oia <- c(oia)

datosk <- data.frame()

datosk<-rbind(datosk, tmatrizo, nucleos, oia)

datosk<-t(datosk)


kruskal.test(datosk[,1]~datosk[,2])
kruskal.test(datosk[,1]~datosk[,3])


