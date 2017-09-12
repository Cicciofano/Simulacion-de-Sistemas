suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }

suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) 

parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}

desde <- 3
hasta <- 7
cuantos <- 500

v<-data.frame()
tiempos <- data.frame()
dif<-data.frame()

for (n in 1:8){

pedazo <- (5000*(n^2))
print("replica")
print(pedazo)
for(r in 1:10){
inicioT <- Sys.time()
montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
integral <- sum(montecarlo) / (cuantos * pedazo)
finT <- Sys.time()
tiempo <- finT - inicioT

tiempos <- rbind(tiempos, tiempo)
v1<-c(((pi / 2) * integral))
v<-rbind(v,v1)
d1<-c(abs(0.048834-v1))
dif<-rbind(dif,d1)
}
}
stopImplicitCluster()


matriz<-data.frame()
pedazos<-c()
for(p in 1:8){
pedazos1<-c(rep((50000*p),10))
pedazos<-c(pedazos,pedazos1)
}
matriz<-cbind(v,pedazos,tiempos,dif)
write.csv(matriz, "matrizt5.csv")

png(filename = "val5.png")
boxplot(v[1:10,], v[11:20,], v[21:30,], v[31:40,], v[41:50,], v[51:60,], v[61:70,], v[71:80,])
abline(h=0.048834, col="red")
png(filename = "val.png")
boxplot(dif[1:10,], dif[11:20,], dif[21:30,], dif[31:40,], dif[41:50,], dif[51:60,], dif[61:70,], dif[71:80,])
png(filename = "tiem.png")
boxplot(tiempos[1:10,], tiempos[11:20,], tiempos[21:30,], tiempos[31:40,], tiempos[41:50,], tiempos[51:60,], tiempos[61:70,], tiempos[71:80,])
graphics.off()
plot(matriz[,2], matriz[,3])
abline(h=0.048834, col="red")

