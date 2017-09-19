#ACTUALIZAR PARALELIZADO
#FUNCIONA

actualizar <- function(){
  a <- agentes[i, ]
  if (contagios[i] & a$estado == "S") {
    a$estado <- "I"
  } else if (a$estado == "I") { # ya estaba infectado
    if (runif(1) < pr) {
      a$estado <- "R" # recupera
    }
  }
  a$x <- a$x + a$dx
  a$y <- a$y + a$dy
  if (a$x > l) {
    a$x <- a$x - l
  }
  if (a$y > l) {
    a$y <- a$y - l
  }
  if (a$x < 0) {
    a$x <- a$x + l
  }
  if (a$y < 0) {
    a$y <- a$y + l
  }
  return(as.vector(a))
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
maxinfectados <- c()

maxmin <- matrix()
for(x in 1:1){
pi <- 0.05*x
for(xx in 1:10){
  
  l <- 1.5
  n <- 50
 
  pr <- 0.02
  pv <- 0.5
  v <- l / 30
agentes <- data.frame(x = runif(n, 0, l), y = runif(n, 0, l), dx = runif(n, -v, v), dy = runif(n, -v, v), estado = sample(c("S", "I", "R"), n, replace=TRUE, c(1-pi, pi, pv)))
agentes$estado = as.factor(agentes$estado)
levels(agentes$estado) = c(levels(agentes$estado), "R")
epidemia <- integer()
r <- 0.1
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
for (tiempo in 1:tmax) {
  infectados <- dim(agentes[agentes$estado == "I",])[1]
  epidemia <- c(epidemia, infectados)
  if (infectados == 0) {
    break
  }
  contagios <- rep(FALSE, n)
  for (i in 1:n) { # posibles contagios
    a1 <- agentes[i, ]
    if (a1$estado == "I") { # desde los infectados
      for (j in 1:n) {
        if (!contagios[j]) { # aun sin contagio
          a2 <- agentes[j, ]
          if (a2$estado == "S") { # hacia los susceptibles
            dx <- a1$x - a2$x
            dy <- a1$y - a2$y
            d <- sqrt(dx^2 + dy^2)
            if (d < r) { # umbral
              p <- (r - d) / r
              if (runif(1) < p) {
                contagios[j] <- TRUE
              }
            }
          }
        }
      }
    }
  }
  final <- data.frame()
  final <- rbind(final ,foreach(i = 1:n, .combine=c) %dopar% actualizar())
  final1 <- data.frame()
  colnames(final)<-rep(seq(1:5),50)
  for(y in 1:50){
    v <- (final[,(5*y-4):(5*y)])
    v1 <- c()
    final1 <- rbind(final1,v)
  }
  colnames(final1) <- c("x","y","dx","dy","estado")
  agentes <- final1
  aS <- agentes[agentes$estado == "S",]
  aI <- agentes[agentes$estado == "I",]
  aR <- agentes[agentes$estado == "R",]
  tl <- paste(tiempo, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  #salida <- paste("p6_t", tl, ".png", sep="")
  #tiempo <- paste("Paso", tiempo)
  #png(salida)
  #plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
  #if (dim(aS)[1] > 0) {
  #  points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
  #}
  #if (dim(aI)[1] > 0) {
  #  points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
  #}
  #if (dim(aR)[1] > 0) {
  #  points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
  #}
  #graphics.off()
}
maxinfectados <- c(maxinfectados, max(epidemia)/n)
print(x)
}
print(maxinfectados)
maxmin <- rbind(maxmin, maxinfectados)
maxinfectados<-c()
}
stopImplicitCluster()

write.csv(maxmin, "maxminar2.csv")
#png("p6e.png", width=600, height=300)
#plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentahe de infectados")
#graphics.off()
datos<-data.frame()
v1<-c(0.06, 0.00, 0.02, 0.40, 0.30, 0.24, 0.40, 0.34, 0.44, 0.44)
v2<-c(0.48, 0.36, 0.36, 0.34, 0.26, 0.24, 0.04, 0.48, 0.32, 0.18)
v3<-c(0.40 ,0.50 ,0.30 ,0.40 ,0.42 ,0.26 ,0.50 ,0.30, 0.36, 0.40)
v4<-c(0.36, 0.32, 0.40, 0.40, 0.18, 0.42, 0.48, 0.38, 0.36, 0.26)
v5<-c(0.38, 0.38, 0.36, 0.42, 0.44, 0.40, 0.44, 0.40, 0.50, 0.42)
v6<-c(0.38, 0.44, 0.36, 0.32, 0.40, 0.58, 0.38, 0.42, 0.34, 0.44)
v7<-c(0.50, 0.34, 0.32, 0.36, 0.40, 0.36, 0.42, 0.38, 0.32, 0.36)
v8<-c(0.42, 0.32, 0.34, 0.42, 0.28, 0.30, 0.40, 0.46 ,0.42 ,0.40)
v9<-c(0.46, 0.46, 0.30, 0.46, 0.44, 0.48, 0.36, 0.50, 0.48, 0.42)
v10<-c(0.50, 0.44, 0.36, 0.50, 0.48, 0.46, 0.52, 0.36, 0.24, 0.54)
datos1<-data.frame()
datos1<-rbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
rownames(datos1)<-c(0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50)
colnames(datos1)<-c(1,2,3,4,5,6,7,8,9,10)
png("p6r2.png", width=600, height=600)
boxplot(t(datos1), xlab="Probabilidad", ylab="Porcentaje de infectados")
graphics.off()
write.csv(datos1, "datos1.csv")
