
library("parallel")
time1 <- Sys.time()

pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

dominadores.s <- function(i){
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  return(cuantos)
}

evaluaciones <- function(i){
  val <- c()
  for (j in 1:k) { # para todos los objetivos
    
    val <- c(val, eval(obj[[j]], sol[i,], tc))
  }
  return(val)
}

objetivo <- function(i){
  return(obj[[i]] <- poli(vc, md, tc))
}

cluster <- makeCluster(detectCores()- 1)
clusterExport(cluster, c("domin.by", "dominadores.s", "sign", "evaluaciones", "eval", "objetivo", "poli", "n"))
#for(k in c(seq(2, 22, 2))){

vc <- 4
md <- 3
tc <- 5
k <- 2 # cuantas funciones objetivo
obj <- list()

#clusterExport("vc", "md", "tc") ##### aqu�!!!!!!!

for (i in 1:k) {
  obj[[i]] <- poli(vc, md, tc)
}
clusterExport(cluster, c("k", "tc", "obj"))
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
#val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
val <- matrix()
clusterExport(cluster, c("tc", "obj", "sol"))

val <- parSapply(cluster, 1:n, evaluaciones) ##### aqu�!!!!!!!
val <- t(val)
#for (i in 1:n) { # evaluamos las soluciones

#for (j in 1:k) { # para todos los objetivos

#  val[i, j] <- eval(obj[[j]], sol[i,], tc)
#}
#}
#val== val1

mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
png("p11_init.png")
plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
graphics.off()
png("p11_mejores.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=1.5)
points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=1.5)
graphics.off()
no.dom <- logical()
dominadores <- integer()
for (i in 1:n) {
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, cuantos == 0) # nadie le domina
}
frente <- subset(val, no.dom) # solamente las no dominadas
if(minim[1]){
  a <- sort(frente[,1])
}
if(!minim[1]){
  a <- sort(frente[,1], decreasing = T)
}
aux <- data.frame()
for(i in 1:dim(frente)[1]){
  aux <- rbind(aux, frente[a[i]==frente[,1]])
}
frente.n <- aux[seq(1, dim(aux)[1], by =2),]
png("p11_frente.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
points(frente.n[,1], frente.n[,2], col="red", pch=16, cex=1.5)
graphics.off()
library(ggplot2) # recordar instalar si hace falta
data <- data.frame(pos=rep(0, n), dom=dominadores)
png("p11_violin.png")
gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
  xlab("") +
  ylab("Frecuencia") +
  ggtitle("Cantidad de soluciones dominantes")
graphics.off()
time2 <- Sys.time()
tiempo <- time2 - time1
print (tiempo)
#}

