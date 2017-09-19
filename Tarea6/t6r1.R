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

infectados.prob <- data.frame()
pi <- seq(0.05, 0.50, 0.05)
for (proba in pi) {
  
infectados.rep <- c()
for (k in 1:10){
l <- 1.5
n <- 50
pr <- 0.02
pv <- 0.35
v <- l / 30
agentes <- data.frame(x = runif(n, 0, l), y = runif(n, 0, l), dx = runif(n, -v, v), dy = runif(n, -v, v), estado = sample(c("S", "I", "R"), n, replace=TRUE, c(1-(proba+pv), proba, pv)))
agentes$estado = as.factor(agentes$estado)
levels(agentes$estado) = c(levels(agentes$estado), "R")
epidemia <- integer()
r <- 0.1
tmax <- 30
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
}
infectados.rep[k] <- max(epidemia) 
}
infectados.prob <- rbind(infectados.prob, infectados.rep)
}
rownames(infectados.prob) <- pi
colnames(infectados.prob) <- c(1:k)
stopImplicitCluster()

boxplot(t(infectados.prob), xlab="Probabilidades distintas", ylab="Infectados")
