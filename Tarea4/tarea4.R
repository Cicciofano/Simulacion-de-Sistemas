grietas <- data.frame()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

celda <-  function(pos) {
  fila <- floor((pos - 1) / n) + 1
  columna <- ((pos - 1) %% n) + 1
  if (zona[fila, columna] > 0) { # es una semilla
    return(zona[fila, columna])
  } else {
    cercano <- NULL # sin valor por el momento
    menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
    for (semilla in 1:k) {
      dx <- columna - x[semilla]
      dy <- fila - y[semilla]
      dist <- sqrt(dx^2 + dy^2)
      if (dist < menor) {
        cercano <- semilla
        menor <- dist
      }
    }
    return(cercano)
  }
}

inicio <- function() {
  direccion <- sample(1:4, 1)
  xg <- NULL
  yg <- NULL
  if (direccion == 1) { # vertical
    xg <- 1
    yg <- sample(1:n, 1)
  } else if (direccion == 2) { # horiz izr -> der
    xg <- sample(1:n, 1)
    yg <- 1
  } else if (direccion == 3) { # horiz der -> izq
    xg <- n
    yg <- sample(1:n, 1)
  } else { # vertical al reves
    xg <- sample(1:n, 1)
    yg <- n
  }
  return(c(xg, yg))
}

for (nn in seq(1,7, by=2)){
  for (kk in seq(1,7, by=2)){
  
  n <-  nn*40
  zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
  k <- kk*12
  x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
  y <- rep(0, k) # igual como las coordenadas y de las semillas
      
  for (semilla in 1:k) {
    while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
      fila <- sample(1:n, 1)
      columna <- sample(1:n, 1)
      if (zona[fila, columna] == 0) {
        zona[fila, columna] = semilla
        x[semilla] <- columna
        y[semilla] <- fila
        break
      }
    }
  }
  
  celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
  
  voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
  rotate <- function(x) t(apply(x, 2, rev))
  
  vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
  for (dx in -1:1) {
    for (dy in -1:1) {
      if (dx != 0 | dy != 0) { # descartar la posicion misma
        vp <- rbind(vp, c(dx, dy))
      }
    }
  }
  names(vp) <- c("dx", "dy")
  vc <- dim(vp)[1]
  
  propaga <- function(replica) {
    # probabilidad de propagacion interna
    prob <- 1
    dificil <- 0.99
    grieta <- voronoi # marcamos la grieta en una copia
    i <- inicio() # posicion inicial al azar
    xg <- i[1]
    yg <- i[2]
    largo <- 0
    while (TRUE) { # hasta que la propagacion termine
      grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
      largo <-  largo + 1
      frontera <- numeric()
      interior <- numeric()
      for (v in 1:vc) {
        vecino <- vp[v,]
        xs <- xg + vecino$dx # columna del vecino potencial
        ys <- yg + vecino$dy # fila del vecino potencial
        if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
          if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
            if (voronoi[yg, xg] == voronoi[ys, xs]) {
              interior <- c(interior, v)
            } else { # frontera
              frontera <- c(frontera, v)
            }
          }
        }
      }
      elegido <- 0
      if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
        if (length(frontera) > 1) {
          elegido <- sample(frontera, 1)
        } else {
          elegido <- frontera # sample sirve con un solo elemento
        }
        prob <- 1 # estamos nuevamente en la frontera
      } else if (length(interior) > 0) { # no hubo frontera para propagar
        if (runif(1) < prob) { # intentamos en el interior
          if (length(interior) > 1) {
            elegido <- sample(interior, 1)
          } else {
            elegido <- interior
          }
          prob <- dificil * prob # mas dificil a la siguiente
        }
      }
      if (elegido > 0) { # si se va a propagar
        vecino <- vp[elegido,]
        xg <- xg + vecino$dx
        yg <- yg + vecino$dy
      } else {
        break # ya no se propaga
      }
    }
    return(largo)
  }
  
  atr<-500
  largos <- foreach(r = 1:atr, .combine=c) %dopar% propaga(r)
  
  a<-(largos)
  grietas <- rbind(grietas,a)
  
}


}

stopImplicitCluster()


boxplot(t(grietas))



matriz<- data.frame()

for(s in 1:16){
  ds<-(summary(as.numeric(grietas[s,])))
  matriz<-rbind(matriz, ds)
}

for(d in seq(1,7, by=2)){ 
dimension<-c(dimension,rep((d*40),4))
}

semillas<-c(12,36,70,84)
semillas<-rep(semillas,4)

matriz<-cbind(matriz,dimension,semillas)
write.csv(matriz, "msummary.csv")


v<-c()
v1<-c()
for(n in 1:16){
  v<-c(as.numeric(grietas[n,]))
  v1<-c(v1,v)
}

dimen<-c(rep(40, (4*atr)),rep(120, (4*atr)),rep(200, (4*atr)),rep(280, (4*atr)))
ad<-c(12,36,70,84)
csemi<-c(rep(ad,each=atr),rep(ad,each=atr),rep(ad,each=atr),rep(ad,each=atr))
datos3<- matrix(c(csemi,dimen, v1 )   ,ncol=3, nrow = length(dimen))
colnames(datos3)<-c("semillas","dimensiones","largos")

kruskal.test(csemi~v1)
kruskal.test(dimen~v1)

write.csv(datos3, "datos.csv")

LARGO<-datos3[,3]
DIMENSIONES<-datos3[,2]
SEMILLAS<-datos3[,1]

regre<-lm(LARGO~DIMENSIONES+SEMILLAS)
summary(regre)
qw<-resid(regre)
qw1<-ad.test(qw)
hist(qw, freq = F, ylim=c(0,0.025))
lines(density(qw), col="red")
text( mean(density(qw)$x),max(density(qw)$y), "AD Valor p:"     ) 
text( mean(density(qw)$x)+30,max(density(qw)$y), qw1[2]     ) 

png("t4ls.png")
boxplot(largos~semillas, data=datos3)
graphics.off()
png("t4ld.png")
boxplot(largos~dimensiones, data=datos3)
graphics.off()