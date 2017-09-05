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

grietas <- data.frame()

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

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

n <-  40
for (j in round(c(1, n/4, n/2, (3*n)/4))){
  for (r in round(c(n/4, n/2, (3*n)/4, n))){ 
    zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
    k <- 12
    x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
    y <- rep(0, k) # igual como las coordenadas y de las semillas
    
    for (semilla in 1:k) {
      while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample((j):(r), 1)
        columna <- sample((j):(r), 1)
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
    png("p4s.png")
    par(mar = c(0,0,0,0))
    image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
    graphics.off()
    png("p4c.png")
    par(mar = c(0,0,0,0))
    image(rotate(voronoi), col=rainbow(k+1), xaxt='n', yaxt='n')
    graphics.off()
    
    limite <- n # grietas de que largo minimo queremos graficar
    
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
t.grietas <- t(grietas)
boxplot(t.grietas)

matriz<- data.frame()

for(s in 1:16){
  ds<-(summary(as.numeric(grietas[s,])))
  matriz<-rbind(matriz, ds)
}

a1<-rep(1,4)
a2<-rep(n/4, 4)
a3<-rep(n/2, 4)
a4<-rep(3*n/4, 4)
desde <-c()
desde <-c(desde,a1,a2,a3,a4)

hasta <-c(n/4, n/2, 3*n/4, n)
hasta <-rep(hasta,4)

matriz<-cbind(matriz,desde, hasta)
write.csv(matriz, "msummaryr1.csv")

v<-c()
v1<-c()
for(n in 1:16){
  v<-c(as.numeric(grietas[n,]))
  v1<-c(v1,v)
}

desd<-c(rep(1, (4*atr)),rep(10, (4*atr)),rep(20, (4*atr)),rep(30, (4*atr)))
ad<-c(10,20,30,40)
hast<-c(rep(ad,each=atr),rep(ad,each=atr),rep(ad,each=atr),rep(ad,each=atr))
datos<- matrix(c(desd,hast, v1 )   ,ncol=3, nrow = length(desd))

colnames(datos)<-c("desde fila/columna","hasta fila/columna","largos")
write.csv(datos, "datosr1.csv")
