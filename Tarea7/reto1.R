library("lattice")
library("latticeExtra")
g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -4
high <- 4
step <- 0.5
replicas <- 100

replica <- function(t) {
  curr <- runif(2, low, high)
  best <- curr
  respuesta <- curr
  for (tiempo in 1:t) {
    best1 <- numeric()
    best2 <- numeric()
    delta.x <- runif(1, 0, step)
    delta.y <- runif(1, 0, step)
    x1 <- curr[1] - delta.x
    x2 <- curr[1] + delta.x
    y1 <- curr[2] - delta.y
    y2 <- curr[2] + delta.y
    if (x2 > 4) {
      x2 <- x2 - 8
    } else{
      x2 <- x2
    }
    if (y2 > 4) {
      y2 <- y2 - 8
    } else{
      y2 <- y2
    }
    if (x1 < -4) {
      x1 <- x1 + 8
    }else{
      x1 <- x1
    }
    if (y1 < -4) {
      y1 <- y1 + 8
    } else{
      y1 <- y1
    }
    left <- c(x1, curr[2])
    right <- c(x2, curr[2])
    down <- c(curr[1], y1)
    up <- c(curr[1], y2)
    
    
    if (g(x1, curr[2]) < g(x2, curr[2])) {
      best1 <- x2
    } else {
      best1 <- x1
    }
    
    if (g(curr[1], y2) < g(curr[1], y1)) {
      best2 <- y1
    } else {
      best2 <- y2
    }
    
    if (g(best1, curr[2]) < g(curr[1], best2)) {
      curr <- c(curr[1], best2)
    } else {
      curr <- c(best1, curr[2])
    }
    if (g(curr[1], curr[2]) > g(best[1], best[2])) {
      best <- curr
      respuesta <- c(respuesta, curr)
    }
  }
  return(respuesta)
}

t <- data.frame()
for(i in seq(-5, 5, by= 0.1)){
  for(j in seq(-5, 5, by= 0.1)){
    t <- rbind(t, c(i, j, g(i,j)))
  }
}
colnames(t) <- c("x", "y", "z")
graf <- levelplot(z ~ x*y, data = t)
respuesta <- c()
resultado <- data.frame()
for(i in seq(1, (length(resultados) -1), by= 2)){
  respuesta <- c(respuesta, g(resultados[i], resultados[i + 1]))
}
for (p in 2:4) {
  tmax <- 10^p
  resultados <- replica(tmax)
  
  for(i in seq(1, (length(resultados) -1), by= 2)){
    punto <- xyplot(resultados[i] ~ resultados[i+1], pch = 16, col = "black")
    graf1 <- graf + as.layer(punto)
    png(paste(tmax, "p7_", i, ".png", sep=""))
    print(graf1)
    graphics.off()
  }
}

graf1 <- graf 
png("gper1.png")
print(graf1)
graphics.off()
