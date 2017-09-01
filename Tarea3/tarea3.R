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

matriz <- data.frame()
matriz2<- data.frame()


for (n in 1:detectCores()){
registerDoParallel(makeCluster((detectCores()+1) - n))
ot <-  numeric()
it <-  numeric()
at <-  numeric()

for (r in 1:replicas) {
  ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
  it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
  at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
  vector <- c(summary(ot), summary(it), summary(at))
  matriz <- rbind(matriz, vector)
  
  }

PromOt<-sum(matriz[,4])/replicas
PromIt<-sum(matriz[,10])/replicas
PromAt<-sum(matriz[,16])/replicas
promedios<-c(PromOt, PromIt, PromAt)
matriz2<-rbind(matriz2,promedios)
colnames(matriz2)<-c("ot", "it", "at")

stopImplicitCluster()

}
total<-length(dimnames(matriz2)[[1]])
for(a in 1:total){
  dimnames(matriz2)[[1]][a]=paste(total-a+1, "núcleos")
}
