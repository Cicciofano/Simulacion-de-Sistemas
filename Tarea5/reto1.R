cluster <- makeCluster(detectCores() - 1)

library(parallel)

circulo<-function(r){
  xs <- runif(runs,min=-0.5,max=0.5)
  ys <- runif(runs,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  mc.pi <- (sum(in.circle)/runs)*4
  return(mc.pi)
}

vp<-data.frame()
tiempos <- data.frame()

for(p in 1:20){
 
  runs <- 20000*(p^2)
  
  clusterExport(cluster, "runs")
  
  clusterExport(cluster, "runs")
  
  finT <- system.time(aprox<-parSapply(cluster, 1:10, circulo))[3]
  
  vp<-rbind(vp,aprox)
  
  tiempos <- rbind(tiempos, finT) 
  
}

stopCluster(cluster)

plot(xs,ys,pch='.',col=ifelse(in.circle,"blue","grey")
     ,xlab='',ylab='',asp=1,
     main=paste("MC Approximation of Pi =",mc.pi))


for (m in 1:20){
  print(mean(as.numeric(vp[m,])))
  
}
boxplot(t(vp))
abline(h=pi, col="red")

boxplot(tiempos[1,],tiempos[2,],tiempos[3,],tiempos[4,],tiempos[5,],tiempos[6,],tiempos[7,],tiempos[8,],tiempos[9,],tiempos[10,],tiempos[11,],tiempos[12,],tiempos[13,],tiempos[14,],tiempos[15,],tiempos[16,],tiempos[17,],tiempos[18,],tiempos[19,],tiempos[20,])


matriz<-matrix()
matriz<-cbind(matriz,vp[1,],vp[2,],vp[3,],vp[4,],vp[5,],vp[6,],vp[7,],vp[8,],vp[9,],vp[10,],vp[11,],vp[12,],vp[13,],vp[14,],vp[15,],vp[16,],vp[17,],vp[18,],vp[19,],vp[20,])
matriz<-matriz[,2:201]
matriz<-t(matriz)

v1<-c()
for(r in 1:20){
  
  v <- c(rep((10000*(r^2)),10))
  v1 <- c(v1,v)
}
t1<-c()
for(t in 1:20){
  tv<-c(rep((tiempos[t,]),10))
  t1<-c(t1,tv)
}

vap<-c()
for(i in 1:200){
  va<-abs(matriz[i,1]-pi)
  vap<-c(vap,va)
}

matriz<-cbind(matriz,v1,t1,vap)
write.csv(matriz, "matrizt5r1.csv")

png(filename = "dp.png")
boxplot(matriz[1:10,4],matriz[11:20,4],matriz[21:30,4],matriz[31:40,4],matriz[41:50,4],matriz[51:60,4],matriz[61:70,4],matriz[71:80,4],matriz[81:90,4],matriz[91:100,4],
        matriz[101:110,4],matriz[111:120,4],matriz[121:130,4],matriz[131:140,4],matriz[141:150,4],matriz[151:160,4],matriz[161:170,4],matriz[171:180,4],matriz[181:190,4],matriz[191:200,4])
png(filename = "ap.png")
boxplot(matriz[1:10,1],matriz[11:20,1],matriz[21:30,1],matriz[31:40,1],matriz[41:50,1],matriz[51:60,1],matriz[61:70,1],matriz[71:80,1],matriz[81:90,1],matriz[91:100,1],
        matriz[101:110,1],matriz[111:120,1],matriz[121:130,1],matriz[131:140,1],matriz[141:150,1],matriz[151:160,1],matriz[161:170,1],matriz[171:180,1],matriz[181:190,1],matriz[191:200,1])
abline(h=pi,col="red")
png(filename = "tp.png")
boxplot(matriz[1:10,3],matriz[11:20,3],matriz[21:30,3],matriz[31:40,3],matriz[41:50,3],matriz[51:60,3],matriz[61:70,3],matriz[71:80,3],matriz[81:90,3],matriz[91:100,3],
        matriz[101:110,3],matriz[111:120,3],matriz[121:130,3],matriz[131:140,3],matriz[141:150,3],matriz[151:160,3],matriz[161:170,3],matriz[171:180,3],matriz[181:190,3],matriz[191:200,3])

runs <- 100000
#runif samples from a uniform distribution
xs <- runif(runs,min=-0.5,max=0.5)
ys <- runif(runs,min=-0.5,max=0.5)
in.circle <- xs^2 + ys^2 <= 0.5^2
mc.pi <- (sum(in.circle)/runs)*4
png(filename = "tp.png")
plot(xs,ys,pch='.',col=ifelse(in.circle,"blue","grey")
     ,xlab='',ylab='',asp=1)

graphics.off()
#bp1<-c()
#for(b in 1:20){
#  bp<-c(matriz[((10*b)-9):(10*b),])
#  bp1<-c(bp1,bp)
#}

