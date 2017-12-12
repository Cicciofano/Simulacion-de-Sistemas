library("igraph")                           
costos<-data.frame()
aristas<-data.frame()
costos2<-data.frame()
aristas2<-data.frame()
todo<-data.frame()
todo2<-data.frame()
p.dif<-data.frame()
c.dif<-data.frame()
a.dif<-data.frame()
p.data<-data.frame()
c.data<-data.frame()
a.data<-data.frame()
data<-data.frame()
grafica.p<-data.frame()
grafica.c<-data.frame()
grafica.a<-data.frame()

nodos<-30
repetir<-500

v1<-c()
v2<-c()

for(i in 1:round(nodos/2)){
  v1<-c(v1,rep(i,round((nodos)/2)))
  v2<-c(v2,seq((i+1),((i+round(nodos/2)))))
  v1<-c(v1,rep(((i+(round(nodos/2)))),(nodos-round(i+(nodos/2)))))
}
for(i in 1:(round(nodos/2)-1)){
  v2<-c(v2,seq((i+round(nodos/2)+1),nodos))
}
v1<-sort(v1,decreasing = FALSE)
data<-cbind(v1,v2)
t<-dim(data)[1]
weight<-round(runif(t, 10, 50))
data<-cbind(v1,v2,weight)
q<-c(sample(1:t,round((17*t)/20), replace=F))   
q<-sort(q,decreasing = TRUE)
for(i in 1:length(q)){
 data<-data[-(q[i]),]
}
t<-dim(data)[1]
                                                      #hasta aquí crea los nodos y aristas
g <- graph.data.frame(data, directed = FALSE)         # Crea igraph 
V(g)$name                                            # Nombres de los vértices
E(g)$weight                                          # Peso de las aristas
#png("red.png")
tkplot(g, edge.label = paste(E(g)$weight, sep = ""))  # Gráfico dinámico
png(paste("graf.png"), width = 500, height = 450)
plot(g, edge.label = paste(E(g)$weight, sep = ""))   # Gráfico 
graphics.off()
sp <- shortest.paths(g, v = "1", to = as.character(nodos))
#print(sp)
gsp <- get.shortest.paths(g, from = "1", to = as.character(nodos))
#print(gsp[1])
costos<-rbind(costos,sp[1,1])
aristas<-rbind(aristas,length(gsp$vpath[[1]]))
todo<-cbind(costos,aristas)
colnames(todo)<-c("costos", "aristas")

#######################################################

for(r in 1:repetir){
  for(s in 1:19){
    # aquí empieza lo de saturar aristas
    q2<-c()
    q2<-c(sample(1:t, (round(t*(s*5/100))) ,replace=F))      # este valor cambia 
    q2<-sort(q2,decreasing = TRUE)
    data2<-data
    for(i in 1:length(q2)){
      data2[,3][q2[i]]<-Inf
    }
    g2 <- graph.data.frame(data2, directed = FALSE)
    V(g2)$name                                    
    E(g2)$weight                                  
    #tkplot(g2, edge.label = paste(E(g1)$weight, sep = ""))
    #plot(g2, edge.label = paste(E(g1)$weight, sep = "")) 
    sp2 <- shortest.paths(g2, v = "1", to = as.character(nodos))
    #print(s)
    #print(sp2)
    gsp2 <- get.shortest.paths(g2, from = "1", to = as.character(nodos))
    #print(gsp2[1])
    costos2<-rbind(costos2,sp2[1,1])
    aristas2<-rbind(aristas2,length(gsp2$vpath[[1]]))
  }
  todo2<-cbind(costos2,aristas2)
  colnames(todo2)<-c("costos", "aristas")
print(r)
}

##########################################

for(c in 1:r){
  for(d in 1:(length(todo2[,1])/r)){
    p<-round((todo2[(c-1)*19+d,1])*(100/todo[1,1])-100)
    p.dif<-rbind(p.dif,p)
    p<-abs((todo2[(c-1)*19+d,1])-(todo[1,1]))
    c.dif<-rbind(c.dif,p)
    p<-abs((todo2[(c-1)*19+d,2])-(todo[1,2]))
    a.dif<-rbind(a.dif,p)
  }
}

for(g in 1:r){
  v<-c((p.dif[((g-1)*19+1):((g-1)*19+19),]))
  p.data<-rbind(p.data,v)
  v<-c((c.dif[((g-1)*19+1):((g-1)*19+19),]))
  c.data<-rbind(c.data,v)
  v<-c((a.dif[((g-1)*19+1):((g-1)*19+19),]))
  a.data<-rbind(a.data,v)
}


for(s in 1:19){
  a<-0
  b<-0
  ab<-0
  for(rr in 1:r){
    if(p.data[rr,s]!=Inf){
      a<-a+1
    }
    if(c.data[rr,s]!=0){
      b<-b+1
    }
    if(a.data[rr,s]!=0){
      ab<-ab+1
    }
  }
  a<-round(a*100/repetir)
  grafica.p<-rbind(grafica.p,a)
  b<-round(b*100/repetir)
  grafica.c<-rbind(grafica.c,b)
  ab<-round(ab*100/repetir)
  grafica.a<-rbind(grafica.a,ab)
}
vector<-c(seq((1:19)))*5
grafica.p<-cbind(vector,grafica.p)
grafica.c<-cbind(vector,grafica.c)
grafica.a<-cbind(vector,grafica.a)
colnames(grafica.p)<-c("Aristas removidas", "Veces que percola")
colnames(grafica.c)<-c("Aristas removidas", "Incremento en el costo")
colnames(grafica.a)<-c("Aristas removidas", "Incremento en las aristas")

png(paste("p_",nodos, "_",repetir,".png"), width = 500, height = 450)
plot(grafica.p, main = paste(nodos,"nodos,",repetir,"repeticiones"),col="dark orange", type="o", pch=19,ylim=c(0,100), xlim=c(5,95))
abline(h=50, v=91,col="blue")
points(x=91,y=50,col="dark blue", type="o", pch=19)
points(x=88,y=45,col="dark blue", pch="9")
points(x=89.5,y=45,col="dark blue", pch="1")
graphics.off()

png(paste("c_",nodos, "_",repetir,".png"), width = 500, height = 450)
plot(grafica.c, main = paste(nodos,"nodos,",repetir,"repeticiones"),ylim=c(0,100), xlim=c(5,95))
lines(grafica.c, col="dark orange",type = "o")
graphics.off()

png(paste("a_",nodos, "_",repetir,".png"), width = 500, height = 450)
plot(grafica.a, main = paste(nodos,"nodos,",repetir,"repeticiones"),ylim=c(0,100), xlim=c(5,95))
lines(grafica.a, col="dark orange",type = "o")
graphics.off()
