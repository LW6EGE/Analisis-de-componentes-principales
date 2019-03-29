#analisis de coordenadas

data ("USPersonalExpenditure") #con datos de la database US personal expenditure
usp= USPersonalExpenditure

library (MASS)
library(ggplot2)
#armo la matriz de distancias 

dis= dist(usp, method = "canberra" , diag = T, upper = T) #utilizo distancia canberra 

#analisis de coordenadas


coor_analisis= cmdscale (dis , k=nrow(usp)-1 , eig=T , x.ret=T ) # matriz doblemente centrada y con autovalores


#autovalores

coor_analisis$eig

#grafico coordenaadas dos primeras componentes

points= as.data.frame (coor_analisis$points[,(1:2)])  

grafico= ggplot(points , aes(x= points$V1 , y=points$V2)) +xlab("CP1") + ylab("CP2")+ geom_point( inherit.aes = T) +geom_hline(aes(yintercept = 0)) + geom_vline (aes (xintercept = 0) ) #grafico


grafico 
grafico +  geom_text (label= rownames(points) , hjust=0 , vjust=0) #para verla con texto
