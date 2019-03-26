
#ACP para la database iris
library (ggplot2)
library (ggforce)
library (ggbiplot)
 

data ("iris") # database incluida en R
iris$Species = NULL #eliminando la columna de especies.

cor (iris) #matriz de correlacion 

ACP= prcomp(iris, scale. = T) #si se pone scale.=F se estaria utilizando matriz de covarianza

ACP$rotation #autovectores

ACP$x #coordenadas del g-plot
g_ACP = as.data.frame ( ACP$x)

ACP$sdev ^2 # autovalores  

cor_ACP= as.data.frame ( ACP$rotation %*% (diag (ACP$sdev))  )#correlacion entre variables y componentes. (utilizando matriz de correlacion)
 #si estaria utilizando covarianzas cor_acp= diag(1/sqrt(diag(cov(iris)))) %*% ACP$rotation %*% diag(ACP$sdev)



#Representacion grafica


grafico_circulo= ggplot (cor_ACP, aes( x = cor_ACP[,1] , y= cor_ACP[,2] , label=colnames(iris)) ) + xlab("CP1")+ ylab("CP2") + xlim(c(-1, 1)) + ylim(c(-1, 1))  + geom_segment ( aes(x=0, xend=cor_ACP[,1], y=0, yend= cor_ACP[,2]), arrow= arrow(length = unit(0.3, "cm")))   

grafico_circulo =   grafico_circulo+ geom_text(size=4, hjust=1, vjust=1, check_overlap = T ) + geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) + geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), inherit.aes = FALSE)

grafico_circulo #grafica circulo de radio uno y radio 0.5.

g_plot= ggplot( g_ACP, aes (x= g_ACP$PC1  , y= g_ACP$PC2 )) + xlab ("CP1") + ylab ("CP2") +geom_hline(aes(yintercept = 0)) + geom_vline (aes (xintercept = 0) )

g_plot +  geom_point() #grafica g plot 


biplot= ggbiplot (ACP) 
biplot =biplot + theme (legend.direction = 'horizontal', legend.box = "horizontal", legend.position = 'top') + geom_hline(aes(yintercept = 0)) + geom_vline (aes (xintercept = 0) )                                                          
 biplot #grafica biplot
 
