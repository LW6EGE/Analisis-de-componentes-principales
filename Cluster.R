data ("VADeaths")
library (cluster)

#distancia euclidea 
d_euclidea = daisy (VADeaths, metric = "euclidean") #el parametro metric permite modifica la distancia a utilizar 
d_euclidea

#generacion del cluster (jerarquico)

cluster_euclid = hclust (d_euclidea , method = "complete") # en este caso mediante  es mediante ligamiento completo 

# dendograma (sin bilbioteca ggplot2)

plot (as.dendrogram(cluster_euclid) , horiz=T) # en este caso en posicion horizontal.

# dendograma (con bilioteca ggplot2)

library (ggplot2)
library (ggdendro)

ggdendrogram (cluster_euclid, rotate = T) + scale_y_reverse() 

# generando cluster no jerarquico (k-means)

cluster_k_means = kmeans(d_euclidea ,3 ) #utilizando la matriz de dist euclidea

cluster_k_means$cluster # separacion por cluster 
library(factoextra)

fviz_cluster(cluster_k_means, data = VADeaths) #grafica de cluster 
