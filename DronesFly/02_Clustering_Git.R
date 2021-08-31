#https://en.proft.me/2017/02/3/density-based-clustering-r/
#https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
#https://www.datanovia.com/en/lessons/k-medoids-in-r-algorithm-and-practical-examples/

# Libraries loading -------------------------------------------------------
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)
library(NbClust)



# Elbow Method ------------------------------------------------------------

#Elbow method
fviz_nbclust(Angles_Mav_xyz, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


# Silhouette method
fviz_nbclust(Angles_Mav_xyz, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#GAP method
set.seed(123)
fviz_nbclust(Angles_Mav_xyz, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#
NbClust(data = Angles_Mav_xyz, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 10, method = "kmeans")

#It suggests having 4 clusters

# KMeans Clustering  basic-------------------------------------------------------

k_vect <- Angles_Mav_xyz %>%
  select( y, x, z) %>% 
  kmeans(., 4, nstart = 25)


Angles_Mav_xyz <-  bind_cols(Angles_Mav_xyz,k_cluster_basic = k_vect$cluster) 

plot_ly(Angles_Mav_xyz, y= ~y,  x= ~x, z = ~z, color =~k_cluster_basic  ) #colors= c('#BF382A', '#0C4B8E', '#ff9900')


# Kmeans scaled -----------------------------------------------------------

scale_angles <- scale(Angles_Mav_xyz[, 2:4])
set.seed(123)
km_ang <- kmeans(scale_angles, 4, nstart = 25)
print(km_ang$withinss)

Angles_Mav_xyz <-  bind_cols(Angles_Mav_xyz,k_cluster_scaled = km_ang$cluster) 

plot_ly(Angles_Mav_xyz, y= ~y,  x= ~x, z = ~z, color =~k_cluster_scaled  ) #colors= c('#BF382A', '#0C4B8E', '#ff9900')





# DBSCAN - Density Based Spacial Clustering of Aplications with Noise -------------------------------
#https://en.proft.me/2017/02/3/density-based-clustering-r/


glimpse(Angles_Mav_xyz)
#DBSCAN input is a matrix. Optional scaled
angles_matrix <- as.matrix(Angles_Mav_xyz[,2:4]);View(angles_matrix)
angles_matrix_sca <- scale(angles_matrix)

#this plot helps us to define the value of epsilon
kNNdistplot(angles_matrix_sca, k=25) #tells us that the ideal values of "e" is between 1 and 2
abline(h=2, col="red")

set.seed(1234)
db <-  dbscan::dbscan(angles_matrix_sca, eps = 1.6, minPts = 10); print(db)
Angles_Mav_xyz <-  bind_cols(Angles_Mav_xyz, k_cluster_dbscan = db$cluster) 
plot_ly(Angles_Mav_xyz, y= ~y,  x= ~x, z = ~z, color=~k_cluster_dbscan ) #, colors= c('#BF382A', '#0C4B8E', '#ff9900'))

hullplot(angles_matrix_sca, db$cluster)

#performance metrics

cs <-  fpc::cluster.stats(dist(angles_matrix_sca), db$cluster) ; print(cs)
cs$within.cluster.ss
cs$avg.silwidth


# Kmeans PAM   ------------------------------------------------------------
#https://www.datanovia.com/en/lessons/k-medoids-in-r-algorithm-and-practical-examples/

pam.res <- pam(angles_matrix_sca, 3); print(pam.res)

pam.res$clustering
pam.res$diss

Angles_Mav_xyz <-  bind_cols(Angles_Mav_xyz, k_cluster_pam = pam.res$clustering) 
plot_ly(Angles_Mav_xyz, y= ~y,  x= ~x, z = ~z, color=~k_cluster_pam ) #, colors= c('#BF382A', '#0C4B8E', '#ff9900'))


pam <-  fpc::cluster.stats(dist(angles_matrix_sca), pam.res$clustering) 
print(pam)


# Hierarchical Clustering -------------------------------------------------

scaled_angles <- Angles_Mav_xyz %>% 
  select( x, y, z) %>% 
  scale()


h_clust <- hclust(d = dist(scaled_angles), method = "average")

plot(h_clust)
fviz_dend(h_clust, cex = .5)

clus_coph <- cophenetic(h_clust)

cor(dist(scaled_angles), clus_coph)

cluster_cut <- cutree(h_clust, 6)
cluster_cut
table(cluster_cut)

fviz_dend(h_clust, k = 6, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#011f4b", "#03396c", "#005b96","#6497b1","#b3cde0", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

Angles_Mav_xyz <-  bind_cols(Angles_Mav_xyz, h_cluster = cluster_cut) 

plot_ly(Angles_Mav_xyz, y= ~y,  x= ~x, z = ~z, color =~cluster_cut)#, colors= c('#BF382A', '#0C4B8E', '#ff9900'))





# Comparing clustering methods --------------------------------------------

summ <- tibble( 
  Approach = c("K-Means Basic", "K-Means Scaled", "DBSCAN", "PAM"),
  Sum_of_Sq = c(sum(k_vect$withinss), sum(km_ang$withinss), cs$within.cluster.ss,pam_out$within.cluster.ss )
)

summ %>% arrange((Approach))
str(summ)
summ$Sum_of_Sq

