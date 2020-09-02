# Akanseoluwa Stephen Adegoke
# Master's Student, Data and Computational Science
# University College Dublin
# © 2020. Akanseoluwa Adegoke.


# Task # 
# - Complete a cluster analysis of the Spotify audio features data 

# Experience
# - Unsupervised Learning

# Performance Measure
# - Internal Validation : Calinski Harabasz Index
#                       : Silhouete
# - External Validation : - Rand Index


# Reading Spotify Songs data
spotify = read.csv("data_spotify_songs.csv")

#Viewing first parts of the dataset
head(spotify)

# Removing categorical data from our data
spotify_new = spotify[, -1 : -3]

# Scaling our dataset
spotify_new = scale(spotify_new)

# Visualising our data 
pairs(spotify_new, gap = 0, pch = 16, col=adjustcolor(1,0.4))

#K-means Clustering wth 3 clusters 
fitkm = kmeans(spotify_new, centers = 3, nstart = 20)
fitkm

# split the plot window in 2 screens 
par( mfrow = c(1,2) ) 

#Setting different shapes for the plot
symb <- c(17, 18, 19)

#Setting different colours for the clusters
col = c("red", "green", "deepskyblue3")


#Clustering according to Music Genre
pairs(spotify, gap = 0, pch = symb[spotify$genre], 
      col= adjustcolor(col[spotify$genre], 0.4), main = "Clustering by genre")


# Plot of Clustering Solution with Three(3) clusters
pairs(spotify_new, gap = 0, pch = symb[fitkm$cluster], 
      col=adjustcolor(col[fitkm$cluster], 0.4), main = "Clustering of three components")


######## CLUSTERING VALIDATION  ##########

## INTERNAL VALIDATION #
# Calinski-Harabasz Index #

#Setting our maximum value of K
K = 10

#Initialise Empty Vector 
wss = bss = rep(NA, K)


for ( k in 1:K ) {
  # run kmeans for each value of k
  fit <- kmeans(spotify_new, centers = k, nstart = 50)
  wss[k] <- fit$tot.withinss 
  # store total within sum of squares 
  bss[k] <- fit$betweenss
 
}

# Computing Calinski-Harabasz Index
N <- nrow(spotify_new)
ch <- ( bss/(1:K - 1) ) / ( wss/(N - 1:K) )
ch[1] <- 0

# Plot of the CH Index
plot(1:K, ch, type = "b", ylab = "CH", xlab = "K")


fit2 <- kmeans(spotify_new, centers = 2, nstart = 50) 

fit3 <- kmeans(spotify_new, centers = 3, nstart = 50)

symb <- c(15, 16, 17)
col = c("red", "green", "deepskyblue3")

# split the plot window in 2 screens
par( mfrow = c(1,2) ) 

# plot corresponding to 2 clusters 
pairs(spotify_new, gap = 0, pch = symb[fit2$cluster],
      col = adjustcolor(col[1:2], 0.4), main = "Clustering Result - K = 2")

# plot corresponding to 3 clusters 
pairs(spotify_new, gap = 0, pch = symb[fit3$cluster],
      col = adjustcolor(col[fit3$cluster], 0.4), main = "Clustering Result - K = 3")


# SILHOUETTE #
# Calling cluster library for Silhouette
library(cluster)

# Calculating the dissimilarity matrix
d = dist(spotify_new, method = "euclidean") ^ 2

# Silhouette for a two(2) cluster solution
sil2 = silhouette(fit2$cluster, d)
# Silhouette for a three(3) cluster solution 
sil3 = silhouette(fit3$cluster, d)

# Producing two Silhouette Plots
col = c("red", "green", "deepskyblue3")

par(mfrow = c(1,2) )

#Plot of Silhouette for a 2 cluster solution
plot(sil2, col = adjustcolor(col[1:2], 0.3), main ="Spotify with K = 2")

#Plot of Silhouette for a 3 cluster solution
plot(sil3, col = adjustcolor(col, 0.3), main ="Spotify with K = 3")


# EXTERNAL VALIDATION #

#Using Rand Index and External Rand Index

#Calling library e1071 for calculating Rand and Adjusted Rand Index
library(e1071)

tab = table(fit2$cluster, spotify[,1])
tab

tab2 = table(fit3$cluster, spotify[,1])
tab2


classAgreement(tab)
classAgreement(tab2)


### REPORT ###
### MOTIVATION and COMMENTS ### 


# From the CH index, there is an elbow at k =2 and from 2 downwards there is a downward movement on the graph
# till it  fades off. From this , I will suggest a 
# two cluster solution. 

# From the Silhouette , the two cluster solution has a greater Silhouette number than that
# of the 3 cluster solution. So I will suggest a two cluster solution
# from the Silhouette Index.
#
# From the Visulaisation of the plots of the two and three clusters, 
# the plot of the two clusters seems to be more clear and pass more
#information than that of the three clusters. I will also suggest a two
# cluster solution from the visualisations.
#
# From the Rand Index, Most of our data in cluster 1 belong to the acoustic genre with 
# little representation of the pop and rock genre while 
# most of our data in Cluster two belong to the pop genre, followed by the rock genre and
# little representation of the acoustic genre.

# The Rand Index gives a value of 0.73 which is quite large
# and the adjusted Rand Index gives a value of 0.47 which is okay and
# still suggests a fairly good fit.
#
