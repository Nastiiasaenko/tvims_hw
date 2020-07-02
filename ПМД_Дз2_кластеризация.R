df <- matrix(c(4,2,7,8,1,6,1,4,1,2,0,2,7,4,5,3,2,6),ncol = 3)
colnames(df) <- c('age','kids','salary')
rownames(df) <- letters[1:6]
library(cluster)
plot(hclust(dist(df, method = 'maximum'), method = 'complete'),hang = -1)

View(df)
#  a
library(cluster)
k <- kmeans(df,2)
clusters <- k$cluster
clusters

# manual_kmeans ------------------------------------------------------------------
#function for distances to new means 
centerdist <- function(newset){
d <- dist(newset)
j <- dist_subset(d,c(rownames(newset)[7],'a','b','c','d','e','f'))
j_dist<- j[1:6]
i <- dist_subset(d,c(rownames(newset)[8],'a','b','c','d','e','f'))
i_dist <- i[1:6]
c <- cbind(j_dist,i_dist)
rownames(c) <- letters[1:6]
h <- numeric(length = 6)

  
  for (i in 1:6){
    if (c[i,1]-c[i,2] > 0){ h[i] <- 'i'
    } else {h[i] <- 'j'}
    
  }
  g <- cbind(c,h)
  g <- as.data.frame(g)
  print(g)
}

# k - means algorithm 
c1 <- c(7,1,5)
c2 <- c(1,0,2)
set1 <- rbind(df,c1,c2)
centerdist(set1)
#newcenters
c1 <- (df['b',]+df['e',])/2
c2 <- (df['a',]+df['c',]+df['d',]+df['f',])/4
set2 <- rbind(df,c1,c2)
centerdist(set2)
#newcenters 2
c1 <- (df['a',]+df['c',]+df['d',]+df['f',])/4
c2 <- (df['b',]+df['e',])/2
set3 <- rbind(df,c1,c2)
centerdist(set3)

library(cluster)
k <- kmeans(df,2)
k$cluster

# Agglomerative -----------------------------------------------------------
library(cluster)
d <- dist(df, method = 'maximum')
plot(hclust(d, method = 'complete'), hang = -1 )
