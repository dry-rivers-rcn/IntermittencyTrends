## K-means clustering of time series

## This script is an attempt to cluster time series
## Following Savoy et al. 2019 L&O, there are two potential approaches:
## 1) Dynamic time warping and hierarchical partitional clustering
## 2) K-means partitional clustering on Euclidean distance matrix among time series

## I am going to start by attempting #2
## This approach has also been detailed in Olden et al. 2012 Ecohydrology

## Prior to clustering, each variable needs to be z-normalized
## Next, compute a Euclidean distance matrix
## Then, define the number of clusters
## Use NbClust to compute cluster validity indices (CVIs)







