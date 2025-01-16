
ibrary(cluster)  
library(dendextend)
#if dont installed use this code:

install.packages("dendextend", dependencies = TRUE)
install.packages("ggdendro", dependencies = TRUE)


dist_matrix <- dist(data)  # Calculate distances between observations
hclust_result <- hclust(dist_matrix, method = "ward.D2")  # Perform hierarchical clustering
plot(hclust_result, main = "Dendrogram of Your Data")

# Install necessary packages if not already installed  
# install.packages("dendextend")  
# install.packages("circlize")  
# Load necessary libraries
library(dendextend)
library(circlize)

# Load necessary libraries
# Load necessary libraries  

library(dendextend)  
library(circlize)  
--------------------------
setwd("F:/Vaezi")

library(dynamicTreeCut)  
library(colorspace)  
library(dendextend) 
# import data  
data <- read.csv("F:/Vaezi/org.csv")  
rownames(data) <- data$Genotype 
d <- dist(data[, -1], method = "euclidean")
hc <- hclust(d, method = "ward.D2") 
plot(hc, hang = -1, labels = rownames(data))  
title("Dendrogram")  


clusters <- cutree(hc, k = 4) 

cat("Cluster assignments:\n")  
members_by_cluster <- split(rownames(data), clusters)  
for (cluster_id in unique(clusters)) {  
  cat("Cluster:", cluster_id, "\n")  
  cat("Members:", paste(members_by_cluster[[as.character(cluster_id)]], collapse = ", "), "\n\n")  
}  
data$cluster <- clusters  

write.csv(data, "data_with_clusters.csv", row.names = FALSE)  

clusters <- clusters[order.dendrogram(as.dendrogram(hc))]  
n_clusters <- length(unique(clusters))  
cols <- rainbow_hcl(n_clusters)  
dend2 <- branches_attr_by_clusters(as.dendrogram(hc), clusters, values = cols)  
dend2 <- set(dend2, "branches_lwd", 3)
plot(dend2, main = "")
