# Install necessary packages if you haven't already
if(!require(ggbiplot)){install.packages("ggbiplot")}
if(!require(devtools)){install.packages("devtools")}
# Install ggbiplot from Github (current version on CRAN has a bug)
devtools::install_github("vqv/ggbiplot")

library(ggbiplot)
library(dplyr)

# Read the data, making sure 'gen' is treated as a character/factor, not numeric
data <- read.csv("F:/Vaezi/y1.csv", stringsAsFactors = TRUE) # or stringsAsFactors = FALSE, then convert gen to factor explicitly

# Convert 'gen' to factor if it was read as character
if (!is.factor(data$gen)) {
  data$gen <- as.factor(data$gen)
}

# Remove the 'gen' column before PCA. PCA works with numerical data.
data_numeric <- select(data, -gen)

# Perform PCA.  Scale the data for meaningful results since variables have different scales.
data.pca <- prcomp(data_numeric, center = TRUE, scale. = TRUE)

# Print summary statistics
summary(data.pca)

# Create a biplot
print(ggbiplot(data.pca, groups = data$gen, ellipse = TRUE, circle = TRUE))

# Access the principal components (PCs)
pcs <- data.pca$x

# Add the PCs back to the original data if needed
data_with_pcs <- cbind(data, pcs)

# Example: Accessing PC1
pc1 <- pcs[, 1]


# Explore variance explained by each PC
explained_variance <- data.pca$sdev^2 / sum(data.pca$sdev^2)
print(explained_variance)

#Cumulative variance
cumulative_variance <- cumsum(explained_variance)
print(cumulative_variance)



# Scree plot to visualize variance explained
screeplot(data.pca, type = "lines")

# Look at loadings (contributions of variables to PCs)
loadings <- data.pca$rotation
print(loadings)

# Biplot with variable labels (loadings) displayed
ggbiplot(data.pca, labels=rownames(data), var.axes = TRUE)

# Customize ggbiplot further if needed (e.g., change colors, labels, etc.)
# See ?ggbiplot for options