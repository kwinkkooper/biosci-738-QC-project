


# Load necessary libraries
library(tidyverse)
library(stats)

# Create a data frame with the known subjects' protein data
protein_data <- data.frame(
  CG1 = c(13, 16, 196, 445, 12, 2059, 112, 36, 22, 6, 292),
  CG2 = c(10, 86, 261, 766, 27, 5640, 304, 73, 54, 23, 246),
  CG3 = c(10, 562, 608, 2124, 192, 1059, 1710, 347, 214, 47, 311),
  CG4 = c(17, 18, 93, 331, 7, 3745, 50, 5, 10, 5, 248),
  CG5 = c(30, 80, 336, 725, 39, 2536, 454, 115, 42, 6, 447),
  CG6 = c(3, 166, 200, 942, 50, 5143, 506, 69, 79, 11, 139),
  CG7 = c(6, 176, 383, 1687, 92, 914, 472, 62, 95, 6, 252),
  CG8 = c(9, 18, 212, 711, 13, 578, 110, 32, 27, 8, 171),
  CG9 = c(24, 10, 135, 479, 3, 3820, 124, 45, 10, 7, 404),
  CG10 = c(27, 144, 203, 1001, 48, 4876, 392, 87, 76, 33, 438),
  CG11 = c(3, 72, 224, 1141, 89, 3379, 642, 91, 82, 4, 118)
)

rownames(protein_data) <- c(
  "A0A1B0GU03", "A2ML1", "ACTB", "ALB", "ALDOA", "AMY1A", 
  "ANXA1", "ANXA2", "APOA1", "APOH", "AZGP1"
)

# Create a data frame for the Mder sample
Mder <- data.frame(
  unknown = c(15, 82, 600, 100, 340, 210, 380, 215, 140, 210, 230)
)
rownames(Mder) <- c(
  "A0A1B0GU03", "A2ML1", "ACTB", "ALB", "ALDOA", "AMY1A", 
  "ANXA1", "ANXA2", "APOA1", "APOH", "AZGP1"
)

# Perform PCA on the known subjects
pca <- prcomp(t(protein_data), scale. = TRUE)

# Use only the first two principal components for the known subjects
known_pca_2d <- pca$x[, 1:2]

# Project the unknown sample onto the PCA space using only the first two principal components
Mder_pca_2d <- predict(pca, newdata = t(Mder))[, 1:2]

# Calculate the Euclidean distances between the unknown sample and each known subject using the first two principal components
distances <- apply(known_pca_2d, 1, function(x) sqrt(sum((x - Mder_pca_2d)^2)))

# Find the closest known subject
closest_subject <- names(which.min(distances))

print(closest_subject)
