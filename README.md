# Install & load packages
if (!require(jpeg)) install.packages("jpeg", dependencies = TRUE)
if (!require(png)) install.packages("png", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)

library(jpeg)
library(png)
library(ggplot2)

# ---- Helper: load image and flatten to vector ----
load_image_vector <- function(path, size = 50) {
  img <- tryCatch({
    if (grepl("\\.jpg$|\\.jpeg$", path)) readJPEG(path) else readPNG(path)
  }, error = function(e) stop("Image not readable: ", path))
  
  # resize (rough by sampling) & flatten
  img_gray <- img[, , 1]  # first channel (assume grayscale or take R channel)
  img_small <- as.vector(scale(img_gray))[1:(size*size)]  # take first pixels
  return(img_small)
}

# ---- Load dataset of MRI images ----
image_files <- list.files("mri_dataset/", pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)

data_matrix <- do.call(rbind, lapply(image_files, load_image_vector))

# ---- Run clustering (2 clusters: healthy / diseased) ----
set.seed(123)
kmeans_model <- kmeans(data_matrix, centers = 2)

# Assign cluster labels
labels <- ifelse(kmeans_model$cluster == 1, "Healthy", "Diseased")
results <- data.frame(File = basename(image_files), Cluster = labels)

print(results)

# ---- Classify a new MRI image ----
new_image <- "new_mri.png"   # path to new MRI# Install & load packages
packages <- c("jpeg", "png", "ggplot2")
lapply(packages, function(p) if (!require(p, character.only = TRUE)) install.packages(p))

library(jpeg)
library(png)
library(ggplot2)

# ---- Helper: load image as feature vector ----
load_image_vector <- function(path, size = 50) {
  img <- if (grepl("\\.jpg$|\\.jpeg$", path)) readJPEG(path) else readPNG(path)
  gray <- img[, , 1]                  # take first channel (grayscale assumption)
  vec <- as.vector(gray)[1:(size*size)]  # sample first pixels
  return(vec)
}

# ---- Load dataset of MRI images ----
image_files <- list.files("mri_dataset/", pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)
data_matrix <- do.call(rbind, lapply(image_files, load_image_vector))

# ---- Run clustering (2 groups: Healthy / Diseased) ----
set.seed(123)
km <- kmeans(data_matrix, centers = 2)
labels <- ifelse(km$cluster == 1, "Healthy", "Diseased")

# ---- Visualization with PCA ----
pca <- prcomp(data_matrix)
df

# sim-cluster-
just testing
