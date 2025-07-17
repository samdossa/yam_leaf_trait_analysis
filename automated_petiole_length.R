# Charger les bibliothèques nécessaires
library(imager)
library(EBImage)
library(dplyr)
library(readr)
library(tools)

# Fonction principale pour traiter une image
process_image <- function(image_path, pixels_per_mm, output_dir) {
  # Nom de la variété à partir du nom du fichier
  variety_name <- tools::file_path_sans_ext(basename(image_path))
  cat("Processing variety:", variety_name, "\n")
  
  # Charger l'image
  image <- readImage(image_path)
  # display(image, method = "raster", title = paste0(variety_name, " - Original"))
  
  # Convertir en niveaux de gris
  gray_image <- channel(image, "gray")
  #  display(gray_image, method = "raster", title = paste0(variety_name, " - Grayscale"))
  
  # Binariser l'image
  otsu_threshold <- otsu(gray_image)
  binary_image <- gray_image > (otsu_threshold * 1.2)
  #  display(binary_image, method = "raster", title = paste0(variety_name, " - Binary"))
  
  # Rognage
  crop_by_percentage <- function(image, crop_percentage = 0.01) {
    height <- nrow(image)
    width <- ncol(image)
    top <- floor(height * crop_percentage)
    bottom <- floor(height * (1 - crop_percentage))
    left <- floor(width * crop_percentage)
    right <- floor(width * (1 - crop_percentage))
    cropped_image <- image[(top + 1):bottom, (left + 1):right]
    return(cropped_image)
  }
  cropped_binary_image <- crop_by_percentage(binary_image, crop_percentage = 0.01)
  # display(cropped_binary_image, method = "raster", title = paste0(variety_name, " - Cropped"))
  
  # Inverser les pixels
  cropped_binary_image_inverted <- !cropped_binary_image
  #  display(cropped_binary_image_inverted, method = "raster", title = paste0(variety_name, " - Inverted"))
  
  # Remplir les trous
  filled_image <- fillHull(cropped_binary_image_inverted)
  #  display(filled_image, method = "raster", title = paste0(variety_name, " - Filled"))
  
  # Étiqueter les objets
  labeled_image <- bwlabel(filled_image)
  
  # Supprimer les petits objets
  min_area <- 1000
  filtered_image <- rmObjects(
    labeled_image,
    which(computeFeatures.shape(labeled_image)[, "s.area"] < min_area)
  )
  #  display(filtered_image, method = "raster", title = paste0(variety_name, " - Filtered"))
  
  # Sauvegarder l'image filtrée dans le dossier de sortie
  # output_image_path <- file.path(output_dir, paste0(variety_name, "_filtered.tif"))
  # writeImage(filtered_image, output_image_path, quality = 100)
  # cat("Filtered image saved to:", output_image_path, "\n")
  
  # Calculer les caractéristiques des objets
  object_features <- computeFeatures.shape(filtered_image)
  object_features_df <- as.data.frame(object_features)
  
  # Conversion des pixels en unités réelles
  object_features_df$area_mm2 <- object_features_df$s.area / (pixels_per_mm^2)
  object_features_df$circularity <- (4 * pi * object_features_df$s.area) / (object_features_df$s.perimeter^2)
  
  # Filtrer uniquement les pétioles
  filtered_df <- object_features_df[
    object_features_df$area_mm2 <= 1000 &  # Aire ≤ 1000 mm²
      object_features_df$circularity <= 0.4,
  ]
  
  # Calculer la longueur réelle pour chaque pétiole
  calc_real_length <- function(label, image) {
    coords <- which(image == label, arr.ind = TRUE)
    distances <- dist(coords)
    max_dist <- max(distances)
    return(max_dist / pixels_per_mm)
  }
  
  filtered_df$length_real_mm <- sapply(rownames(filtered_df), function(label) {
    calc_real_length(as.numeric(label), filtered_image)
  })
  
  # Préparer les résultats
  results <- data.frame(
    Variety = variety_name,
    ObjectID = seq_len(nrow(filtered_df)),
    Length_mm = filtered_df$length_real_mm
  )
  
  return(results)
}

# Fonction pour parcourir le répertoire et appliquer le pipeline
process_directory <- function(directory, pixels_per_mm, output_file, image_output_dir) {
  # Créer le dossier pour les images traitées s'il n'existe pas
  if (!dir.exists(image_output_dir)) {
    dir.create(image_output_dir, recursive = TRUE)
  }
  
  # Trouver toutes les images .tif dans les sous-dossiers
  image_files <- list.files(directory, pattern = "\\.(tif|jpg)$", full.names = TRUE, recursive = TRUE)
  
  # Initialiser un data frame pour tous les résultats
  all_results <- list()
  
  # Parcourir chaque fichier image
  for (image_path in image_files) {
    cat("\nProcessing:", image_path, "\n")
    results <- tryCatch({
      process_image(image_path, pixels_per_mm, image_output_dir)
    }, error = function(e) {
      cat("Error processing", image_path, ":", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(results)) {
      all_results[[length(all_results) + 1]] <- results
    }
  }
  
  # Combiner tous les résultats en un seul data frame
  final_results <- bind_rows(all_results)
  
  # Sauvegarder les résultats dans un fichier CSV
  write_csv(final_results, output_file)
  cat("Results saved to:", output_file, "\n")
}

# Définir les paramètres et exécuter
directory <- "/home/dossa/Bureau/leaf_panda_2023"  # Répertoire des images
pixels_per_mm <- 23.7  # Ajustez selon votre calibration
output_file <- "petiole_results_2023_lot1-7.csv"  # Fichier de sortie
image_output_dir <- "image_traite"  # Dossier des images traitées

process_directory(directory, pixels_per_mm, output_file, image_output_dir)
