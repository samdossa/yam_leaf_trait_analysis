setwd("/home/dossa/Bureau/Leaf/LED_bache_74F")  # Change to your working directory
library(imager)
library(EBImage)
library(dplyr)
library(readr)
library(tools)

process_image <- function(image_path, pixels_per_mm, output_dir) {
  cat("Starting process for:", image_path, "\n")
  tryCatch({
    variety_name <- tools::file_path_sans_ext(basename(image_path))
    cat("Variety:", variety_name, "\n")
    
    image <- readImage(image_path)
    cat("Image loaded\n")
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
    gray_image <- channel(image, "gray")
    binary_image <- gray_image > otsu(gray_image)
    cropped_binary_image <- crop_by_percentage(binary_image, crop_percentage = 0.01)
    cropped_binary_image_inverted <- !cropped_binary_image
    filled_image <- fillHull(cropped_binary_image_inverted)
    labeled_image <- bwlabel(filled_image)
    
    min_area <- 2000
    filtered_image <- rmObjects(labeled_image, which(computeFeatures.shape(labeled_image)[, "s.area"] < min_area))
    cat("Objects filtered\n")
    
    #  output_image_path <- file.path(output_dir, paste0(variety_name, "_filtered.tif"))
    #  writeImage(filtered_image, output_image_path, quality = 100)
    # cat("Filtered image saved\n")
    
    object_features <- computeFeatures.shape(filtered_image)
    object_features_df <- as.data.frame(object_features)
    object_features_df$area_mm2 <- object_features_df$s.area / (pixels_per_mm^2)
    filtered_df <- object_features_df[object_features_df$area_mm2 >= 1000, ]
    
    if (nrow(filtered_df) == 0) {
      cat("No valid objects for:", variety_name, "\n")
      return(NULL)
    }
    
    results <- data.frame(
      Variety = variety_name,
      ObjectID = seq_len(nrow(filtered_df)),
      Area_mm2 = filtered_df$area_mm2
    )
    cat("Results processed for:", variety_name, "\n")
    return(results)
  }, error = function(e) {
    cat("Error processing:", image_path, " - ", e$message, "\n")
    return(NULL)
  })
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
    cat("Processing:", image_path, "\n")
    # Appliquer le pipeline
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
directory <- "/home/dossa/Bureau/Leaf/LED_bache_74F"  # Remplacez par le chemin du répertoire
pixels_per_mm <- 23.01  # Ajustez selon votre calibration
output_file <- "leafarea_LED_Bache.csv"
image_output_dir <- "image_traite"
process_directory(directory, pixels_per_mm, output_file, image_output_dir)
