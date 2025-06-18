# Description:
# This script calculates various vegetation indices (NDVI, EVI, OSAVI, TDVI, GDVI) from a multispectral orthomosaic
# and extracts mean vegetation index values for each harvest plot. The script starts by loading the necessary libraries
# and data, followed by the calculation of vegetation indices and the extraction of mean values per plot.

# Load libraries
library(terra)
library(tidyverse)
library(sf)
library(exactextractr)

# # Load multispectral orthomosaic
multispectral_ortho <- rast("C:/Users/ThinkPad/Downloads/AOI 3 MS/AOI3_MS_orthomosaic.tif")
# # Load harvest plot data
harvest_plots <- read_csv("data/Harvest_plots_AOI3.csv")


# # Access individual bands
Blue <- multispectral_ortho[[1]]  # Blue band
Green <- multispectral_ortho[[2]]  # Green band
Red <- multispectral_ortho[[3]]  # Red band
Rededge <- multispectral_ortho[[4]]  # Red edge band
NIR <- multispectral_ortho[[5]]  # NIR band

# # Vegetation index calculations
ndviCal <- function(Red, NIR) {
  (NIR - Red) / (NIR + Red)
}

eviCal <- function(Blue, Red, NIR) {
  2.5 * (NIR - Red) / (NIR + 6 * Red - 7.5 * Blue + 1)
}

osaviCal <- function(Red, NIR) {
  (NIR - Red) / (NIR + Red + 0.16)
}

tdviCal <- function(Red, NIR) {
  1.5 * (NIR - Red) / sqrt(NIR^2 + Red + 0.5)
}

gdviCal <- function(Green, NIR) {
  NIR - Green
}

# # Calculate indices
ndvi <- ndviCal(Red, NIR)
evi <- eviCal(Blue, Red, NIR)
osavi <- osaviCal(Red, NIR)
tdvi <- tdviCal(Red, NIR)
gdvi <- gdviCal(Green, NIR)

# # Combine indices into a single SpatRaster object
indices_stack <- c(ndvi, evi, osavi, tdvi, gdvi)
names(indices_stack) <- c("NDVI", "EVI", "OSAVI", "TDVI", "GDVI")

# # Plot combined indices
plot(indices_stack)

# # Create a subdirectory for the vegetation indices if it doesn't exist
if (!dir.exists("Vegetation_Indices")) {
  dir.create("Vegetation_Indices")
}

# # Export the combined indices to a single multi-band TIFF file
# # writeRaster(indices_stack, "Vegetation_Indices/vegetation_indices.tif", filetype="GTiff", overwrite=TRUE)
#
# ######Extract mean vegetation indices----


# # Convert the data frame to a spatial object using sf
harvest_points <- st_as_sf(harvest_plots, coords = c("Easting(m)", "Northing(m)"), crs = st_crs(multispectral_ortho))

# # Create 33 cm (0.33 meter) buffers around each point
buffers <- st_buffer(harvest_points, dist = 0.33)

# ##### Extract mean spectral bands ----
band_stack <- c(Blue, Green, Red, Rededge, NIR)
names(band_stack) <- c("B1_Blue", "B2_Green", "B3_Red", "B4_RedEdge", "B5_NIR")
mean_bands <- exact_extract(band_stack, buffers, 'mean')
mean_bands_df <- as.data.frame(mean_bands)

# ##### Extract mean vegetation indices ----
mean_indices <- exact_extract(indices_stack, buffers, 'mean')
mean_indices_df <- as.data.frame(mean_indices)
colnames(mean_indices_df) <- c("Mean_NDVI", "Mean_EVI", "Mean_OSAVI", "Mean_TDVI", "Mean_GDVI")

# # Combine everything into the final harvest_plots data
harvest_plots <- cbind(harvest_plots, mean_bands_df, mean_indices_df)

# # Save updated dataset to CSV
write_csv(harvest_plots, "data/extracted_reflectance_AOI3.csv")

## Preview first rows
print(head(harvest_plots))

#### DEFINE FUNCTION
################################################################################
# extract_spectral_indices <- function(ms_path, harvest_csv, output_csv, buffer_radius = 0.33) {
#   # Load libraries
#   library(terra)
#   library(tidyverse)
#   library(sf)
#   library(exactextractr)
#   
#   # Load multispectral orthomosaic and harvest plot data
#   multispectral_ortho <- rast(ms_path)
#   harvest_plots <- read_csv(harvest_csv)
#   
#   # Extract bands
#   Blue     <- multispectral_ortho[[1]]
#   Green    <- multispectral_ortho[[2]]
#   Red      <- multispectral_ortho[[3]]
#   Rededge  <- multispectral_ortho[[4]]
#   NIR      <- multispectral_ortho[[5]]
#   
#   # Vegetation index calculations
#   ndvi <- (NIR - Red) / (NIR + Red)
#   evi  <- 2.5 * (NIR - Red) / (NIR + 6 * Red - 7.5 * Blue + 1)
#   osavi <- (NIR - Red) / (NIR + Red + 0.16)
#   tdvi <- 1.5 * (NIR - Red) / sqrt(NIR^2 + Red + 0.5)
#   gdvi <- NIR - Green
#   
#   # Stack vegetation indices
#   indices_stack <- c(ndvi, evi, osavi, tdvi, gdvi)
#   names(indices_stack) <- c("NDVI", "EVI", "OSAVI", "TDVI", "GDVI")
#   
#   # Convert harvest plots to spatial object
#   harvest_points <- st_as_sf(harvest_plots, coords = c("Easting(m)", "Northing(m)"), crs = st_crs(multispectral_ortho))
#   buffers <- st_buffer(harvest_points, dist = buffer_radius)
#   
#   # Stack bands
#   band_stack <- c(Blue, Green, Red, Rededge, NIR)
#   names(band_stack) <- c("B1_Blue", "B2_Green", "B3_Red", "B4_RedEdge", "B5_NIR")
#   
#   # Extract mean reflectance and vegetation index values
#   mean_bands_df   <- as.data.frame(exact_extract(band_stack, buffers, 'mean'))
#   mean_indices_df <- as.data.frame(exact_extract(indices_stack, buffers, 'mean'))
#   colnames(mean_indices_df) <- c("Mean_NDVI", "Mean_EVI", "Mean_OSAVI", "Mean_TDVI", "Mean_GDVI")
#   
#   # Combine and export
#   harvest_final <- cbind(harvest_plots, mean_bands_df, mean_indices_df)
#   write_csv(harvest_final, output_csv)
#   
#   message("✅ Extraction complete: ", output_csv)
# }
# 
# 
# 
# ##### Call function for each AOI
# extract_spectral_indices(
#   ms_path = "C:/Users/ThinkPad/Downloads/AOI 1 MS/AOI1_orthomosaic_new_nocal.tif",
#   harvest_csv = "data/Harvest_plots_AOI1.csv",
#   output_csv = "data/extracted_reflectance_AOI2.csv",
#   buffer_radius = 0.33
# )
# 
# 
# extract_spectral_indices(
#   ms_path = "C:/Users/ThinkPad/Downloads/AOI 2 MS/AOI2_Orthomosaic_ms.tif",
#   harvest_csv = "data/Harvest_plots_AOI2.csv",
#   output_csv = "data/extracted_reflectance_AOI2.csv",
#   buffer_radius = 0.33
# )
# 
# 
# extract_spectral_indices(
#   ms_path = "C:/Users/ThinkPad/Downloads/AOI 3 MS/AOI3_MS_orthomosaic.tif",
#   harvest_csv = "data/Harvest_plots_AOI3.csv",
#   output_csv = "data/extracted_reflectance_AOI3.csv",
#   buffer_radius = 0.33
# )
