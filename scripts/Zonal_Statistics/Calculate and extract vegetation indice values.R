# Description:
# This script calculates various vegetation indices (NDVI, EVI, OSAVI, TDVI, GDVI) from a multispectral orthomosaic
# and extracts mean vegetation index values for each harvest plot. The script starts by loading the necessary libraries
# and data, followed by the calculation of vegetation indices and the extraction of mean values per plot.
# Load libraries
library(terra)
library(tidyverse)
library(sf)
library(exactextractr)

# Load multispectral orthomosaic
multispectral_ortho <- rast("C:/Users/ThinkPad/Downloads/AOI 1 MS/AOI1_orthomosaic_new_nocal.tif")

# Load harvest plot data
harvest_plots <- read_csv("data/Harvest_plots_AOI1.csv")

# Access individual bands
Blue     <- multispectral_ortho[[1]]
Green    <- multispectral_ortho[[2]]
Red      <- multispectral_ortho[[3]]
Rededge  <- multispectral_ortho[[4]]
NIR      <- multispectral_ortho[[5]]

# Index calculation functions
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

tcariCal <- function(Red, Green, NIR) {
  3 * ((NIR - Red) - 0.2 * (NIR - Green) * ((NIR - Red) / (NIR + Red)))
}

mcariCal <- function(Red, Green, NIR) {
  ((NIR - Red) - 0.2 * (NIR - Green)) * (NIR / Red)
}

ndreCal <- function(NIR, Rededge) {
  (NIR - Rededge) / (NIR + Rededge)
}

gciCal <- function(NIR, Green) {
  (NIR / Green) - 1
}

# Calculate indices
ndvi   <- ndviCal(Red, NIR)
evi    <- eviCal(Blue, Red, NIR)
osavi  <- osaviCal(Red, NIR)
tdvi   <- tdviCal(Red, NIR)
gdvi   <- gdviCal(Green, NIR)
tcari  <- tcariCal(Red, Green, NIR)
mcari  <- mcariCal(Red, Green, NIR)
ndre   <- ndreCal(NIR, Rededge)
gci    <- gciCal(NIR, Green)

# Combine indices into a single SpatRaster object
indices_stack <- c(ndvi, evi, osavi, tdvi, gdvi, tcari, mcari, ndre, gci)
names(indices_stack) <- c("NDVI", "EVI", "OSAVI", "TDVI", "GDVI", "TCARI", "MCARI", "NDRE", "GCI")

# Plot combined indices (optional)
plot(indices_stack)

# Create output folder if not exists
if (!dir.exists("Vegetation_Indices")) {
  dir.create("Vegetation_Indices")
}

# Convert data frame to spatial object
harvest_points <- st_as_sf(harvest_plots, coords = c("Easting(m)", "Northing(m)"), crs = st_crs(multispectral_ortho))

# Create 33 cm (0.33 meter) buffer around each point
buffers <- st_buffer(harvest_points, dist = 0.33)

# Stack bands
band_stack <- c(Blue, Green, Red, Rededge, NIR)
names(band_stack) <- c("B1_Blue", "B2_Green", "B3_Red", "B4_RedEdge", "B5_NIR")

# Extract mean reflectance values
mean_bands <- exact_extract(band_stack, buffers, 'mean')
mean_bands_df <- as.data.frame(mean_bands)

# Extract mean index values
mean_indices <- exact_extract(indices_stack, buffers, 'mean')
mean_indices_df <- as.data.frame(mean_indices)
colnames(mean_indices_df) <- paste0("Mean_", names(indices_stack))

# Combine all into final dataframe
harvest_plots <- cbind(harvest_plots, mean_bands_df, mean_indices_df)

# Export to CSV
write_csv(harvest_plots, "data/extracted_reflectance_AOI1.csv")

# Preview first few rows
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
#   ndvi  <- (NIR - Red) / (NIR + Red)
#   evi   <- 2.5 * (NIR - Red) / (NIR + 6 * Red - 7.5 * Blue + 1)
#   osavi <- (NIR - Red) / (NIR + Red + 0.16)
#   tdvi  <- 1.5 * (NIR - Red) / sqrt(NIR^2 + Red + 0.5)
#   gdvi  <- NIR - Green
#   
#   # TCARI calculation
#   tcari <- 3 * ((NIR - Red) - 0.2 * (NIR - Green) * ((NIR - Red) / (NIR + Red)))
#   
#   # Stack vegetation indices
#   indices_stack <- c(ndvi, evi, osavi, tdvi, gdvi, tcari)
#   names(indices_stack) <- c("NDVI", "EVI", "OSAVI", "TDVI", "GDVI", "TCARI")
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
#   colnames(mean_indices_df) <- c("Mean_NDVI", "Mean_EVI", "Mean_OSAVI", "Mean_TDVI", "Mean_GDVI", "Mean_TCARI")
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
#   output_csv = "data/extracted_reflectance_AOI1.csv",
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
