# Description:
# This script calculates various vegetation indices (NDVI, EVI, OSAVI, TDVI, GDVI) from a multispectral orthomosaic
# and extracts mean vegetation index values for each harvest plot. The script starts by loading the necessary libraries
# and data, followed by the calculation of vegetation indices and the extraction of mean values per plot.

# Load libraries
library(terra)
library(readr)
library(sf)
library(exactextractr)

# Load multispectral orthomosaic
multispectral_ortho <- rast("C:/Users/ThinkPad/Downloads/AOI 3 MS/AOI3_MS_orthomosaic.tif")
# Load harvest plot data
harvest_plots <- read_csv("C:/Workspace/Nare_et_al_rangeland_condition_with_UAS/data/Harvest_plots_AOI3.csv")


# Access individual bands
Blue <- multispectral_ortho[[1]]  # Blue band
Green <- multispectral_ortho[[2]]  # Green band
Red <- multispectral_ortho[[3]]  # Red band
Rededge <- multispectral_ortho[[4]]  # Red edge band
NIR <- multispectral_ortho[[5]]  # NIR band

# Vegetation index calculations
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

# Calculate indices
ndvi <- ndviCal(Red, NIR)
evi <- eviCal(Blue, Red, NIR)
osavi <- osaviCal(Red, NIR)
tdvi <- tdviCal(Red, NIR)
gdvi <- gdviCal(Green, NIR)

# Combine indices into a single SpatRaster object
indices_stack <- c(ndvi, evi, osavi, tdvi, gdvi)
names(indices_stack) <- c("NDVI", "EVI", "OSAVI", "TDVI", "GDVI")

# Plot combined indices
plot(indices_stack)

# Create a subdirectory for the vegetation indices if it doesn't exist
if (!dir.exists("Vegetation_Indices")) {
  dir.create("Vegetation_Indices")
}

# Export the combined indices to a single multi-band TIFF file
writeRaster(indices_stack, "Vegetation_Indices/vegetation_indices.tif", filetype="GTiff", overwrite=TRUE)

######Extract mean vegetation indices----


# Convert the data frame to a spatial object using sf
harvest_points <- st_as_sf(harvest_plots, coords = c("Easting(m)", "Northing(m)"), crs = st_crs(multispectral_ortho))

# Create 33 cm (0.33 meter) buffers around each point
buffers <- st_buffer(harvest_points, dist = 0.33)

# Extract mean vegetation index values using exactextractr
mean_indices <- exact_extract(indices_stack, buffers, 'mean')

# Combine the mean indices with the harvest plot data
mean_indices_df <- as.data.frame(mean_indices)
colnames(mean_indices_df) <- c("Mean_NDVI", "Mean_EVI", "Mean_OSAVI", "Mean_TDVI", "Mean_GDVI")
harvest_plots <- cbind(harvest_plots, mean_indices_df)

# Save the updated CSV file with mean vegetation index values
write_csv(harvest_plots, "C:/Workspace/Nare_et_al_rangeland_condition_with_UAS/data/updated_Harvest_plots_AOI1.csv")

# Print the updated data frame to verify
print(head(harvest_plots))
