#### This script is for extracting plot mean canopy heights from the raster CHM
library(sf)
library(terra)
library(exactextractr)
library(tidyverse)
# Read the raster data
# r <- rast("C:/workspace/Nare_dev/plots/AOI2.tif")
r <- rast("C:/Users/ThinkPad/Downloads/AOI3_Canopy_Height_Model.tif")

# Read the CSV file with harvest plot points
harvest_plots <- read_csv("data/Harvest_plots_AOI3.csv")
head(harvest_plots)
# Convert the data frame to a spatial object using sf
harvest_points <- st_as_sf(harvest_plots, coords = c("Easting(m)", "Northing(m)"), crs = st_crs(r))

# Create 33 cm (0.33 meter) buffers around each point
buffers <- st_buffer(harvest_points, dist = 0.33)      

# Add plot names to the buffers
buffers$Plots <- harvest_plots$Plot

# Plot the raster
plot(r)

# Plot the harvest points on top of the raster
plot(harvest_points, add = TRUE, pch = 20, col = "red")

# Plot the buffers around harvest points
plot(buffers, add = TRUE, border = "blue", lwd = 2)

# Extract mean canopy heights using exactextractr
mean_canopy_heights <- exact_extract(r, buffers, 'mean')

# Add the mean canopy heights to the original CSV file
harvest_plots$Mean_Canopy_Height <- mean_canopy_heights

# Save the updated CSV file
write.csv(harvest_plots, "data/extracted_mean_ch_aoi3.csv", row.names = FALSE)

# Print the updated data frame to verify
print(head(harvest_plots))
