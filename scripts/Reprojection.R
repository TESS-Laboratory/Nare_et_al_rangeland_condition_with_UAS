# Load necessary libraries
library(sf)
library(rgdal)

# Assuming Biomass_summary is your data frame with latitude, longitude, and altitude columns
# Create an sf object
coords <- st_as_sf(Biomass_summary, 
                   coords = c("longitude", "latitude"), 
                   crs = 4326) # CRS: WGS84

# Transform coordinates to EPSG: 32735
coords_32735 <- st_transform(coords, crs = "+init=epsg:32735")

# Add altitude column to the transformed coordinates
coords_32735$altitude <- Biomass_summary$altitude

# Print the transformed coordinates
print(coords_32735)


# Install and load required packages
install.packages("sf")
install.packages("rgdal")
library(sf)
library(rgdal)

# Assuming Biomass_summary is your data frame with northing, easting, and altitude columns
# Create an sf object
coords <- st_as_sf(Biomass_summary, 
                   coords = c("easting", "northing"), 
                   crs = 4326) # CRS: WGS84

# Transform coordinates to EPSG: 32735
coords_32735 <- st_transform(coords, crs = "+init=epsg:32735")

# Add altitude column to the transformed coordinates
coords_32735$altitude <- Biomass_summary$altitude

# Print the transformed coordinates
print(coords_32735)



######################
# Install and load required packages
install.packages("sf")
install.packages("rgdal")
library(sf)
library(rgdal)

# Assuming Biomass_summary is your data frame with latitude, longitude, and altitude columns
# Create an sf object with latitude and longitude
coords <- st_as_sf(Biomass_summary, 
                   coords = c("longitude", "latitude"), 
                   crs = 4326) # CRS: WGS84

# Transform coordinates to EPSG: 32735
coords_32735 <- st_transform(coords, crs = "+init=epsg:32735")

# Extract easting and northing coordinates from the transformed data
easting <- st_coordinates(coords_32735)[, 1]
northing <- st_coordinates(coords_32735)[, 2]

# Combine easting, northing, and altitude into a data frame
coords_df <- data.frame(easting = easting,
                        northing = northing,
                        altitude = Biomass_summary$altitude)
desktop <- "C:/Users/202200875/OneDrive - buan.ac.bw/Desktop"
# Print the transformed coordinates
print(coords_df)

# Define the directory path for saving the CSV file
output_path <- "C:/Users/202200875/OneDrive - buan.ac.bw/Desktop"

# Save the coordinates data frame as a CSV file
write.csv(coords_df, file.path(output_path, "transformed_coordinates.csv"), row.names = FALSE)

# Print a message confirming the file has been saved
cat("CSV file saved successfully at:", file.path(output_path, "transformed_coordinates.csv"), "\n")
