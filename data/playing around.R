####A function to extract GCP Metadata to obtain x,y, and z coordinates of the GCPs
# Load necessary libraries----
source("Packages.R")


## Define the absolute path where images are located
directory_path <- ("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/GCPs")

extract.GCP.metadata <- function(directory_path, height_offset = 0) {
  # Extract directory name to use as the name for the output csv
  directory_name <- basename(directory_path)
  
  # Import image files
  files <- list.files(directory_path, full.names = TRUE)
  
  # Read EXIF metadata for each image
  metadata <- lapply(files, read_exif)
  
  # # Extract x, y, z coordinates and GNSS-derived datetime stamp
  # xy_coords <- lapply(metadata, function(x) data.frame(
  #   latitude = x$GPSLatitude,
  #   longitude = x$GPSLongitude,
  #   altitude = x$GPSAltitude - height_offset,
  #   xy_accuracy = 0.03,       #### This is a temporary hard coded number, still working on the metric
  #   z_accuracy = 0.06,        #### This is a temporary hard coded number, still working on the metric  
  #   datetime_stamp = x$DateTimeOriginal
  # ))
  # Extract x, y, z coordinates and GNSS-derived datetime stamp
  xy_coords <- lapply(metadata, function(x) data.frame(
    latitude = x$GPSLatitude,
    longitude = x$GPSLongitude,
    altitude = x$GPSAltitude - height_offset,
    xy_accuracy = 0.03,       #### This is a temporary hard coded number, still working on the metric
    z_accuracy = 0.06,        #### This is a temporary hard coded number, still working on the metric  
    datetime_stamp = x$DateTimeOriginal
  ))
  # Combine metadata into a single data frame
  combined_metadata <- do.call(rbind, xy_coords)
  
  # Define CRS of coordinates
  crs <- "EPSG:4326"
  # Insert a new row specifying CRS
  combined_metadata <- rbind(
    data.frame(
      latitude = "CRS",
      longitude = crs,
      altitude = NA,
      xy_accuracy = NA,
      z_accuracy = NA,
      datetime_stamp = NA
    ),
    combined_metadata
  )
  
  # Save metadata as CSV file
  write.csv(combined_metadata, file.path(directory_path, paste0(directory_name, "_GCP_data.csv")), row.names = FALSE)
}

# Call the function for each AOI folder

extract.GCP.metadata("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/GCPs/AOI1",height_offset=1)
extract.GCP.metadata("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/GCPs/AOI2",height_offset=1)
extract.GCP.metadata("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/GCPs/AOI3",height_offset=1)

