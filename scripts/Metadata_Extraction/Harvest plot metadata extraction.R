# Load packages
library(exifr)

# Define directory path
directory_path <- ("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/Harvest plots")

# Create function
extract.harvestplot.metadata <- function(directory_path, height_offset = 0) {
  # Extract directory name to use as the name for the output csv
  directory_name <- basename(directory_path)
  
  # Import image files
  files <- list.files(directory_path, full.names = TRUE)
  
  # Initialize an empty list to store metadata
  metadata_list <- list()
  file_names <- list()
  
  # Read EXIF metadata for each image
  for (file in files) {
    metadata <- read_exif(file)
    metadata_list[[file]] <- metadata
    file_names[[file]] <- basename(file)
  }
  
  # Extract x, y, z coordinates, GNSS-derived datetime stamp, and RTK data
  xy_coords <- lapply(metadata_list, function(x) {
    data.frame(
      latitude = x$GPSLatitude,
      longitude = x$GPSLongitude,
      altitude = x$GPSAltitude - height_offset,
      datetime_stamp = x$DateTimeOriginal,
      RtkFlag = x$RtkFlag,
      RtkSrcType = x$RtkSrcType,
      RtkStdLon = x$RtkStdLon,
      RtkStdLat = x$RtkStdLat,
      RtkStdHgt = x$RtkStdHgt
    )
  })
  
  # Combine metadata into a single data frame
  combined_metadata <- do.call(rbind, xy_coords)
  
  # Add a column for file names
  combined_metadata$file_name <- unlist(file_names)
  
  # Define CRS of coordinates
  crs <- "WGS84 EPSG::32735"
  
  # Insert a new row specifying CRS
  combined_metadata <- rbind(
    data.frame(
      latitude = "CRS",
      longitude = crs,
      altitude = NA,
      datetime_stamp = NA,
      RtkFlag = NA,
      RtkSrcType = NA,
      RtkStdLon = NA,
      RtkStdLat = NA,
      RtkStdHgt = NA,
      file_name = NA
    ),
    combined_metadata
  )
  
  # Save metadata as CSV file
  write.csv(combined_metadata, file.path(directory_path, paste0(directory_name, "_harvest_plot_data.csv")), row.names = FALSE)
}

# Call the function for each AOI folder
extract.harvestplot.metadata("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/Harvest plots/AOI1", height_offset = 1)
extract.harvestplot.metadata("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/Harvest plots/AOI2", height_offset = 1)
extract.harvestplot.metadata("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/Harvest plots/AOI3", height_offset = 1)
