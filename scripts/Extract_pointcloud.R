# ===============================================
# Extract selected plants from multiple AOIs
# 
# ===============================================

library(lidR)
library(tidyverse)
library(sf)

# ---------------------------
# Function to process an AOI
# ---------------------------
process_aoi <- function(las_path, harvest_csv, plants_to_export, output_folder) {
  
  # 1. Load point cloud
  aoi <- readLAS(las_path)
  
  if(is.empty(aoi)) stop("LAS file is empty!")
  
  # 2. Check CRS
  crs_aoi <- st_crs(aoi)
  
  # 3. Load harvest plot centres
  harvest <- read_csv(harvest_csv)
  
  # 4. Convert to spatial points
  plots_sf <- st_as_sf(
    harvest,
    coords = c("Easting(m)", "Northing(m)"),
    crs = crs_aoi
  )
  
  # 5. Create circular harvest plots (0.33 m radius)
  plots_buffer <- st_buffer(plots_sf, dist = 0.33)
  
  # 6. Clip individual plants
  plant_clouds <- clip_roi(aoi, plots_buffer)
  
  # 7. Name each clipped cloud by Plot_ID
  names(plant_clouds) <- plots_buffer$Plot_ID
  
  # 8. Export only the selected plants
  for(p in plants_to_export) {
    if(p %in% names(plant_clouds)) {
      writeLAS(plant_clouds[[p]], file.path(output_folder, paste0(p, ".laz")))
      message("Exported: ", p)
    } else {
      warning("Plot_ID not found: ", p)
    }
  }
  
}

# ---------------------------
# AOI1
# ---------------------------
process_aoi(
  las_path = "C:/Users/Alan Dumezweni/University of Exeter/TESSLab - Alan Nare Data/Dissertation/Dissertation Processed data/AOI1_pointcloud.laz",
  harvest_csv = "data/Harvest_plots_AOI1.csv",
  plants_to_export = c("S1P02"),
  output_folder = "exports"
)

# ---------------------------
# AOI2
# ---------------------------
process_aoi(
  las_path = "C:/Users/Alan Dumezweni/University of Exeter/TESSLab - Alan Nare Data/Dissertation/Dissertation Processed data/AOI2_pointcloud.laz",
  harvest_csv = "data/Harvest_plots_AOI2.csv",
  plants_to_export = c("S2P13", "S2P01", "S2P20"),
  output_folder = "exports"
)

# ---------------------------
# AOI3
# ---------------------------
process_aoi(
  las_path = "C:/Users/Alan Dumezweni/University of Exeter/TESSLab - Alan Nare Data/Dissertation/Dissertation Processed data/AOI3_pointcloud.laz",
  harvest_csv = "data/Harvest_plots_AOI3.csv",
  plants_to_export = c("S3P17","S3P20"),
  output_folder = "exports"
)
