#' Generate a canopy height model from a classified UAV point cloud
#'
#' Clips a classified LAS/LAZ point cloud to an AOI, extracts ground points,
#' generates a DTM, normalises point heights, creates a CHM using DSMTIN,
#' fills gaps, smooths the CHM, and exports the result as a GeoTIFF.
#'
#' @param las_path Path to the input LAS/LAZ point cloud.
#' @param aoi_path Path to the AOI boundary shapefile.
#' @param output_path Path for the output CHM GeoTIFF.
#' @param resolution CHM spatial resolution in metres.
#'
#' @return A smoothed canopy height raster.
#'
#' Script developed by Alan Nare and Andrew Cunliffe, 2024.

generate_chm <- function(las_path,
                         aoi_path,
                         output_path,
                         resolution = 0.01) {
  
  # Load point cloud
  las <- readLAS(las_path, select = "xyzrnc")
  
  if (is.empty(las)) {
    stop("The LAS/LAZ file contains no valid points: ", las_path)
  }
  
  # Read AOI boundary
  aoi <- st_read(aoi_path, quiet = TRUE)
  
  # Clip point cloud to AOI
  las_clipped <- clip_roi(las, aoi)
  
  if (is.empty(las_clipped)) {
    stop("No point-cloud data overlap the AOI: ", aoi_path)
  }
  
  # Separate ground points
  ground <- filter_poi(las_clipped, Classification == 2)
  
  if (is.empty(ground)) {
    stop("No ground-classified points found in: ", las_path)
  }
  
  # Generate DTM
  dtm <- rasterize_terrain(
    ground,
    algorithm = knnidw(k = 10L, p = 2)
  )
  
  # Calculate height above ground
  hag <- normalize_height(
    las_clipped,
    knnidw(),
    dtm = dtm
  )
  
  # Generate CHM using DSMTIN
  chm <- rasterize_canopy(
    hag,
    res = resolution,
    algorithm = dsmtin(max_edge = 3, highest = TRUE)
  )
  
  # Fill gaps
  fill.na <- function(x, i = 5) {
    if (is.na(x[i])) {
      return(mean(x, na.rm = TRUE))
    } else {
      return(x[i])
    }
  }
  
  w <- matrix(1, 3, 3)
  
  filled_chm <- terra::focal(
    chm,
    w,
    fun = fill.na
  )
  
  # Smooth CHM
  smoothed_chm <- terra::focal(
    filled_chm,
    w,
    fun = mean,
    na.rm = TRUE
  )
  
  # Set negative heights to zero
  smoothed_chm[smoothed_chm < 0] <- 0
  
  # Export CHM
  writeRaster(
    smoothed_chm,
    output_path,
    overwrite = TRUE
  )
  
  message("CHM exported to: ", output_path)
  
  return(smoothed_chm)
}


###### Call function for each AOI

generate_chm(
  las_path = "data/processed/laz/AOI1_pointcloud.laz",
  aoi_path = "data/raw/vector/AOI1_Boundary_shapefile.shp",
  output_path = "outputs/raster/AOI1_CHM.tif"
)

generate_chm(
  las_path = "data/processed/laz/AOI2_pointcloud.laz",
  aoi_path = "data/raw/vector/AOI2_Boundary_shapefile.shp",
  output_path = "outputs/raster/AOI2_CHM.tif"
)

generate_chm(
  las_path = "data/processed/laz/AOI3_pointcloud.laz",
  aoi_path = "data/raw/vector/AOI3_Boundary_shapefile.shp",
  output_path = "outputs/raster/AOI3_CHM.tif"
)