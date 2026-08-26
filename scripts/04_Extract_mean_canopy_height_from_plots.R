#' Extract mean canopy height for harvest plots
#'
#' Extracts mean canopy height from a UAV-derived CHM within 0.33 m buffers
#' around harvest-plot sampling points and exports the results as a CSV.
#
#' @param chm_path Path to the canopy height model GeoTIFF.
#' @param plots_path Path to the harvest-plot CSV file.
#' @param output_path Path for the output CSV file.
#' @param buffer_distance Buffer radius around each harvest plot in metres.
#
#' @return A data frame containing the original harvest-plot data and extracted
#' mean canopy heights.

extract_mean_ch <- function(chm_path,
                            plots_path,
                            output_path,
                            buffer_distance = 0.33) {
  
  # Read CHM
  chm <- terra::rast(chm_path)
  
  # Read harvest-plot data
  harvest_plots <- readr::read_csv(plots_path, show_col_types = FALSE)
  
  # Convert harvest plots to spatial points
  harvest_points <- sf::st_as_sf(
    harvest_plots,
    coords = c("Easting(m)", "Northing(m)"),
    crs = sf::st_crs(chm)
  )
  
  # Create buffers around harvest plots
  buffers <- sf::st_buffer(
    harvest_points,
    dist = buffer_distance
  )
  
  # Extract mean canopy height
  mean_canopy_heights <- exactextractr::exact_extract(
    chm,
    buffers,
    "mean"
  )
  
  # Add extracted values to original data
  harvest_plots$Mean_Canopy_Height <- mean_canopy_heights
  
  # Save output
  readr::write_csv(
    harvest_plots,
    output_path
  )
  
  message("Mean canopy heights exported to: ", output_path)
  
  return(harvest_plots)
}

##### Call function for each AOI

extract_mean_ch(
  chm_path = "outputs/raster/AOI1_CHM.tif",
  plots_path = "data/raw/Harvest_plots_AOI1.csv",
  output_path = "data/processed/extracted_mean_ch_aoi1.csv"
)

extract_mean_ch(
  chm_path = "outputs/raster/AOI2_CHM.tif",
  plots_path = "data/raw/Harvest_plots_AOI2.csv",
  output_path = "data/processed/extracted_mean_ch_aoi2.csv"
)

extract_mean_ch(
  chm_path = "outputs/raster/AOI2_CHM.tif",
  plots_path = "data/raw/Harvest_plots_AOI3.csv",
  output_path = "data/processed/extracted_mean_ch_aoi3.csv"
)