#' Extract UAV spectral reflectance and vegetation indices for harvest plots
#'
#' Calculates nine vegetation indices from five-band multispectral UAV
#' orthomosaics and extracts mean reflectance and vegetation-index values
#' within 0.33 m buffers around each harvest-plot sampling point.
#'
#' @param ms_path Path to the multispectral orthomosaic.
#' @param harvest_csv Path to the harvest-plot CSV file.
#' @param output_csv Path for the output CSV file.
#' @param buffer_radius Buffer radius around each harvest plot in metres.
#'
#' @return A data frame containing the harvest-plot data, mean band
#' reflectance, and mean vegetation-index values.

extract_spectral_indices <- function(ms_path,
                                     harvest_csv,
                                     output_csv,
                                     buffer_radius = 0.33) {
  
  # Load multispectral orthomosaic
  multispectral_ortho <- terra::rast(ms_path)
  
  # Load harvest-plot data
  harvest_plots <- readr::read_csv(
    harvest_csv,
    show_col_types = FALSE
  )
  
  # Extract individual bands
  Blue    <- multispectral_ortho[[1]]
  Green   <- multispectral_ortho[[2]]
  Red     <- multispectral_ortho[[3]]
  Rededge <- multispectral_ortho[[4]]
  NIR     <- multispectral_ortho[[5]]
  
  # Calculate vegetation indices
  NDVI <- (NIR - Red) / (NIR + Red)
  
  EVI <- 2.5 * (NIR - Red) /
    (NIR + 6 * Red - 7.5 * Blue + 1)
  
  OSAVI <- (NIR - Red) /
    (NIR + Red + 0.16)
  
  TDVI <- 1.5 * (NIR - Red) /
    sqrt(NIR^2 + Red + 0.5)
  
  GDVI <- NIR - Green
  
  TCARI <- 3 * (
    (NIR - Red) -
      0.2 * (NIR - Green) *
      ((NIR - Red) / (NIR + Red))
  )
  
  MCARI <- (
    (NIR - Red) -
      0.2 * (NIR - Green)
  ) * (NIR / Red)
  
  NDRE <- (NIR - Rededge) /
    (NIR + Rededge)
  
  GCI <- (NIR / Green) - 1
  
  # Combine vegetation indices
  indices_stack <- c(
    NDVI, EVI, OSAVI, TDVI, GDVI,
    TCARI, MCARI, NDRE, GCI
  )
  
  names(indices_stack) <- c(
    "NDVI", "EVI", "OSAVI", "TDVI", "GDVI",
    "TCARI", "MCARI", "NDRE", "GCI"
  )
  
  # Convert harvest plots to spatial points
  harvest_points <- sf::st_as_sf(
    harvest_plots,
    coords = c("Easting(m)", "Northing(m)"),
    crs = sf::st_crs(multispectral_ortho)
  )
  
  # Create buffers around harvest plots
  buffers <- sf::st_buffer(
    harvest_points,
    dist = buffer_radius
  )
  
  # Stack original spectral bands
  band_stack <- c(
    Blue, Green, Red, Rededge, NIR
  )
  
  names(band_stack) <- c(
    "B1_Blue",
    "B2_Green",
    "B3_Red",
    "B4_RedEdge",
    "B5_NIR"
  )
  
  # Extract mean reflectance
  mean_bands_df <- as.data.frame(
    exactextractr::exact_extract(
      band_stack,
      buffers,
      "mean"
    )
  )
  
  # Extract mean vegetation-index values
  mean_indices_df <- as.data.frame(
    exactextractr::exact_extract(
      indices_stack,
      buffers,
      "mean"
    )
  )
  
  names(mean_indices_df) <- paste0(
    "Mean_",
    names(indices_stack)
  )
  
  # Combine results
  harvest_final <- cbind(
    harvest_plots,
    mean_bands_df,
    mean_indices_df
  )
  
  # Export results
  readr::write_csv(
    harvest_final,
    output_csv
  )
  
  message(
    "Spectral extraction complete: ",
    output_csv
  )
  
  return(harvest_final)
}


##### Call function for each aoi
extract_spectral_indices(
  ms_path = "data/processed/raster/AOI1_orthomosaic_multispectral.tif",
  harvest_csv = "data/raw/Harvest_plots_AOI1.csv",
  output_csv = "data/processed/extracted_reflectance_AOI1.csv"
)

extract_spectral_indices(
  ms_path = "data/processed/raster/AOI2_orthomosaic_multispectral.tif",
  harvest_csv = "data/raw/Harvest_plots_AOI2.csv",
  output_csv = "data/processed/extracted_reflectance_AOI2.csv"
)

extract_spectral_indices(
  ms_path = "data/processed/raster/AOI3_orthomosaic_multispectral.tif",
  harvest_csv = "data/raw/Harvest_plots_AOI3.csv",
  output_csv = "data/processed/extracted_reflectance_AOI3.csv"
)