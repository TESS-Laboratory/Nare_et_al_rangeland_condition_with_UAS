# ==========================================================
# Script: 04_export_harvest_plot_visualisations.R
# Author: Alan Nare
# Project: UAV estimation of herbaceous biomass in Kalahari savanna
# Purpose: Export NDVI, RGB, and canopy height visualisations
#          for individual harvest plots
# ==========================================================



#### Load libraries
library(terra)
library(tidyverse)
library(sf)

export_plot_ndvi <- function(ortho_path, harvest_csv, output_folder){
  
  # Load orthomosaic
  ortho <- rast(ortho_path)
  
  # Extract bands
  Red <- ortho[[3]]
  NIR <- ortho[[5]]
  
  # Calculate NDVI
  ndvi <- (NIR - Red) / (NIR + Red)
  names(ndvi) <- "NDVI"
  
  # Load harvest plot centres
  harvest <- read_csv(harvest_csv)
  
  plots_sf <- st_as_sf(
    harvest,
    coords = c("Easting(m)", "Northing(m)"),
    crs = crs(ndvi)
  )
  
  # Create circular harvest plots (0.33 m radius)
  plots_buffer <- st_buffer(plots_sf, dist = 0.33)
  
  # Loop through each plot
  for(i in 1:nrow(plots_buffer)){
    
    plot_id <- plots_buffer$Plot_ID[i]
    plot_geom <- plots_buffer[i,]
    
    # Crop NDVI to plot
    ndvi_crop <- crop(ndvi, vect(plot_geom))
    
    # Convert raster to dataframe
    ndvi_df <- as.data.frame(ndvi_crop, xy = TRUE, na.rm = TRUE)
    
    # Create plot
    p <- ggplot() +
      
      geom_raster(
        data = ndvi_df,
        aes(x = x, y = y, fill = NDVI)
      ) +
      
      geom_sf(
        data = plot_geom,
        colour = "blue",
        fill = NA,
        linewidth = 1
      ) +
      
      scale_fill_viridis_c(
        limits = c(-1,1),
        name = "NDVI"
      ) +
      
      scale_x_continuous(n.breaks = 3) +
      scale_y_continuous(n.breaks = 3) +
      
      coord_sf(expand = FALSE) +
      
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Export figure
    ggsave(
      filename = file.path(output_folder, paste0(plot_id, "_NDVI.tif")),
      plot = p,
      width = 5,
      height = 5,
      dpi = 300,
      compression = "lzw"
    )
    
    message("Exported: ", plot_id)
  }
}


#### Call function
export_plot_ndvi(
  ortho_path = "C:/Users/Alan Dumezweni/Downloads/MS orthomosaic/AOI1_orthomosaic_multispectral.tif",
  harvest_csv = "data/Harvest_plots_AOI1.csv",
  output_folder = "exports"
)


export_plot_ndvi(
  ortho_path = "C:/Users/Alan Dumezweni/Downloads/MS orthomosaic/AOI2_orthomosaic_multispectral.tif",
  harvest_csv = "data/Harvest_plots_AOI2.csv",
  output_folder = "exports"
)

export_plot_ndvi(
  ortho_path = "C:/Users/Alan Dumezweni/Downloads/MS orthomosaic/AOI3_orthomosaic_multispectral.tif",
  harvest_csv = "data/Harvest_plots_AOI3.csv",
  output_folder = "exports"
)



##########################################Export RGB



export_plot_rgb <- function(rgb_path, harvest_csv, output_folder){
  
  # Load RGB orthomosaic
  rgb <- rast(rgb_path)
  
  # Name bands
  names(rgb) <- c("Red","Green","Blue")
  
  # Load harvest plot centres
  harvest <- read_csv(harvest_csv)
  
  plots_sf <- st_as_sf(
    harvest,
    coords = c("Easting(m)", "Northing(m)"),
    crs = crs(rgb)
  )
  
  # Create circular plots (0.33 m)
  plots_buffer <- st_buffer(plots_sf, dist = 0.33)
  
  # Loop through each plot
  for(i in 1:nrow(plots_buffer)){
    
    plot_id <- plots_buffer$Plot_ID[i]
    plot_geom <- plots_buffer[i,]
    
    # Crop RGB to plot
    rgb_crop <- crop(rgb, vect(plot_geom))
    
    # Convert to dataframe
    rgb_df <- as.data.frame(rgb_crop, xy = TRUE, na.rm = TRUE)
    
    # Normalise RGB values for ggplot
    rgb_df <- rgb_df %>%
      mutate(
        Red = Red / max(Red),
        Green = Green / max(Green),
        Blue = Blue / max(Blue)
      )
    
    # Plot
    p <- ggplot() +
      
      geom_raster(
        data = rgb_df,
        aes(x = x, y = y,
            fill = rgb(Red, Green, Blue))
      ) +
      
      scale_fill_identity() +
      
      geom_sf(
        data = plot_geom,
        colour = "blue",
        fill = NA,
        linewidth = 1
      ) +
      
      scale_x_continuous(n.breaks = 3) +
      scale_y_continuous(n.breaks = 3) +
      
      coord_sf(expand = FALSE) +
      
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Export figure
    ggsave(
      filename = file.path(output_folder, paste0(plot_id, "_RGB.tif")),
      plot = p,
      width = 5,
      height = 5,
      dpi = 300,
      compression = "lzw"
    )
    
    message("Exported: ", plot_id)
  }
}

export_plot_rgb(
  rgb_path = "C:/Users/Alan Dumezweni/Downloads/RGB orthomosaic/AOI1_orthomosaic_rgb.tif",
  harvest_csv = "data/Harvest_plots_AOI1.csv",
  output_folder = "exports"
)

export_plot_rgb(
  rgb_path = "C:/Users/Alan Dumezweni/Downloads/RGB orthomosaic/AOI2_orthomosaic_rgb.tif",
  harvest_csv = "data/Harvest_plots_AOI2.csv",
  output_folder = "exports"
)

export_plot_rgb(
  rgb_path = "C:/Users/Alan Dumezweni/Downloads/RGB orthomosaic/AOI3_orthomosaic_rgb.tif",
  harvest_csv = "data/Harvest_plots_AOI3.csv",
  output_folder = "exports"
)



###########################Canopy Height Model############################

library(terra)
library(tidyverse)
library(sf)

export_plot_chm <- function(chm_path, harvest_csv, output_folder){
  
  # Load canopy height model
  chm <- rast(chm_path)
  names(chm) <- "CHM"
  
  # Load harvest plot centres
  harvest <- read_csv(harvest_csv)
  
  plots_sf <- st_as_sf(
    harvest,
    coords = c("Easting(m)", "Northing(m)"),
    crs = crs(chm)
  )
  
  # Create circular harvest plots (0.33 m)
  plots_buffer <- st_buffer(plots_sf, dist = 0.33)
  
  # Loop through each plot
  for(i in 1:nrow(plots_buffer)){
    
    plot_id <- plots_buffer$Plot_ID[i]
    plot_geom <- plots_buffer[i,]
    
    # Crop CHM to plot
    chm_crop <- crop(chm, vect(plot_geom))
    
    # Convert raster to dataframe
    chm_df <- as.data.frame(chm_crop, xy = TRUE, na.rm = TRUE)
    
    # Plot
    p <- ggplot() +
      
      geom_raster(
        data = chm_df,
        aes(x = x, y = y, fill = CHM)
      ) +
      
      geom_sf(
        data = plot_geom,
        colour = "blue",
        fill = NA,
        linewidth = 1
      ) +
      
      scale_fill_viridis_c(
        name = "Height (m)",
        limits = c(0, 1.5)   # adjust if your grass is taller/shorter
      ) +
      
      scale_x_continuous(n.breaks = 3) +
      scale_y_continuous(n.breaks = 3) +
      
      coord_sf(expand = FALSE) +
      
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Export figure
    ggsave(
      filename = file.path(output_folder, paste0(plot_id, "_CHM.tif")),
      plot = p,
      width = 5,
      height = 5,
      dpi = 300,
      compression = "lzw"
    )
    
    message("Exported: ", plot_id)
  }
}

export_plot_chm(
  chm_path = "C:/Users/Alan Dumezweni/University of Exeter/TESSLab - Alan Nare Data/Dissertation/Dissertation Processed data/AOI1_Canopy_Height_Model.tif",
  harvest_csv = "data/Harvest_plots_AOI1.csv",
  output_folder = "exports"
)

export_plot_chm(
  chm_path = "C:/Users/Alan Dumezweni/University of Exeter/TESSLab - Alan Nare Data/Dissertation/Dissertation Processed data/AOI2_Canopy_Height_Model.tif",
  harvest_csv = "data/Harvest_plots_AOI2.csv",
  output_folder = "exports"
)

export_plot_chm(
  chm_path = "C:/Users/Alan Dumezweni/University of Exeter/TESSLab - Alan Nare Data/Dissertation/Dissertation Processed data/AOI3_Canopy_Height_Model.tif",
  harvest_csv = "data/Harvest_plots_AOI3.csv",
  output_folder = "exports"
)
