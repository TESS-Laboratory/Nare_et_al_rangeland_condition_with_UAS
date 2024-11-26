##########################################################################
###Canopy height modelling script
#### Script developed by Alan Nare and Andrew Cunliffe 2024

###Load necessary packages
library (lidR) # The lidR package provides tools for reading, processing, analysing, 
# and visualising point cloud data
library (rgl)   ##a versatile and interactive 3D viewer with points coloured by Z coordinates
library(mapview) ##Interactive viewing of point clod, raster and vector data
library(sf)     ### for reading AOI boundary shapefile
library(raster)
library(ggplot2)   ##Creating visualisations and plots
library(terra)      ####For CHM smoothing (Post processing)

#######################STEP 1##########################################
####Import point cloud data for AOIs---- 

aoi1 <- readLAS("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/Metashape Analysis/AOI1 RGB metashape analysis/Classified_AOI1.laz", select = "xyzrnc")
aoi2 <- readLAS("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/Metashape Analysis/AOI2 RGB metashape analysis/AOI2_pointcloud.laz", select = "xyzrnc")
aoi3 <- readLAS("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/Metashape Analysis/AOI3 RGB metashape analysis/AOI3_pointcloud.laz", select = "xyzrn")  # load XYZc only
hist(aoi1@data$Classification)


######################STEP 2###########################################
################Clipping out our AOI#######----
polyaoi1 <- st_read("C:/Users/202200875/Downloads/aoi1boundary/AOI1_Boundary.shp") ##Import shapefile
las_clipped_aoi1 <- clip_roi(aoi1, polyaoi1)


#####################STEP 3###Checking the data##############----

print(las_clipped_aoi1) ### Shows point cloud metadata
summary(las_clipped_aoi1) #### Detailed print out
las_check(las_clipped_aoi1) # note high number of duplicate points in current file.
hist(las_clipped_aoi1@data$Z)
plot(las_clipped_aoi1)

####################STEP 4 Filtering ground from pointcloud###################
ground <- filter_poi(las_clipped_aoi1, Classification == 2)
veg <- filter_poi(las_clipped_aoi1, Classification == 1)
plot(ground)

####################STEP 5 Generating DTM######################################
dtm_idw <- rasterize_terrain(ground, algorithm = knnidw(k = 10L, p = 2))
plot(dtm_idw)                    ############Colour visualisation
plot(dtm_idw, col = gray(1:50/50))###########Grey scale visualisation
??rasterize_terrain


#######################STEP 6 Height above ground (HAG)#####################
HAG <- normalize_height(las_clipped_aoi1, knnidw(), dtm = dtm_idw)   
####ground points in las_clipped_aoi1 are not considered for interpolation. The 
#DTM is used as regularly spaced ground points that are triangulated.
hist(HAG$Z)
plot(HAG)
??normalize_height()

##################STEP 7 - Canopy Height Model (CHM)##################----
##Using point to raster (p2r) algorithm#
chm <- rasterize_canopy(HAG, res = 0.01, algorithm = p2r(subcircle = 0.15))
col <- height.colors(25)
plot(chm, col = col)

###using dsmtin algorithm
CHM2 <- rasterize_canopy(HAG, res = 0.01, dsmtin(max_edge = 3, highest = TRUE))
plot(CHM2, col=col) #####This was found most suitable for our data


##Using triangulation method
CHM3 <- rasterize_canopy(HAG, res = 0.01, algorithm = dsmtin())
plot(CHM3, col = col)

#### Using pit free algotithm
CHM4 <- rasterize_canopy(HAG, res = 0.01, pitfree(max_edge = c(0, 2.5)))
plot(CHM4, col = col)

###Pit free
CHM5 <- rasterize_canopy(HAG, res = 0.5, pitfree(thresholds = c(0, 10, 20), max_edge = c(0, 1.5)))
plot(CHM5, col = col)



####################Visualing CHM############################----

### Apply custom color palette
max_height <- 10 # Adjust this value based on the maximum height in your data
breaks <- c(seq(0, 1, length.out = 50), max_height)
colors <- c(viridis::viridis(50), "red") # Gradient below 1m, red above 1m

### Smoothing and interpolation to fill gaps
CHM2_smooth <- focal(CHM2, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
plot(CHM2_smooth, breaks = breaks, col = colors, legend = TRUE, main = "CHM with Custom Color Palette")


#############Step 8 CHM post processing (smoothing) ########----

### Fill NAs and smooth the CHM2 ###
fill.na <- function(x, i=5) { 
  if (is.na(x)[i]) { 
    return(mean(x, na.rm = TRUE)) 
  } else { 
    return(x[i]) 
  }
}
w <- matrix(1, 3, 3)

# Fill gaps in CHM2
filled_CHM2 <- terra::focal(CHM2, w, fun = fill.na)

# Smooth CHM2
smoothed_CHM2 <- terra::focal(CHM2, w, fun = mean, na.rm = TRUE)

# Set negative values to 0
smoothed_CHM2[smoothed_CHM2 < 0] <- 0

# Plot the smoothed CHM2
plot(smoothed_CHM2, col = col)


########### Step 9 Export the smoothed CHM2 to a GeoTIFF file###################
writeRaster(smoothed_CHM2, "CHM2_smoothed.tif", overwrite = TRUE)

