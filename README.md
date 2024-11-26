# Nare_dev

## Evaluating rangeland condition using remote sensing from Unoccupied Aerial Vehicles (UAVs) 
### Contributors: Alan Nare (alandnare@gmail.com) and Andrew Cunliffe

Description: This research assessed how UAV fine-scale remote sensing could contribute to monitoring and mapping AGB in Kalahari savanna ecosystems with large proportions of non-palatable herbaceous plants. The research focuses on assessing the predictive capacity of RGB and multispectral datasets in estimating aboveground biomass in a Kalahari savanna ecosystem in Botswana. Structure from Motion (SfM) photogrammetry was performed on RGB images to generate canopy height data, while multispectral data was utilised to capture spectral reflectance information. We did destructive harvesting on harvest plots to establish the allometric relationship between biomass and UAV derived canopy height and spectral reflectance. The following research questions were addressed: 
#1 How well can aboveground biomass in Kalahari savanna ecosystems be predicted by fine-scale UAV observations of canopy height and spectral reflectance?; 
#2 How well can AGB of foraging importance be predicted by fine-scale UAV observations of canopy height and spectral reflectance?; 
#3 Do these relationships between biomass components and remotely sensed attributes differ across different levels of grazing intensity?

The following scripts were used for this study.

1). Function for extracting GCP metadata- This R script extracts EXIF metadata from images captured using DJI drones (e.g., Phantom 4 RTK and Phantom 4 Multispectral) to obtain x, y, and z coordinates, date and time stamps, and RTK accuracy information. The extracted metadata is saved as CSV files for each Area of Interest (AOI).
2). Harvest plot metadata extraction- Performs the same function as the GCP EXIF extraction script, but it extracts exif data for harvest plots to obtain x,y,z coordinates
3). Canopy Height Model- This script performs canopy height modelling (CHM) using drone-derived point cloud data. It imports LAS files, clips out areas of interest (AOIs), filters ground points, generates a Digital Terrain Model (DTM), and calculates heights above ground (HAG). It then creates CHMs using different algorithms,  and post-processes the CHM by filling gaps, smoothing, and exporting the result as a GeoTIFF.
4). Biomass_Summary- This script cleans biomass data from a CSV file, computes the total dry weight for each plot, and identifies the rows (plant species) corresponding to maximum biomass for each plot. It then saves the summarised data as a new CSV file and performs various visualizations using ggplot2.
5). Zonal statistics- This script extracts mean canopy heights from a raster for specified harvest plots. It reads the canopy height model (CHM) raster and the CSV file with plot coordinates, creates 33 cm buffers around each plot, and uses the exactextractr package to compute mean canopy heights within these buffers. The resulting mean values are added to the original CSV file as a new column.
