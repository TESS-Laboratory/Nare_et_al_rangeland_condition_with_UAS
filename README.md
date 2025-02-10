## Evaluating rangeland condition using remote sensing from Unoccupied Aerial Vehicles (UAVs) 

This repository includes summary data and the analysis codebase for the manuscript:

## Evaluating rangeland condition using remote sensing from Unoccupied Aerial Vehicles (UAVs) 

##Contributors: Alan Nare (alandnare@gmail.com) and Andrew Cunliffe


To assess how UAV fine-scale remote sensing could contribute to monitoring and mapping AGB in Kalahari savanna ecosystems, this study addressed the following research questions:

1. How well can aboveground biomass in Kalahari savanna ecosystems be predicted by fine-scale UAV observations of canopy height and spectral reflectance?
2. How well can AGB of foraging importance be predicted by fine-scale UAV observations of canopy height and spectral reflectance?
3. Do these relationships between biomass components and remotely sensed attributes differ across different levels of grazing intensity?


We surveyed three areas of interest (50 m x 50 m each) across a gradient of grazing 
intensity in the Southwestern Kalahari, Botswana, using an RGB camera for Structure 
from Motion (SfM) photogrammetry to derive canopy height, and a multispectral multi-camera 
array to capture spectral reflectance data for vegetation indices. Biomass samples 
were destructively harvested from 30 plots within each site to provide ground truth 
data for AGB estimation. Robust linear regression was used to assess (1) the 
relationship between UAV-derived canopy height and spectral reflectance for estimating 
AGB, (2) the effectiveness of UAV observations in predicting herbaceous biomass of 
foraging importance, and (3) the transferability of these models by testing them 
across gradients of grazing intensity.

![image](https://github.com/user-attachments/assets/dc2ee052-9279-4dc0-8c88-7f16900e1a5c)


### The repo contains the following sub-folders in the "Scripts" folder:

"Metadata_Extraction" folder Contains scripts for extracting EXIF metadata from drone images.
"Canopy_Height_Modeling" Contains the script for creating Canopy Height Models (CHM) using drone-derived point cloud data.
"Zonal_Statistics" Contains scripts for extracting mean canopy heights and vegetation indices from a raster for specified harvest plots
"Research_Questions" Contains scripts that address the three research questions.
