# Evaluating rangeland condition using remote sensing from Unoccupied Aerial Vehicles (UAVs)

> Alan D Nare., Glenn Slade., Lawrence Akanyang., Jeremy S Perkins & Andrew M Cunliffe. (2026). *Canopy height from drone photogrammetry better predicts aboveground biomass than vegetation greenness indices in a semi-arid savanna*. African Journal of Range & Forage Science. DOI: https://doi.org/10.2989/10220119.2026.2688781

A permanent version of this repository is archived at [![DOI](https://doi.org/10.5281/zenodo.18187018)](...)

Use of this code is licensed under [![Licence: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](...)

Contact Alan D Nare. and/or Andrew M Cunliffe at (a.dumezweni@exeter.ac.uk / a.cunliffe@exeter.ac.uk).
---
![image alt](https://github.com/TESS-Laboratory/Nare_et_al_rangeland_condition_with_UAS/blob/main/TESS%20Logo%20normal.png?raw=true).
---

## Project overview

This repository contains the code used to process, analyse, and visualise UAV 
remote-sensing data for assessing herbaceous aboveground biomass (AGB) in a 
semi-arid Kalahari savanna. The study evaluates whether UAV-derived canopy 
height from structure-from-motion (SfM) photogrammetry and spectral vegetation 
indices can predict AGB across a gradient of grazing intensity, and whether 
these relationships vary with grazing pressure. Field data were collected from 
90 biomass harvest plots across three study areas near Zutshwa, Botswana, 
alongside RGB and multispectral UAV imagery. The repository provides the R 
workflows used for point-cloud processing, canopy height modelling, vegetation-index 
extraction, statistical modelling, machine learning, and visualisation.

---


## Repository contents

This repository contains the R scripts and supporting files used to process, 
analyse and visualise the data presented in the study.The SfM point clouds, RGB 
and multispectral orthomosaics, and canopy height models (CHMs) used in the 
study are available from [Zenodo](https://doi.org/10.5281/zenodo.16874170).

### Scripts

| Script | Description |
|---------|-------------|
| *01_Harvest_plot_metadata_extraction.R* | Downloads or imports raw datasets. |
| *02_Function_for_extracting_GCP_EXIF_metadata.R* | Cleans and prepares datasets for analysis. |
| *03_Canopy_Height_Model.R* | Fits statistical or machine learning models. |
| *04_Extract_mean_canopy_height_from_plots.R* | Performs model validation and accuracy assessment. |
| *05_Calculate_and_extract_vegetation_indice_values* | Produces all manuscript figures. |
| *06_Question 1.R* | Produces manuscript tables and supplementary outputs. |
| *07_Question_2_Random_forest.R* | Produces manuscript tables and supplementary outputs. |
| *08_Question 3.R* | Produces manuscript tables and supplementary outputs. |


### Data


| File                                                                                  | Description                                               |
| ------------------------------------------------------------------------------------- | --------------------------------------------------------- |
| *Data.csv*                                                                            | Main dataset used for statistical analysis and modelling. |
| *Alan 2024-03-26 - Biomass and Coordinate Reporting Spreadsheet - Biomass Drying.csv* | Field biomass measurements and plot coordinates.          |
| *Biomass_partitioned.csv*                                                             | Biomass data used for analysis partitioned by species.               |
| *Biomass_summary.csv*                                                                 | Summary of field biomass measurements.                    |
| *chirps_precipitation_2000_2024.csv*                                                  | CHIRPS precipitation data for 2000–2024.                  |
| *extracted_mean_ch_aoi1.csv*                                                          | Mean canopy height extracted for harvest plots in AOI1.   |
| *extracted_mean_ch_aoi2.csv*                                                          | Mean canopy height extracted for harvest plots in AOI2.   |
| *extracted_mean_ch_aoi3.csv*                                                          | Mean canopy height extracted for harvest plots in AOI3.   |
| *extracted_reflectance_AOI1.csv*                                                      | UAV-derived reflectance data for harvest plots in AOI1.   |
| *extracted_reflectance_AOI2.csv*                                                      | UAV-derived reflectance data for harvest plots in AOI2.   |
| *extracted_reflectance_AOI3.csv*                                                      | UAV-derived reflectance data for harvest plots in AOI3.   |
| *GCP_locationsAOI1.csv*                                                               | Ground control point coordinates for AOI1.                |
| *GCP_locationsAOI2.csv*                                                               | Ground control point coordinates for AOI2.                |
| *GCP_locationsAOI3.csv*                                                               | Ground control point coordinates for AOI3.                |
| *Grazing_value.csv*                                                                   | Grass species grazing value.            |
| *Harvest_plots_AOI1.csv*                                                              | Harvest-plot field data for AOI1.                         |
| *Harvest_plots_AOI2.csv*                                                              | Harvest-plot field data for AOI2.                         |
| *Harvest_plots_AOI3.csv*                                                              | Harvest-plot field data for AOI3.                         |
| *precipitation_sept_april.csv*                                                        | September–April precipitation data used in the analysis.  |
| *Synthetic_grazing_value.csv*                                                         | Synthetic grazing value indices for different plots.      |


## Getting started

Clone this repository and review the project overview and repository structure before running the analysis. 

TESS Lab projects typically use [renv](https://rstudio.github.io/renv/) to record package dependencies and software versions. 
Where an renv.lock file is included, restore the project environment before running any analyses:
```r
renv::restore()
```
Download the large UAV datasets (SfM point clouds, RGB and multispectral 
orthomosaics, and CHMs) from [Zenodo](https://doi.org/10.5281/zenodo.16874170) 
and place them in the corresponding directories specified in the analysis scripts 
before running the workflow.


## Running the analysis


Scripts should be run in numerical order. **Scripts 01 and 02 can be skipped** 
when reproducing the analysis, as they require the original DJI drone images as 
input. These raw drone images are not included in this repository; the metadata 
generated by these scripts are provided in the `data/` directory.

The remaining scripts can be run using the data and UAV-derived products 
provided in the repository and archived on Zenodo.

![image alt](https://github.com/TESS-Laboratory/Nare_et_al_rangeland_condition_with_UAS/blob/main/Picture1.png?raw=true)

## Funding acknowledgement

We acknowledge funding support from the Oppenheimer Programme in African Landscape 
Systems (OPALS), jointly funded by the University of Exeter, Sarah Turvill and 
Oppenheimer Generations Research and Conservation, as well as the Botswana University 
of Agriculture and Natural Resources. 

---
