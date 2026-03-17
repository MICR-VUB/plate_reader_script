# Analyse OD & fluorescence

This folder contains an R-based workflow for processing and visualising data containing OD and fluorescence measurments.
The script has been tested for 96-well plates, but should also be usable for other formats.

## Folder Structure

- The `.Rproj` file is located in the root directory and sets the working directory accordingly. **Do not move this file unless you know what you're doing.**
- `Data/` contains all input files:
  - An `.xlsx` file with raw OD (optical density) and fluorescence measurements.
  - A `template.csv` file with metadata (e.g., treatment layout, ligand names, etc.).
- `Script/` contains all the code the form of .Rmd files.
- All outputs (e.g., processed data, plots, knitted reports) are saved to `Output/`.
- helpers.R, containing helper functions used by the .Rmd scripts, should be in the parent directory with respect to the project folder.

## Overview
### 1. Data Import (`01_import.Rmd`)

- Loads raw optical density (OD) and fluorescence data from an `.xlsx` file located in the `Data/` subfolder.
- Annotates the data with metadata parsed from:
  - The well positions.
  - The `template.csv` metadata file.
- Transforms the data into a tidy, long-format data frame suited for further processing and visualisation.

### 2. Data Visualisation (`02_data-vis.Rmd`)

- Provides a basic exploratory analysis of the OD and fluorescence time-courses.

### 3. Data Visualisation (`03_stationary_phase.Rmd`)

- Analyse normalised fluorescence output averaged for three consecutive time points in the stationary phase.

## Requirements

This script was tested using R version 4.5.2 and following package versions:

- `readxl (1.4.5)`
- `dplyr (1.1.4)`
- `data.table (1.17.8)`
- `ggplot2 (4.0.1)`
- `forcats (1.0.1)`
- `patchwork` (1.3.2)
- `ggthemes` (5.1.0)

## Usage

1. Add your raw OD and fluorescence data (`raw_data.xlsx`) and corresponding `template.csv` file to the `Data/` folder.
2. Open the R project in RStudio (`*.Rproj`).
3. Run `01_import.Rmd` to import and annotate the data.
4. Run `02_data-vis.Rmd` to generate plots and explore the data.
5. Run `03_stationary-phase`.Rmd` to analyse data in the stationary phase.
