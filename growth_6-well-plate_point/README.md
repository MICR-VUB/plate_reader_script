# Import and parse 6-well plate readings

This folder contains an R-based workflow for importing and parsing optical density (OD) readings from 6-well plates contained in different sheets of an Excel file. The script is designed to handle data from multiple experiments involving blank medium measurements and correct the OD values accordingly.

## Table of Contents

- [Introduction](#introduction)  
- [Requirements](#requirements)  
- [Folder Structure](#folder-structure)  
- [Usage](#usage)

## Introduction

This project provides a set of functions to parse and process OD readings from 6-well plates. The main script reads data from an Excel file, parses it, and corrects the OD values for blank medium measurements. The processed data is then saved in both `.RData` and `.csv` formats. 

## Requirements

This script was tested using R version 4.4.3 and following package versions:

- `readxl (1.4.5)`
- `tibble (3.2.1)`
- `dplyr (1.1.4)`
- `data.table (1.17.0)`
- `ggplot2 (3.5.2)`

## Folder Structure
- The `.Rproj` file is located in the root directory and sets the working directory accordingly. **Don't move the .Rproj file unless you know what you're doing.**
- `Data/` contains all input files:
  - The Excel file with raw OD measurements across multiple sheets.
- `Script/` contains all the code the form of .Rmd files.
- All output (e.g., processed data, plots, knitted reports) is saved to `Output/`.

## Usage

Before running the script, ensure the following:
1. Place your raw Excel file in the `Data/` subfolder and update the `file_name` variable in the script accordingly.
2. All raw data is contained in cells `C28:E29` for each sheet of the Excel file.
3. The sheets must be ordered as follows:
   - The first `num_experiments` sheets contain blank measurements for each experiment.
   - The next `num_experiments` sheets contain measurements at timepoint 1 for each experiment.
   - This pattern continues for subsequent time points.
**The script matches experimental sheets to their corresponding blanks based on sheet order, so consistency is essential.**
