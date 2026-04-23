# Final Project: Internal Phosphorus Loading in Lakes: Geochemical Pathways and Water Quality Implications (Trial Experiment)

## Project overview
This repository contains a reproducible R workflow for analyzing sediment phosphorus fractions and selected sediment physicochemical properties with depth. The project includes data cleaning, visualization, summary table generation, and a simple linear model.

## Repository structure
- `data/` : raw input data
- `analysis/` : R Markdown file
- `output/figures/` : exported figures
- `output/tables/` : exported summary tables
- `final-project.Rproj` : R project file

## Software used
- RStudio

## Required packages
- ggplot2
- dplyr
- tidyr
- readr
- ggpubr
- ggpattern
- grid

## How to reproduce the analysis
1. Download this repository.
2. Open `final-project.Rproj` in RStudio.
3. Make sure the data file is located at `Final project/Data file.csv`.
4. Open `final_project.Rmd`.
5. Knit the file to PDF or run all code chunks.

## Outputs
The workflow produces:
- manuscript-ready figures
- summary tables
- a compiled report in PDF and Markdown format

## Reproducibility features
- input files are called using relative paths (for example, `read.csv("Data file.csv")`) rather than machine-specific absolute paths
- basic data checks are included to ensure the dataset is correctly loaded and structured
- reusable functions
- loop-based plot generation
- R Markdown documentation
- project-based file structure
