# Final Project: Internal Phosphorus Loading in Lakes: Geochemical Pathways and Water Quality Implications (Trial Experiment)

## Project overview
This repository contains a reproducible R workflow for analyzing sediment phosphorus fractions and selected sediment physicochemical properties with depth. The project includes data cleaning, visualization, summary table generation, and a simple linear model.

## Repository structure
- `data/` : raw input data
- `analysis/` : R Markdown analysis file
- `output/figures/` : exported figures
- `output/tables/` : exported summary tables
- `final-project.Rproj` : R project file

## Software used
- R
- RStudio

## Required packages
- ggplot2
- dplyr
- tidyr
- readr
- ggpubr
- ggpattern
- here
- grid

## How to reproduce the analysis
1. Clone or download this repository.
2. Open `final-project.Rproj` in RStudio.
3. Make sure the data file is located at `data/Data file.csv`.
4. Open `analysis/final_project.Rmd`.
5. Knit the file to PDF or run all code chunks.

## Outputs
The workflow produces:
- manuscript-ready figures in `output/figures/`
- summary tables in `output/tables/`
- a compiled report in PDF and/or Markdown format

## Reproducibility features
- relative file paths using `here()`
- self-checks using `stopifnot()`
- reusable functions
- loop-based plot generation
- R Markdown documentation
- project-based file structure
