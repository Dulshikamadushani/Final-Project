# Final Project: Internal Phosphorus Loading in Lakes: Geochemical Pathways and Water Quality Implications (Trial Experiment)

## Project overview
This repository contains a reproducible R workflow for analyzing sediment phosphorus fractions and selected sediment physicochemical properties with depth. The project includes data cleaning, visualization, summary table generation, and a simple linear model.

## Repository structure
Final-Project/
├── Results/
│   ├── Final project new.Rmd
│   ├── Final-project-new.md
│   └── Final-project-new.pdf
├── Data file.csv
└── README.md

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
2. Open `Final-project.Rproj` in RStudio.
3. Make sure the data file is located at `Final-project/Data file.csv`.
4. Open `final_project new.Rmd`.
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
