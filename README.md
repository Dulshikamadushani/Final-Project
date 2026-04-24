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
- R (version 4.0 or higher recommended)
- RStudio (recommended for ease of use)

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
2. Open RStudio.
3. Set the working directory to the repository folder (where Data file.csv is located).
4. Navigate to the Results/ folder.
5. Open the file:
   Final project new.Rmd
6. Click Knit (to PDF or HTML), or run all code chunks sequentially.

## Running the R Markdown file will generate:
- Multi-panel figures of sediment properties
- Phosphorus fraction distribution plots
- Percentage composition plots
- Linear model results
- A compiled report in PDF and Markdown formats

## Reproducibility features
- Use of relative file paths (e.g., "Data file.csv")
- R Markdown for transparent and automated reporting
- Reusable functions for repeated analytical steps
- Loop-based generation of multiple figures
- Structured workflow from raw data to final outputs
