
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CATIE_analysis_modules

<!-- badges: start -->
<!-- badges: end -->

The goal of CATIE_analysis_modules is to bundle the data set and code
used for the CATIE Virtual days into an R project.

## Setup

1.  Download the zip file or clone the GitHub repo to your computer
2.  Launch the CATIE_analysis_modules.Rproj file in R studio
3.  The R environment should activate
4.  Type renv::restore() to download the necessary packages

## Virtual modules

There are two virtual modules that give an overview of common data
analytic work flows.

- Virtual module 2 (V2) Primary Aims analytics:
  `V2_primary_aims_workbook`

  - Main effects of first stage treatment
  - Main effects of second stage treatment
  - Mean of embedded adaptive interventions

- Virtual module 3 (V3) Secondary Aims analytics:
  `V3_secondary_aims_workbook`

  - Implement Q-learning to test the utility of candidate moderators for
    tailoring first and second-stage intervention options.

  - Implement `qlaci()`

Follow along with the rendered `.html` documents or run the code in the
interactive `.qmd` file.

## Code Examples

Simple R scripts to practice implementing V2 & V3 concepts.

- `primary_aims_example_script.R`

- `secondary_aims_example_script.R`

## /Data

- `adhd-simulated-2023.csv` *simulated* data set similar to the ADHD
  SMART study conducted by Bill Pelham. For instructional purposes only.

## /Docs

- Folder with handouts describing the ADHD SMART study design and
  variables in the simulated dataset.

## /R

- `estimate.R` function to compute contrasts of model coeffecients

## qlaci package

`qlaci` can be downloaded from D3C GitHub:
<https://github.com/d3center-isr/qlaci>
