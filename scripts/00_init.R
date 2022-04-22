# File name: 00_init.R
# Path: './scripts/00_init.R'

# Author: NK Chan
# Purpose: Initializes project and installs required dependencies.

# Load up custom functions
source(paste0(getwd(), "/R/functions.R"))
message("./R/functions.R is loaded.")

# List of required packages for analysis
required_packages <-
  c("remotes",
    "dataverse",
    "mice",
    "knitr",
    "kableExtra",
    "tidyverse",
    "haven",
    "plotly",
    "htmlwidgets",
    "shiny",
    "colorspace")

# Check, install, and load required packages
using(required_packages)

# Install and load custom github packages
# remotes::install_github("derekbeaton/ExPosition1/ExPosition")
# remotes::install_github("derekbeaton/ExPosition1/TExPosition")
# remotes::install_github("derekbeaton/GSVD")
# remotes::install_github("derekbeaton/OuRS/OuRS")
# remotes::install_github("derekbeaton/GPLS/Package")
library(ExPosition)
library(TExPosition)
library(GSVD)
library(GPLS)
library(ours)


message("./scripts/00_init.R was executed.")

