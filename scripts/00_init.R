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
    "plot.matrix",
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
# library(ExPosition)
# library(TExPosition)
# library(GSVD)
# library(GPLS)
# library(ours)

github_packages <-
  c("ExPosition",
    "TExPosition",
    "GSVD",
    "GPLS",
    "ours"
  )

github_repos <- 
  c("derekbeaton/ExPosition1/ExPosition",
    "derekbeaton/ExPosition1/TExPosition",
    "derekbeaton/GSVD",
    "derekbeaton/OuRS/OuRS",
    "derekbeaton/GPLS/Package"
  )

using_github(libs = github_packages, repos = github_repos)

missing_packages <- 
  !(paste0("package:", c(required_packages, github_packages)) %in% search())

if (any(missing_packages)) {
  missing_packages <- c(required_packages, github_packages)[which(missing_packages)]
  missing_msg <- paste0("Required packages could not be loaded. Please ensure all required packages are installed and loadable, then re-run this script (00_init.R) before moving to the next step. \n\r\n\rThe required packages that were not loaded include: '", paste0(c(missing_packages), sep = "'", collapse = ", '"))
  stop(missing_msg)
} else {
  rm(missing_packages)
  message("All required packages are loaded")
}

message("./scripts/00_init.R was executed")

