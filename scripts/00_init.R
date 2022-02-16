# File name: 00_init.R
# Path: './scripts/00_init.R'

# Author: NK Chan
# Purpose: Initializes project and installs required dependencies.

# Load up custom functions
source(paste0(getwd(), "/R/functions.R"))
message("./R/functions.R is loaded.")

# List of required packages for analysis
required_packages <-
  c(
    "dataverse",
    "mice",
    "knitr",
    "kableExtra",
    "tidyverse",
    "haven",
    "plotly",
    "htmlwidgets",
    "shiny")

# Check, install, and load required packages
using(required_packages)

message("./scripts/00_init.R was executed.")

