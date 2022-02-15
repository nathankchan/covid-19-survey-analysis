# File name: 01_loaddata.R
# Path: './scripts/01_loaddata.R'

# Author: NK Chan
# Purpose: Loads the COVID-19 Behavior Determinants Database from Harvard Dataverse

# Load and install missing packages
source(paste0(getwd(), "/scripts/00_init.R"))

# Retrieve data in "original" form (i.e., as the SPSS binary), 
# and parse with read_sav() from haven
coviddata <- get_dataframe_by_name(
  filename = "COVID-19 Behavior Determinants Database_v1.0.tab",
  dataset = "10.7910/DVN/NILCAV",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = haven::read_sav
)

message("./scripts/01_loaddata.R was executed.")
