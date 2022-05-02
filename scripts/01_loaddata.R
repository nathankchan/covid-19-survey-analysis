# File name: 01_loaddata.R
# Path: './scripts/01_loaddata.R'

# Author: NK Chan
# Purpose: Loads the COVID-19 Behavior Determinants Database from Harvard Dataverse

# Load and install missing packages
source(paste0(getwd(), "/scripts/00_init.R"))

# Load in data from intermediate file. Otherwise, retrieve data in "original"
# form (i.e., as an SPSS binary) and parse with read_sav() from haven.

if (file.exists(paste0(getwd(), "/output/coviddata.RDS"))) {
  coviddata <-
    readRDS(file = paste0(getwd(), "/output/coviddata.RDS"))
  message("coviddata read in from ./output/coviddata.RDS")
} else {
  if (file.exists(paste0(getwd(),"/data/COVID-19 Behavior Determinants Database_v1.0.sav"))) {
    coviddata <-
      haven::read_sav(file = "./data/COVID-19 Behavior Determinants Database_v1.0.sav")
    message("coviddata read in from ./data/COVID-19 Behavior Determinants Database_v1.0.sav")
  } else {
    # Note: this will not run if used too frequently... the connection may be refused by Dataverse
    coviddata <- get_dataframe_by_name(
      filename = "COVID-19 Behavior Determinants Database_v1.0.tab",
      dataset = "10.7910/DVN/NILCAV",
      server = "dataverse.harvard.edu",
      original = TRUE,
      .f = haven::read_sav
    )
    message("coviddata downloaded from Harvard Dataverse (doi.org/10.7910/DVN/NILCAV)")
  }
  
  saveRDS(object = coviddata, 
        file = paste0(getwd(), "/output/coviddata.RDS"))
  message("coviddata saved to ./output/coviddata.RDS")
}

message("./scripts/01_loaddata.R was executed")
