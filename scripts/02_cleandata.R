# File name: 02_cleandata.R
# Path: './scripts/02_cleandata.R'

# Author: NK Chan
# Purpose: Cleans data by searching for outliers and missing values

source(paste0(getwd(), "/scripts/01_loaddata.R"))

# Select columns with quantitative data (i.e., contains numeric values or data
# that can be coerced to numerics). Note that categorical data are permissible
# as categories can be read in as factors (and are thus coercible to numeric).
# Thus, any qualitative data (e.g. short responses/strings) are excluded.
coviddata_num <- coviddata[, which(sapply(coviddata, is.numeric))]

# Make a data frame listing the question and values for each column of
# coviddata_num
datadict <- sapply(
  coviddata,
  function(x) {
    a <- attr(x, "label", exact = TRUE)
    b <- attr(x, "labels", exact = TRUE)
    return(list("Question" = a, "Values" = b))
  }, simplify = F
)

message("./scripts/01_loaddata.R was executed.")
