# COVID-19 Survey Analysis

***Prepared by Nathan Chan***

## Overview

This R project produces an analysis of self-reported web-based survey data from the [COVID-19 Behaviour Determinants Database](https://doi.org/10.7910/DVN/NILCAV). It is intended to serve as a code demonstration for several advanced statistical methods, including:

  * *multiple imputation* via [MICE](https://cran.r-project.org/web/packages/mice/mice.pdf), to impute missing data of arbitrary type (continuous, ordinal, or categorical);
  * *outlier analysis* via [OuRS](https://github.com/derekbeaton/OuRS), to identify extreme observations and problematic variables;
  * *generalized partial least squares correspondence analysis* (PLSCA) via [TExPosition](https://github.com/derekbeaton/ExPosition1/tree/master/TExPosition), to characterize the strongest (multivariate) associations between groups of related variables and all other variables in the databse; and
  * *visualizations* of multivariate associations, to aid with interpretation of PLSCA results.
  
This project makes extensive use of R packages created by [Derek Beaton](https://github.com/derekbeaton), especially for OuRS and TExPosition. *Derek* - thank you for the time and dedication you took to developing these tools and enabling multivariate mixed data analyses!

## Description

The database includes responses from 8070 participants located in the most populous U.S. states (New York, California, Florida, and Texas) and Canadian provinces (excluding Quebec). It was collected at three time points: May 2020, July 2020, and March 2021.

Please note that this project is currently under development.

The project contains several parts:

1. an **R Markdown document** (under construction), to guide readers through the project's analysis pipeline and interpretations; and
2. a series of **R scripts**, to perform the analysis and generate figures.
