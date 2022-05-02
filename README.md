# COVID-19 Survey Analysis

***Prepared by Nathan Chan***

## Overview

This R project produces an analysis of self-reported web-based survey data from the [COVID-19 Behaviour Determinants Database](https://doi.org/10.7910/DVN/NILCAV). It is intended to serve as a code demonstration for several advanced statistical methods, including:

  * *multiple imputation* via [MICE](https://cran.r-project.org/web/packages/mice/mice.pdf), to impute missing data of arbitrary type (continuous, ordinal, or categorical);
  * *outlier analysis* via [OuRS](https://github.com/derekbeaton/OuRS), to identify extreme observations and problematic variables;
  * *generalized partial least squares correspondence analysis* (PLSCA) via [TExPosition](https://github.com/derekbeaton/ExPosition1/tree/master/TExPosition), to characterize the strongest (multivariate) associations between groups of related variables and all other variables in the database; and
  * *visualizations* of multivariate associations, to aid with interpretation of PLSCA results.
  
This project makes extensive use of R packages created by [Derek Beaton](https://github.com/derekbeaton), especially for OuRS and TExPosition. *Derek* - thank you for the time and dedication you took to developing these tools and enabling multivariate mixed data analyses!

Data used in the preparation of this project was obtained from the [COVID-19 Behavior Determinants Database](www.covid19-database.com). Data collection and sharing of this database was funded by the Centre for Addiction and Mental Health (CAMH) Foundation.

## Description

The database includes responses from 8070 participants located in the most populous U.S. states (New York, California, Florida, and Texas) and Canadian provinces (excluding Quebec). It was collected at three time points: May 2020, July 2020, and March 2021.

Please note that this project is currently under development.

The project contains several parts:

1. a series of **R Markdown documents**, to guide readers through the project's analysis pipeline and interpretations, including:

  1. [**Part 1: Introduction**](https://nathankchan.github.io/covid-19-survey-analysis/Analysis_part1.html)
  2. [**Part 2: Data Extraction & Preprocessing**](https://nathankchan.github.io/covid-19-survey-analysis/Analysis_part2.html)
  3. [**Part 3: Outlier Analysis**](https://nathankchan.github.io/covid-19-survey-analysis/Analysis_part3.html) (*under construction*)
  4. [**Part 4: Inferential Analysis**](https://nathankchan.github.io/covid-19-survey-analysis/Analysis_part4.html) (*under construction*)

2. a series of **R scripts**, to perform the analysis and generate figures.
