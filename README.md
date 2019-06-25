---
output: 
github_document: 
  html_preview: no
  
---


# envair 

## Overview

This is an R packages developed by the BC Ministry of Environment and Climate Change Strategy  Knowledge Management Branch/Environmental and Climate Monitoring Section (ENV/KMB/ECMS) air quality monitoring unit. The package allows retrieval and processing of air quality monitoring data.

## Installation 

You can install `envair` directly from this GitHub repository. To proceed, you will need the [remotes](https://cran.r-project.org/package=remotes) package:


```r
install.packages("remotes")
```

Next, install and load the `envair` package using `remotes::install_github()`:


```r
remotes::install_github("bcgov/envair")
library(envair)
```

## Functions

`GET_STATION_DETAILS_FTP()`  Retrieves station details

`GET_VALID_DATA_PARAMETER()` Retrieves data that has completed ENV's annual validation process. Parameters include PM25, PM10, NO, NO2, NOX, SO2, TRS, H2S, CO, TEMP_MEAN, SNOW, PRECIP, WDIR_VECT, WSPD_VECT, WDIR_UVEC, WSPD_SCLR

`GET_RECENT_DATA_STATION_FTP()` Retrieve the recent 1-month data. Data retrieved from Open Data Portal catalogue of (unverified hourly data)[https://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-unverified-hourly-air-quality-and-meteorological-data]

`RUN_PACKAGE()` Installs and loads (Cran-R)[https://cran.r-project.org/] packages into the library specified in .libPaths()

`GET_VENTING_ECCC()` Retrieve the venting index FLCN39 from Environment and Climate Change Canada datamart
`GET_VENTING_OBSCR()` Creates a kml or shape file based on the proposed (2019) OBSCR rules. This incorporates venting index and sensitivity zones.

`GET_STATISTICS_PARAMETER()` Calculates basic statistics (daily max, percentiles, exceedances) from validated and, if unavailable, unverified data sets.

