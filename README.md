
-   <a href="#bcgovenvair-bc-air-quality-data-retrieval-and-analysis-tool"
    id="toc-bcgovenvair-bc-air-quality-data-retrieval-and-analysis-tool">bcgov/envair:
    BC air quality data retrieval and analysis tool</a>
-   <a href="#bcgovr-" id="toc-bcgovr-">bcgovr
    <img src="tools/readme/logo.png" align="right" /></a>
-   <a href="#overview" id="toc-overview">Overview</a>
-   <a href="#installation" id="toc-installation">Installation</a>
-   <a href="#functions" id="toc-functions">Functions</a>
-   <a href="#usage-and-examples" id="toc-usage-and-examples">Usage and
    Examples</a>

<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->

# bcgov/envair: BC air quality data retrieval and analysis tool

# bcgovr <img src="tools/readme/logo.png" align="right" />

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# Overview

bcgov/envair is an R package developed by the BC Ministry of Environment
and Climate Change Strategy Knowledge Management Branch/Environmental
and Climate Monitoring Section (ENV/KMB/ECMS) air quality monitoring
unit. Package enables R-based retrieval and processing of [air quality
monitoring data](https://envistaweb.env.gov.bc.ca/). Output is now
compatible with the popular [openair
package](https://cran.r-project.org/web/packages/openair/openair.pdf).

# Installation

You can install `envair` directly from this GitHub repository. To
proceed, you will need the
[remotes](https://cran.r-project.org/package=remotes) package:

``` r
install.packages("remotes")
```

Next, install and load the `envair` package using
`remotes::install_github()`:

``` r
remotes::install_github("bcgov/envair")
library(envair)
```

# Functions

-   `importBC_data()` Retrieves station or parameter data from specified
    year/s between 1980 and yesterday.

    -   if station is specified, output includes all parameters from
        that station. Output is also a compatible input to the openair
        package functions with *scalar wind speed* and *vector wind
        direction* as default ws, wd openair feed. The date is also
        shifted to the time-beginning format.
    -   if parameter is specified, output displays data from all air
        quality monitoring stations that reported this data.
    -   set *pad = TRUE* to pad missing dates, set *use_ws_vector =
        TRUE* to use vector wind speed instead of scalar, and set
        *use_openairformat = FALSE* to produce the original
        *non-openair* output.

-   `listBC_stations()` Lists details of all air quality monitoring
    stations (active or inactive)

-   `list_parameters()` Lists the parameters that can be imported by
    `importBC_data()`

-   `importECCC_forecast()` Retrieves AQHI, PM2.5, PM10, O3, and NO2
    forecasts from the ECCC datamart

-   `GET_VENTING_ECCC()` Retrieve the venting index FLCN39 from
    Environment and Climate Change Canada datamart

-   `ventingBC_kml()` Creates a kml or shape file based on the 2019
    OBSCR rules. This incorporates venting index and sensitivity zones.

# Usage and Examples

#### `importBC_data()`

------------------------------------------------------------------------

##### Using *openair* package function on BC ENV data.

> By default, this function produces openair-compatible dataframe
> output. This renames *WSPD_VECT*,*WDIR_VECT* into *ws* and *wd*,
> changes pollutant names to lower case characters (e.g.,
> *pm25*,*no2*,*so2*), and shifts the date from time-ending to
> time-beginning format. To use, specify station name and year/s. For a
> list of stations, use *listBC_stations()* function. If no year is
> specified, function retrieves latest data, typically the unverfied
> data from start of year to current date.

``` r
library(openair)
PG_data <- importBC_data('Prince George Plaza 400',2010:2012)
pollutionRose(PG_data,pollutant='pm25')
```

![](importBC_data.png)<!-- -->

##### Other features for station data retrieval

-   To import without renaming column names, specify *use_openairformat
    = FALSE*. This also keeps date in time-ending format
-   By default, *vector wind direction* and *scalar wind speeds* are
    used
-   To use vector wind speed, use *use_ws_vector = TRUE*
-   To pad missing dates, set *pad = TRUE*
-   Station name is not case sensitive, and works on partial text match
-   Multiple stations can be specified *c(‘Prince George’,‘Kamloops’)*
-   For non-continuous multiple years, use *c(2010,2011:2014)*

``` r
importBC_data('Prince George Plaza 400',2010:2012,use_openairformat = FALSE)
importBC_data('Kamloops',2015)
importBC_data(c('Prince George','Kamloops'),c(2010,2011:2014))
importBC_data('Trail',2015,pad = TRUE)              
```

##### Retrieve parameter data

> Specify parameter name to retrieve data. Note that these are very
> large files and may use up your computer’s resources. List of
> parameters can be found using the *list_parameters()* function.

``` r
pm25_3year <- importBC_data('PM25',2010:2012)
```

#### `listBC_stations()`

------------------------------------------------------------------------

> produces a dataframe that lists all air quality monitoring station
> details. if year is specified, it retrieves the station details from
> that year. Note that this entry may not be accurate since system has
> not been in place to generate these station details.

``` r
listBC_stations()
listBC_stations(2016)
```

| SERIAL_CODE | EMS_ID  | STATION_NAME                     | STATION_NAME_FULL                | LOCATION            | CITY       | CATEGORY                        | STATION_ENVIRONMENT | STATION_OWNER | DATE_ESTABLISHED     | NOTES                       | LATITUDE | LONGITUDE | HEIGHT.m. | STATUS   | CGNDB |
|------------:|:--------|:---------------------------------|:---------------------------------|:--------------------|:-----------|:--------------------------------|:--------------------|:--------------|:---------------------|:----------------------------|:---------|:----------|----------:|:---------|:------|
|         428 | E289309 | Abbotsford A Columbia Street     | Abbotsford A Columbia Street     | N/A                 | Abbotsford | METRO VANCOUVER                 | N/A                 | MVRD          | 7/25/2012            | GVRD T045                   | 49.0215  | -122.3266 |        65 | ACTIVE   | N/A   |
|         429 | E289309 | Abbotsford A Columbia Street     | Abbotsford A Columbia Street Met | N/A                 | Abbotsford | METRO VANCOUVER                 | N/A                 | MVRD          | 7/25/2012 6:28:41 AM | GVRD T045                   | 49.0215  | -122.3266 |        65 | ACTIVE   | N/A   |
|         306 | 0310081 | Abbotsford Airport               | Abbotsford Airport               | 2nd Avenue          | Abbotsford | NON OPERATIONAL                 | Commercial          | MVRD          | 1/7/1978             | GVRD T011;Closed 1994-04-28 | 49.0306  | -122.3761 |        40 | INACTIVE | N/A   |
|         262 | E246240 | Abbotsford Airport Walmsley Road | Abbotsford Airport Walmsley Road | 31790 Walmsley Road | Aldergrove | METRO VANCOUVER NON OPERATIONAL | Commercial          | MVRD          | 5/1/2001             | GVRD T034                   | 49.0235  | -122.3430 |        65 | INACTIVE | N/A   |

#### `list_parameters()`

------------------------------------------------------------------------

> produce a vector string of available parameters that can be retrieved
> with *importBC_data()*

#### `GET_VENTING_ECCC()`

------------------------------------------------------------------------

> produces a dataframe containing the recent venting index. Optional
> entry of date string retrieves venting from a particular date.
> Archived data is dependent on ECCC’s data availability

``` r
GET_VENTING_ECCC()
GET_VENTING_ECCC('2019-11-08')
```

| VENTING_INDEX_ABBREV | DATE_ISSUED | CURRENT_VI | CURRENT_VI_DESC | CURRENT_WSPD | CURRENT_MIX_HEIGHT | TODAY_VI | TODAY_VI_DESC | TODAY_WSPD | TODAY_MIX_HEIGHT | TOMORROW_VI | TOMORROW_VI_DESC | TOMORROW_WSPD | TOMORROW_MIX_HEIGHT | NAME           | REGION           |      LAT |      LONG |
|:---------------------|:------------|-----------:|:----------------|-------------:|-------------------:|---------:|:--------------|-----------:|-----------------:|------------:|:-----------------|--------------:|--------------------:|:---------------|:-----------------|---------:|----------:|
| 100 MILE             | 2019-11-08  |         23 | POOR            |           20 |               1072 |       21 | POOR          |         23 |             1040 |          23 | POOR             |            11 |                1183 | 100 Mile House | CENTRAL INTERIOR | 51.63915 | -121.2945 |
| ATLIN                | 2019-11-08  |         16 | POOR            |            3 |               1169 |       36 | FAIR          |         23 |              966 |          34 | FAIR             |            16 |                1050 | Atlin          | NORTHERN BC      | 59.57000 | -133.7000 |
| BELLA COOLA          | 2019-11-08  |          9 | POOR            |            5 |                 55 |       16 | POOR          |         14 |              134 |          22 | POOR             |             8 |                 345 | Bella Coola    | COAST            | 52.38000 | -126.7500 |
| BURNS LAKE           | 2019-11-08  |         20 | POOR            |           17 |                833 |       26 | POOR          |         16 |              915 |          19 | POOR             |            19 |                 815 | Burns Lake     | CENTRAL INTERIOR | 54.23142 | -125.7597 |

#### `importECCC_forecast()`

------------------------------------------------------------------------

-   Retrieves forecasts and model data from ECCC
-   parameters include AQHI, PM25, NO2, O3, PM10

``` r
importECCC_forecast('no2')
```

#### `ventingBC_kml()`

------------------------------------------------------------------------

-   creates a kml object based on the 2019 OBSCR rules
-   directory to save kml file can be specified. File will be saved in
    that directory as *Venting_Index_HD.kml*.

``` r
ventingBC_kml()
ventingBC_kml('C:/temp/')
```
