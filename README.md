
-   <a href="#bcgovenvair-bc-air-quality-data-retrieval-and-analysis-tool"
    id="toc-bcgovenvair-bc-air-quality-data-retrieval-and-analysis-tool">bcgov/envair:
    BC air quality data retrieval and analysis tool</a>
-   <a href="#bcgovr" id="toc-bcgovr">bcgovr</a>
    -   <a href="#overview" id="toc-overview">Overview</a>
    -   <a href="#installation" id="toc-installation">Installation</a>
    -   <a href="#features" id="toc-features">Features</a>
    -   <a href="#functions" id="toc-functions">Functions</a>
    -   <a href="#usage-and-examples" id="toc-usage-and-examples">Usage and
        Examples</a>

<!--
Copyright 2022 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->
<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->

# bcgov/envair: BC air quality data retrieval and analysis tool

# bcgovr

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

bcgov/envair is an R package developed by the BC Ministry of Environment
and Climate Change Strategy Knowledge Management Branch/Environmental
and Climate Monitoring Section (ENV/KMB/ECMS) air quality monitoring
unit. Package enables R-based retrieval and processing of [air quality
monitoring data](https://envistaweb.env.gov.bc.ca/). Output is now
compatible with the popular [openair
package](https://cran.r-project.org/web/packages/openair/openair.pdf).

## Installation

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

## Features

-   Retrieve data from the Air Quality Data Archive by specifying
    parameter (pollutant) or station. Functions have the option to add
    Transboundary Flow Exceptional Event (TFEE) flags. Data archive is
    located in the ENV’s FTP server:
    <ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/>

-   Generate annual metrics, data captures and statistical summaries
    following the Guidance Document on Achievement Determination and the
    Canadian Ambient Air Quality Standards

-   Most of data processing functions can automatically process a
    parameter as input, or a dataframe of air quality data.

-   Retrieve archived and current ventilation index data with options to
    generate a kml map.

## Functions

-   `importBC_data()` Retrieves station or parameter data from specified
    year/s between 1980 and yesterday.

    -   for you can specify one or several parameters, or name of one or
        several stations
    -   if station is specified, output returns a wide table following
        the format of the openair package. It also renames all columns
        into lowercase letters, changes scalar wind speed to ws, and
        vector wind direction to wd. It also shifts the datetime to the
        time-beginning format
    -   if parameter is specified, output displays data from all air
        quality monitoring stations that reported this data.
    -   use *flag_TFEE = TRUE* to add a new boolean (TRUE/FALSE) column
        called *flag_tfee*. This option only works when you enter
        parameter (not station) in the parameter_or_station.
    -   use merge_Stations = TRUE to merge data from monitoring stations
        and corresponding alternative stations, especially in locations
        where the monitoring station was relocated. This function may
        also change the name of the air quality monitoring station.
    -   set *pad = TRUE* to pad missing dates, set *use_ws_vector =
        TRUE* to use vector wind speed instead of scalar, and set
        *use_openairformat = FALSE* to produce the original
        *non-openair* output.

-   `importBC_data_avg()` Retrieves pollutant (parameter) data and
    performs statistical averaging based on the specified averaging_type

    -   function can retrieve 24-hour averages (24-hr), daily 1-hour
        maximum (d1hm), daily 8-hour maximum (d8hm), rolling 8-hour
        values (8-hr)
    -   it can also make annual summaries such as 98th percentile of
        daily 1-hour maximum, annual mean of 24-hour values. To perform
        annual summaries, the averaging_type should include “annual
        \<averaging/percentile\> \<1-hour or 24-hour or dxhm\>”
        -   function can calculate the number of times a certain value
            has been exceeded

            <table>
            <caption>List of possible values for the
            <em>averaging_type</em>.</caption>
            <colgroup>
            <col style="width: 29%" />
            <col style="width: 20%" />
            <col style="width: 50%" />
            </colgroup>
            <thead>
            <tr class="header">
            <th>Type of averaging</th>
            <th><em>averaging_type=</em> Syntax</th>
            <th>Output description</th>
            </tr>
            </thead>
            <tbody>
            <tr class="odd">
            <td>1-hr</td>
            <td>“1-hr”</td>
            <td>Outputs hourly data. No averaging done.</td>
            </tr>
            <tr class="even">
            <td>Daily Average</td>
            <td>“24-hr”</td>
            <td>Outputs the daily (24-hour) values.</td>
            </tr>
            <tr class="odd">
            <td>Rolling 8-hour</td>
            <td>“8-hr”</td>
            <td>Outputs hourly values that were calculated from rolling 8-hour
            average.</td>
            </tr>
            <tr class="even">
            <td>Daily 1-hour maximum</td>
            <td>“d1hm”</td>
            <td>Outputs daily values of the highest 1-hour concentration</td>
            </tr>
            <tr class="odd">
            <td>Daily 8-hour maximum</td>
            <td>“d8hm”</td>
            <td>Outputs the daily 8-hour maximum for each day</td>
            </tr>
            <tr class="even">
            <td>Annual mean of 1-hour values</td>
            <td>“annual mean 1-hr”</td>
            <td>Outputs the average of all hourly values</td>
            </tr>
            <tr class="odd">
            <td>Annual mean of daily values</td>
            <td><p>“annual mean 24-hr”</p>
            <p>“annual mean &lt;avging&gt;”</p></td>
            <td>Outputs average of all daily values.</td>
            </tr>
            <tr class="even">
            <td>Annual 98th percentile of 1-hour values</td>
            <td><p>“annual 98p 1-hr”</p>
            <p>“annual &lt;xxp&gt; &lt;avging&gt;</p></td>
            <td>Outputs the 98th percentile of the 1-hour values.</td>
            </tr>
            <tr class="odd">
            <td>4th Highest daily 8-hour maximum</td>
            <td><p>“annual 4th d8hm”</p>
            <p>“annual &lt;rank&gt; &lt;avging&gt;</p></td>
            <td>Outputs the 4th highest daily 8-hour maximum.</td>
            </tr>
            <tr class="even">
            <td>Number of daily values exceeding 28 µg/m3</td>
            <td><p>“exceed 28 24-hr”</p>
            <p>“exceed &lt;value&gt; &lt;avging&gt;</p></td>
            <td>Outputs the number of days where the 28 µg/m3 is exceeded</td>
            </tr>
            <tr class="odd">
            <td>Number of exceedance to d8hm of 62ppb</td>
            <td><p>“exceed 62 d8hm”</p>
            <p>“exceed &lt;value&gt; &lt;avging&gt;</p></td>
            <td>Outputs the number of days where the daily 8-hour maximum exceeds 62
            ppb</td>
            </tr>
            </tbody>
            </table>

            List of possible values for the *averaging_type*.

-   `get_stats()` Retrieves a statistical summary based on the default
    for the pollutant. Currently applies to PM2.5, O3, NO2, and SO2.
    Output includes data captures, annual metrics, and exceedances.

-   `get_captures()` Calculates the data captures for a specified
    pollutant or dataframe. Output is a long list of capture statistics
    such as hourly, daily, quarterly, and annual sumamaries.

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

## Usage and Examples

#### `importBC_data()`

------------------------------------------------------------------------

##### Retrieving air quality data, include TFEE adjustment and combine related stations

> Use flag_TFEE = TRUE an merge_Stations = TRUE to produce a result that
> defines the TFEE and merges station and instruments, as performed
> during the CAAQS-reporting process.

``` r

library(envair)
df_data <- importBC_data('pm25',years = 2015:2017, flag_TFEE = TRUE,merge_Stations = TRUE)

knitr::kable(df_data[1:4,])
```

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

#### `importBC_data_avg()`

------------------------------------------------------------------------

##### Retrieving the annual average of daily values for multiple parameters

> The function is capable of processing multiple parameters, and
> multiple years, but it can only do one averaging type. The averaging
> type can be a simple averaging (e.g., 24-hour, 8-hour, or combined
> averaging (e.g., annual 98p d1hm, annual mean 24-hour). Check the
> table above for a comprehensive list of averaging_type.
>
> ``` r
> #user can specify the parameter, enter parameter name as input
> annual_mean <- importBC_data_avg(c('pm25','o3'), years = 2015:2018, averaging_type = 'annual mean 24-hr')
>
> #or if you already have a dataframe, you can use the dataframe as input to for the statistical summary
> df_input <- importBC_data(param = c('pm25','o3'), years = 2015:2018)
> annual_mean <- importBC_data_avg(df_input,averaging_type = 'annual mean 24-hr')
> ```

#### `get_stats()`

------------------------------------------------------------------------

##### Calculate the annual metrics of PM2.5

> The function will calculate statistical summaries , data captures, and
> number of exceedances based on the CAAQS metrics and values. The
> function will only perform a year-by-year calculation so the results
> are not the actual CAAQS metrics, but can be used to derive it. For
> ozone, it creates Q2 + Q3
>
> ``` r
> #example retrieves stat summaries
> stats_result <- get_stats(param = 'o3', years = 2016,add_TFEE = TRUE, merge_Stations = TRUE)
> ```

#### `get_captures()`

------------------------------------------------------------------------

##### Calculate the data captures of PM2.5

> The function will create a summary of data captures for the parameter
> or for dataframe. You can specify the parameter or, if available, use
> an air quality dataframe as input.
>
> ``` r
> #you can use the parameter as input
> data_captures <- get_captures(param = c('pm25','o3'), years = 2015:2018,merge_Stations = TRUE)
>
> #or you can use a dataframe 
> air_data <- importBC_data(c('pm25','o3'), years = 2015:201,merge_Stations = TRUE)
> data_captures <- get_captures(param = air_data, years = 2015:2018)
> ```

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
