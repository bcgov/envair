
<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->

# bcgov/envair: BC air quality data retrieval and analysis tool

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

  - `importBC_data()` Retrieves station or parameter data from specified
    year/s between 1980 and yesterday.
    
      - if station is specified, output includes all parameters from
        that station. Output is also a compatible input to the openair
        package functions.
    
      - if parameter is specified, output displays data from all air
        quality monitoring stations that reported this data.

  - `listBC_stations()` List details of all air quality monitoring
    stations (active or inactive)

  - `importECCC_forecast()` Retrieves AQHI, PM2.5, PM10, O3, and NO2
    forecasts from the ECCC datamart

  - `GET_VENTING_ECCC()` Retrieve the venting index FLCN39 from
    Environment and Climate Change Canada datamart

  - `GET_VENTING_OBSCR()` Creates a kml or shape file based on the 2019
    OBSCR rules. This incorporates venting index and sensitivity zones.

# Usage and Examples

#### `importBC_data()`

-----

##### Using *openair* package function on BC ENV data.

> By default, this function produces openair-compatible dataframe
> output. This renames *WSPD\_VECT*,*WDIR\_VECT* into *ws* and *wd*,
> changes pollutant names to lower case characters (e.g.,
> *pm25*,*no2*,*so2*). To use, specify station name and year/s. For a
> list of stations, use *listBC\_stations()* function. If no year is
> specified, function retrieves latest data, typically the unverfied
> data from start of year to current date.

``` r
library(openair)
PG_data <- importBC_data('Prince George Plaza 400',2010:2012)
pollutionRose(PG_data,pollutant='pm25')
```

![](importBC_data.png)<!-- -->

##### Importing without column name modification

> To import without renaming column names, specify *use\_openairformat =
> FALSE*.

``` r
PG_data <- importBC_data('Prince George Plaza 400',2010:2012,use_openairformat = FALSE)
```

##### Retrieve parameter data

> Specify parameter name to retrieve data. Note that these are very
> large files and may use up your computer’s resources. List of
> parameters can be found using the *list\_parameters()* function.

``` r
pm25_3year <- importBC_data('PM25',2010:2012)
```

#### `listBC_stations()`

-----

> produces a dataframe that lists all air quality monitoring station
> details. if year is specified, it retrieves the station details from
> that year. Note that this entry may not be accurate since system has
> not been in place to generate these station
details.

``` r
stations <- listBC_stations()
```

| SERIAL\_CODE | EMS\_ID | STATION\_NAME                    | STATION\_NAME\_FULL              | LOCATION            | CITY       | CATEGORY                        | STATION\_ENVIRONMENT | STATION\_OWNER | DATE\_ESTABLISHED    | NOTES                       | LATITUDE | LONGITUDE  | HEIGHT.m. | STATUS   | CGNDB |
| -----------: | :------ | :------------------------------- | :------------------------------- | :------------------ | :--------- | :------------------------------ | :------------------- | :------------- | :------------------- | :-------------------------- | :------- | :--------- | --------: | :------- | :---- |
|          428 | E289309 | Abbotsford A Columbia Street     | Abbotsford A Columbia Street     | N/A                 | Abbotsford | METRO VANCOUVER                 | N/A                  | MVRD           | 7/25/2012            | GVRD T045                   | 49.0215  | \-122.3266 |        65 | ACTIVE   | N/A   |
|          429 | E289309 | Abbotsford A Columbia Street     | Abbotsford A Columbia Street Met | N/A                 | Abbotsford | METRO VANCOUVER                 | N/A                  | MVRD           | 7/25/2012 6:28:41 AM | GVRD T045                   | 49.0215  | \-122.3266 |        65 | ACTIVE   | N/A   |
|          306 | 0310081 | Abbotsford Airport               | Abbotsford Airport               | 2nd Avenue          | Abbotsford | NON OPERATIONAL                 | Commercial           | MVRD           | 1/7/1978             | GVRD T011;Closed 1994-04-28 | 49.0306  | \-122.3761 |        40 | INACTIVE | N/A   |
|          262 | E246240 | Abbotsford Airport Walmsley Road | Abbotsford Airport Walmsley Road | 31790 Walmsley Road | Aldergrove | METRO VANCOUVER NON OPERATIONAL | Commercial           | MVRD           | 5/1/2001             | GVRD T034                   | 49.0235  | \-122.3430 |        65 | INACTIVE | N/A   |

#### `list_parameters()`

-----

> produce a vector string of available parameters that can be retrieved
> with *importBC\_data()*
