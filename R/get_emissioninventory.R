# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



#' Retrieve the air pollutant emission inventory from ECCC datamart
#'
#' Files are retrieved from ECCC APRI site
#'
#' @param categorytype is either source, sector, or subsector. Default is source
#' @param pollutant is the pollutant to retrieve. If NULL, it will retrieve all pollutants
#' @param df is the dataframe containg the data from source. added to expedite for the shiny app version.
#' @param URL the ECCC URL for the NPRI. Can be downloaded file in local directory or in ECCC data mart.
#' At writing, it datamart location is at:
#' 'https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-air-pollutant-emissions-inventory/EN_APEI-Can-Prov_Terr.csv'
#'
#' @export
get_apei <- function(categorytype = 'Source',pollutant=NULL,df=NULL,URL=NULL) {
  if (0) {
    pollutant <- NULL
    df <- NULL
    categorytype <- 'Source'
    URL <- NULL
    URL <- '././test_data/EN_APEI-Can-Prov_Terr.csv'
  }
  if (is.null(URL)) {
    URL <- 'https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-air-pollutant-emissions-inventory/EN_APEI-Can-Prov_Terr.csv'
  }

  #These will be the static  portion of the data, to be included in all queries
  cols_include <- c('Region','Source','Sector','Subsector','Year')

  #retrieve data from ECCC
  if (is.null(df)) {
    df_apei <- readr::read_csv(URL)
  }
  #retrieve the columns related relevant to the query
  cols <- colnames(df_apei)
  cols_include <- cols_include[tolower(cols_include) %in% tolower(cols)]

  #find colnames that are pollutants
  #note that these are colnames that have parentheses
  lst_pollutants <- cols[grepl(pattern = '\\(',cols)]

  #select the pollutant
  if (!is.null(pollutant)) {
    pollutant_select <- lst_pollutants[grepl(pollutant,lst_pollutants,ignore.case=TRUE)]
  } else {
    pollutant_select <- lst_pollutants
  }

  df_apei <- df_apei %>%
    tidyr::pivot_longer(cols = lst_pollutants) %>%
    dplyr::rename(pollutant = name)

  #retrieve source, sector, subsectors
  lst_source <- df_apei %>%
    pull(Source) %>% unique()
  lst_sector <- df_apei %>%
    pull(Sector) %>% unique()
  lst_subsector <- df_apei %>%
    pull(Subsector) %>% unique()

  #filter according to user preference
  df_apei <- df_apei %>%
    filter(Region == 'BC') %>%
    filter(pollutant == pollutant_select) %>%
    filter(!is.na(value))

  #retrieve the values based on the category type
  if (tolower(categorytype) == 'source') {
    df_apei <- df_apei %>%
      filter(is.na(Sector),is.na(Subsector))
  }
  if (tolower(categorytype) == 'sector') {
    df_apei <- df_apei %>%
      filter(!is.na(Sector),is.na(Subsector))
  }
  if (tolower(categorytype) == 'subsector') {
    df_apei <- df_apei %>%
      filter(!is.na(Subsector))
  }


  df_apei <- df_apei %>%
    filter(Source != 'GRAND TOTAL')

  return(df_apei)
}


#' Retrieve NPRI Emissions and Sources from ECCC Datamart
#'
#' @description  Script here generates an output file based on the NPRI data----
#' NPRI Data is owned and maintained by Environment and Climate Change Canada
#' If the URL is incorrect, please refer to:
#' https://open.canada.ca/data/en/dataset/40e01423-7728-429c-ac9d-2954385ccdfb
#'
#' @param URL_emissions is the ECCC datamart download link for the NPRI release csv file
#' @param URL_sources is the ECCC datamart download link for the NPRI Emission sources
#'
#' @export
get_npri <- function(URL_emissions = NULL, URL_sources = NULL) {


  if (0) {
    URL_emissions <- NULL
    URL_sources <- NULL
  }
  require(dplyr)
  #Emission Sources
  #
  # df_NPRI_source <- readr::read_csv('../test_data/NPRI-INRP_GeolocationsGeolocalisation_1993-present.csv')
  # df_NPRI_emissions <- readr::read_csv('../test_data/NPRI-INRP_ReleasesRejets_1993-present_BC.csv')

  if (is.null(URL_emissions)) {
    URL_emissions <- 'https://data-donnees.ec.gc.ca/data/substances/plansreports/reporting-facilities-pollutant-release-and-transfer-data/bulk-data-files-for-all-years-releases-disposals-transfers-and-facility-locations/NPRI-INRP_ReleasesRejets_1993-present.csv'
  }

  if (is.null(URL_sources)) {
    URL_sources <- 'https://data-donnees.ec.gc.ca/data/substances/plansreports/reporting-facilities-pollutant-release-and-transfer-data/bulk-data-files-for-all-years-releases-disposals-transfers-and-facility-locations/NPRI-INRP_GeolocationsGeolocalisation_1993-present.csv'
  }

  df_NPRI_emissions <- readr::read_csv(URL_emissions,
                                       locale = readr::locale(encoding = "windows-1252"))

  df_NPRI_source <- readr::read_csv(URL_sources,
                                    locale = readr::locale(encoding = "windows-1252"))
  #common ID
  index_column <- c('NPRI_ID / No_INRP','NpriID')
  cols_emission <- colnames(df_NPRI_emissions)
  cols_source <- colnames(df_NPRI_source)

  #rename index_column to "NPRI_ID"
  cols_emission_index <- cols_emission[tolower(cols_emission) %in% tolower(index_column)]
  cols_source_index <- cols_source[tolower(cols_source) %in% tolower(index_column)]

  df_NPRI_emissions <- df_NPRI_emissions %>%
    dplyr::rename('NPRI_ID' =cols_emission_index)

  df_NPRI_source <- df_NPRI_source %>%
    dplyr::rename('NPRI_ID' =cols_source_index)

  #define the selected column names
  cols_emission <- colnames(df_NPRI_emissions)
  cols_emission_select <- c('NPRI_ID','Reporting_Year / Année','Substance Name (English) / Nom de substance (Anglais)',
                            'Quantity / Quantité','Units / Unités')
  cols_emission_select <- cols_emission[cols_emission %in% cols_emission_select]
  df_NPRI_emissions <- df_NPRI_emissions %>% select(cols_emission_select)

  cols_source <- colnames(df_NPRI_source)
  cols_source_select <- c('NPRI_ID','CompanyName','FacilityName','Latitude','Longitude',
                          'SectorDescriptionEn','ProvinceCode')
  cols_source_select <- cols_source[cols_source %in% cols_source_select]
  df_NPRI_source <- df_NPRI_source %>% select(cols_source_select)

  #combine the two data sets
  df_NPRI <- df_NPRI_emissions %>%
    left_join(df_NPRI_source) %>%
    dplyr::rename('year' = 'Reporting_Year / Année',
                  'parameter' ='Substance Name (English) / Nom de substance (Anglais)',
                  'metric' = 'Quantity / Quantité',
                  'metric_unit' ='Units / Unités',
                  'province' = 'ProvinceCode')

  #standardize parameters

  parameters <- unique(df_NPRI$parameter)
  param_pm25 <- parameters[grepl('particulate matter',parameters,ignore.case = TRUE)][[1]]
  param_nox <- parameters[grepl('nitrogen',parameters,ignore.case = TRUE)]
  param_so2 <- parameters[grepl('sulphur',parameters,ignore.case = TRUE)]

  df_NPRI <- df_NPRI %>%
    dplyr::mutate(parameter = dplyr::recode(parameter,  'PM2.5 - Particulate Matter <= 2.5 Micrometers' = 'pm25',
                                            'Nitrogen oxides (expressed as nitrogen dioxide)'= 'no2',
                                            'Sulphur dioxide' = 'so2'))

  #get the air zone
  df_NPRI <- df_NPRI %>%
    dplyr::rename( lat = Latitude, lon = Longitude, ems_id = NPRI_ID) %>%
    dplyr::filter(!is.na(lat),!is.na(lon),abs(lat)<=90) %>%
    rcaaqs::assign_airzone(bcmaps::airzones()) %>%
    dplyr::rename( latitude = lat , longitude = lon, NPRI_ID = ems_id,
                   sector = SectorDescriptionEn)


  return(df_NPRI)
}
