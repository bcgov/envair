# Script here generates an output file based on the NPRI data
require(dplyr)
#Emissin Sources
df_NPRI_source <- readr::read_csv('../test_data/NPRI-INRP_GeolocationsGeolocalisation_1993-present.csv')
df_NPRI_emissions <- readr::read_csv('../test_data/NPRI-INRP_ReleasesRejets_1993-present_BC.csv')

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
cols_source_select <- c('NPRI_ID','CompanyName','FacilityName','Latitude','Longitude','SectorDescriptionEn')
cols_source_select <- cols_source[cols_source %in% cols_source_select]
df_NPRI_source <- df_NPRI_source %>% select(cols_source_select)

#combine the two data sets
df_NPRI <- df_NPRI_emissions %>%
  left_join(df_NPRI_source) %>%
  dplyr::rename('year' = 'Reporting_Year / Année',
                'parameter' ='Substance Name (English) / Nom de substance (Anglais)',
                'metric' = 'Quantity / Quantité',
                'metric_unit' ='Units / Unités')

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
readr::write_csv(df_NPRI,'../test_data/NPRI.csv')
