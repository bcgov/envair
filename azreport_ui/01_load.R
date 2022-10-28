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

#This file is a required file to initiate server.R
#DEBUG NOTE: fix the metric "pm25_annual". change to "pm25_ann" to be consistent

library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
# library("ggtext")
library("stringr")

library("sf")
library("bcmaps")

library("rcaaqs")
library("envreportutils")

if (!grepl('ui',getwd())) {
  setwd('./azreport_ui')
}

if (0){
  files <- list.files('../R',full.names = TRUE)
  files <- c(files,list.files('../R/shiny',full.names = TRUE))
  for (file in files) {
    try(source(file))
  }
}


source('00_setup.R')
try(source('../R/listBC_stations.R'))
try(source('../R/get_caaqs_stn_history.R'))
try(source('../R/envairfunctions.R'))
try(source('../R/get_emissioninventory.R'))
try(source('../R/shiny/00_shinysetup.R'))


az <- airzones() %>%
  st_make_valid() %>%
  st_transform(st_crs(bc_bound())) %>%
  st_intersection(st_geometry(bc_bound())) %>%
  group_by(airzone = Airzone) %>%
  summarize()

df_parameter <- tribble(
  ~display, ~parameter,
  'PM\u2082.\u2085','pm25',
  'Ozone','o3',
  'NO\u2082','no2',
  'SO\u2082','so2'
)
# Preload from saved files --------
# Generate the files using /R/shiny/04_output.R


aq_summary <-  readr::read_csv(paste(saveDirectory,'caaqs_results.csv',sep='/'))
df_caaqs <- aq_summary
df_apei <- readr::read_csv(paste(saveDirectory,'EN_APEI-Can-Prov_Terr.csv',sep='/'))
lst_stations <- listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE) %>%
  dplyr::rename(label = Label,
                latitude  = LAT,
                longitude = LONG,
                airzone = AIRZONE) %>%
  select(site,label,airzone,latitude,longitude) %>%
  group_by(site) %>%
  slice(1) %>% ungroup()%>%
  filter(!is.na(airzone))



#cleanup
df_caaqs$metric_value <- ifelse(df_caaqs$metric_value<0,NA,df_caaqs$metric_value )

df_preload_management <- readr::read_csv(paste(saveDirectory,'management.csv',sep='/'))


df_management <- get_management_summary(outputtype = 'station',df_preload =  df_preload_management)


df_management_airzone <- get_management_summary(outputtype = 'airzone',df_preload =  df_preload_management) %>%
  select(parameter,airzone,tfee,year,site,label,metric,metric_value,colour_text,colour,colour_order,latitude,longitude)

stationlist <- aq_summary %>%
  pull(site) %>%
  unique() %>%
  sort() %>%
  list()

yearlist <- aq_summary %>%
  pull(year) %>%
  unique() %>%
  sort() %>%
  list() %>%
  unlist()

parameterlist <- aq_summary %>%
  pull(metric) %>%
  unique() %>%
  sort() %>%
  list() %>%
  unlist()
#preparation of the maps

#list of metrics
df_metric <- tribble(
  ~pollutant,~parameter,~metric,~display,
  'PM25','pm2.5_annual','pm25_annual','PM\u2082.\u2085 Annual Metric',
  'PM25','pm2.5_24h','pm25_24h','PM\u2082.\u2085 24-Hour Metric',
  'O3','o3','o3_8h','Ozone 8-Hour Metric',
  'NO2','no2_1yr','no2_ann','NO\u2082 Annual Metric',
  'NO2','no2_3yr','no2_1hr','NO\u2082 1-Hour Metric',
  'SO2','so2_1yr','so2_ann','SO\u2082 Annual Metric',
  'SO2','so2_3yr','so2_1hr','SO\u2082 1-Hour Metric'
)

# List of management levels
labels_mgmt <- rcaaqs::management_levels %>%
  filter(str_detect(parameter, "pm2.5")) %>%
  select(labels, colour, colour_text, units_html) %>%
  distinct() %>%
  mutate(icons = paste0("assets/marker_", colour_text, ".svg"),
         text_colour = c("white", "black", "black", "white", "white"))

#create for map-----

#for guide on map: https://rstudio.github.io/leaflet/showhide.html

#list of stations
df_current_list_all <- df_preload_management %>%
  filter(year == max(df_preload_management$year)) %>%
  select(site,latitude,longitude,parameter,airzone) %>%
  distinct() %>%
  group_by(site) %>%
  dplyr::mutate(parameter = paste0(unique(parameter),collapse = ',')) %>%
  slice(1)%>% ungroup()

#assets
df_current_list <-  df_preload_management %>%
  left_join(df_metric_list() %>%select(-parameter)) %>%
  select(site,tfee,instrument,year,latitude,longitude,pollutant,metric,metric_value,airzone,colour_order,colour,label,colour_text) %>%
  ungroup() %>%
  group_by(site,year,pollutant,tfee) %>%
  arrange(desc(metric),desc(colour_order)) %>%
  dplyr::mutate(mgmt = max(colour_order,na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::filter(mgmt == colour_order) %>%
  group_by(site,year,pollutant,tfee) %>%
  slice(1) %>% select(-mgmt) %>% ungroup() %>%
  left_join(labels_mgmt %>% select(colour_text,labels)) %>%
  mutate(popup = paste(site,'<br>',pollutant,'Management Action:',colour_text,
                       '<br>',labels))

df_current_list_pm25 <- df_current_list %>%
  filter(year == max(df_current_list$year)) %>%
  filter(pollutant == 'PM25') %>%
  filter(!tfee)

df_current_list_pm25_tfee <- df_current_list %>%
  filter(year == max(df_current_list$year)) %>%
  filter(pollutant == 'PM25')%>%
  filter(tfee)

df_current_list_o3 <- df_current_list %>%
  filter(year == max(df_current_list$year)) %>%
  filter(pollutant == 'O3')

df_current_list_no2 <- df_current_list %>%
  filter(year == max(df_current_list$year)) %>%
  filter(pollutant == 'NO2')

df_current_list_so2 <- df_current_list %>%
  filter(year == max(df_current_list$year)) %>%
  filter(pollutant == 'SO2') %>%
  mutate(icon = '../assets/marker_orange01.svg')


#create the airzone background-----
az_mgmt <- airzones() %>%
  st_make_valid() %>%
  st_transform(st_crs(bc_bound())) %>%
  st_intersection(st_geometry(bc_bound())) %>%
  group_by(airzone = Airzone) %>%
  summarize() %>%
  st_transform(4326) %>%
  left_join(df_management_airzone) %>%
  filter(year == 2021)

#create the bar graph of pollutant rank
#use the plot_bar_ranked() function
#statis inputs:
#    df_caaqs_results = df_caaqs,
#    df_stations = lst_stations
#dynamic inputs:
#    metric
#    year
#    airzone

#Create the NPRI Summaries

df_NPRI <- readr::read_csv(paste(saveDirectory,'NPRI.csv',sep='/'))

#Create the woodstove exchange program summaries
# list.files('../test_data')
woodstove_file <- paste(saveDirectory,'2021 report summary Woodstove exchange Totals.xlsx',sep='/')

# readxl::excel_sheets(woodstove_file)
df_woodstove <- readxl::read_excel(woodstove_file,sheet = 'Reported totals',skip =1,n_max = 31)

cols_select_ <- c(1980:2050,'Community','airzone')
cols_select <- colnames(df_woodstove)
cols_select <- cols_select[tolower(cols_select) %in% tolower(cols_select_)]


df_woodstove <- df_woodstove %>%
  select(cols_select) %>%
  tidyr::pivot_longer(cols = -c('Community','airzone')) %>%
  dplyr::rename(year = name)





