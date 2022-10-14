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

library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
# library("ggtext")
library("stringr")

library("sf")
library("bcmaps")
library(envair)
library("rcaaqs")
library("envreportutils")

source('00_setup.R')
az <- airzones() %>%
  st_make_valid() %>%
  st_transform(st_crs(bc_bound())) %>%
  st_intersection(st_geometry(bc_bound())) %>%
  group_by(airzone = Airzone) %>%
  summarize()

# Preload from saved files --------
# Generate the files using /R/shiny/04_output.R
aq_summary <-  readr::read_csv('../test_data/caaqs_results.csv')
df_npri <- readr::read_csv('../test_data/EN_APEI-Can-Prov_Terr.csv')
lst_stations <- listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE)
df_caaqs <- readr::read_csv('../test_data/caaqs_results.csv')

df_preload_management <- readr::read_csv('../test_data/management.csv')
df_management <- get_management_summary(outputtype = 'station',df_preload =  df_preload_management)
df_management_airzone <- get_management_summary(outputtype = 'airzone',df_preload =  df_preload_management)

#preparation of the maps

#list of metrics
df_metric <- tribble(
  ~pollutant,~parameter,~metric,
  'PM25','pm2.5_annual','pm25_annual',
  'O3','o3','o3_8h',
  'PM25','pm2.5_24h','pm25_24h',
  'NO2','no2_1yr','no2_ann',
  'NO2','no2_3yr','no2_1hr',
  'SO2','so2_1yr','so2_ann',
  'SO2','so2_3yr','so2_1hr'
)

# List of management levels
labels_mgmt <- rcaaqs::management_levels %>%
  filter(str_detect(parameter, "pm2.5")) %>%
  select(labels, colour, colour_text, units_html) %>%
  distinct() %>%
  mutate(icons = paste0("assets/marker_", colour_text, ".svg"),
         text_colour = c("white", "black", "black", "white", "white"))
