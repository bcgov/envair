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

# The scripts here generates csv files
# that will be used as input for the shiny and web apps
# RUN only if:
#      - data content needs to be updated, such as, after completing Level 2 data validation
#      - if the test data folder is missing or has no data content


# set the working and save directory-----
# multiple attempts to ensuer it will be at './R'
saveDirectory <- '../data/out'
years <- 2015:2021

try(setwd('../r/'),silent = FALSE)
try(setwd('./r/'),silent = FALSE)


print(getwd())

dir.create(saveDirectory,recursive = TRUE)
# define where data files will be saved

#load sources----
source('./shiny/00_shinysetup.R')

for (file_ in list.files()) {
  try(source(file_))
}

#Create NPRI data----
fileNPRI <- paste(saveDirectory,'NPRI.csv',sep='/')
df_NPRI <- get_npri()

df_NPRI %>%
  filter(province == 'BC') %>%
  readr::write_csv(fileNPRI)

#save APEI data----
download.file(url = 'https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-air-pollutant-emissions-inventory/EN_APEI-Can-Prov_Terr.csv',
              destfile = paste(saveDirectory,'EN_APEI-Can-Prov_Terr.csv',sep='/')
)


# Create the caaqs values----
# Calculations are based on the rcaaqs package
# using envair for data retrieval
# this generates the file: caaqs_results.csv
# this function output will append on itself, so years can be operated in chunks
create_caaqs_annual(years = years, savedirectory = saveDirectory)

# Create management levels summary-----
df_management_summary <- get_management_summary(datafile = paste(saveDirectory,'management.csv',sep='/'))
readr::write_csv(df_management_summary,paste(saveDirectory,'management.csv',sep='/'))

# OPTIONAL: Create annual metrics file----
# Calculations for YOY metrics are based on the envair package
# this generates the file: annual_results.csv
create_metrics_annual(years = years,savedirectory = saveDirectory)

