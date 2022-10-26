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

#' Calculate the management levels
#'   NOTE: needs future management, change from datafile to an actual dataframe entry
#'
#' @param datafile is the location of the file containing summarized CAAQS data.
#' This dataset was created with the create_metrics_annual function
#'
get_management <- function(datafile = NULL) {

  if (0) {
    datafile <- NULL
  }
  #retrieve data

  if (is.null(datafile)) {
    datafile <- '././test_data/caaqs_results.csv'
    # list.files(datafile)
  }

  df <- readr::read_csv(datafile) %>%
    dplyr::mutate(idx0 = 1:n())

  df_levels <- rcaaqs::management_levels %>%
    dplyr::rename(metric = parameter) %>%
    dplyr::mutate(idx1 = 1:n()) %>%
    mutate(metric = recode(metric,
                           'o3' = 'o3_8h',
                           'pm2.5_annual' = 'pm25_annual',
                           'pm2.5_24h' = 'pm25_24h',
                           'no2_1yr' = 'no2_ann',
                           'no2_3yr' = 'no2_1hr',
                           'so2_1yr' = 'so2_ann',
                           'so2_3yr' = 'so2_1hr'))

  df_levels_ <- df_levels %>%
    select(metric,idx1,lower_breaks,upper_breaks) %>%
    dplyr::mutate(lower_breaks = ifelse(is.na(lower_breaks),-9999,lower_breaks)) %>%
    dplyr::mutate(upper_breaks = ifelse(is.na(upper_breaks),0,upper_breaks)) %>%
    dplyr::mutate(upper_breaks = ifelse(is.infinite(upper_breaks),99999999,upper_breaks))

  #conditions for assigning management levels
  df_ <- df %>%
    left_join(df_levels_) %>%
    mutate(metric_value = ifelse(is.na(metric_value),-9999,metric_value)) %>%
    filter(metric_value >= lower_breaks & metric_value < upper_breaks) %>%
    select(-lower_breaks,-upper_breaks) %>%
    mutate(metric_value = ifelse(metric_value == -9999,NA, metric_value)) %>%
    left_join(df_levels) %>%
    select(-idx0,-idx1)

  #add column called colour_order to put sorting or numerical order to the colours
  df_colour <- tribble(
    ~colour_text, ~colour_order,
    'grey',0,
    'green',1,
    'yellow',2,
    'orange',3,
    'red',4
  )
  df_ <- left_join(df_,df_colour)


  return(df_)




}

#' Retrieves the management level summary of station and airzones
#'
#' @param outputtype is either 'complete','station', 'airzone'
#' 'complete' means that output is detailed for each metric, in each station
#' 'station' means that output is a summary of the management for the station. only metric with highest management level is displayed
#' 'airzone' means that output is a summary of the management for the airzones
#' @param df_preload is dataframe of preloaded data, generated in initial load only
get_management_summary <- function(outputtype = 'complete',df_preload = NULL) {


  #define the parameter for each metric
  #arrange in terms of an order
  df_metric <- tribble(
    ~metric,~parameter,
    "pm25_annual",'pm25',
    "pm25_24h",'pm25',
    "o3_8h",'o3',
    "no2_ann",'no2',
    "no2_1hr",'no2',
    "so2_ann",'so2',
    "so2_1hr",'so2',
  )

  df <- df_preload

  if (is.null(df_preload)) {
    lst_stations <- listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE) %>%
      dplyr::rename(latitude = LAT,
                    longitude = LONG,
                    airzone = AIRZONE,
                    label = Label)

    df <- get_management()

    df <- df %>%
      select(site,instrument,year,metric,metric_value,colour,colour_text,colour_order,tfee) %>%
      left_join(lst_stations) %>%
      left_join(df_metric)
  }
  if (0) {
    readr::write_csv(df,'././test_data/management.csv')
  }
  #add order to the metric
  df$metric <- factor(df$metric,levels = df_metric$metric)
  #calculate and return result based on the type specified
  outputtype <- tolower(outputtype)
  if (outputtype == 'complete') {
    return(df)
  }

  if (outputtype == 'station') {

    df <- df %>%
      group_by(parameter,site,year,airzone,tfee) %>%
      dplyr::mutate(max_colour_order = max(colour_order)) %>%
      ungroup() %>%
      filter(colour_order == max_colour_order) %>%
      arrange(metric) %>%   #this gives priority to annual over 24h/1h metrics
      group_by(parameter,site,year,airzone,tfee) %>%
      dplyr::mutate(index =1:n()) %>%
      filter(index==1) %>% ungroup() %>% select(-index) %>%
      arrange(parameter,site,tfee,year) %>%
      select(-max_colour_order)

    return(df)
  }

  if (outputtype == 'airzone') {

    df <- df %>%
      arrange(airzone,metric_value) %>%
      group_by(parameter,metric,year,airzone,tfee) %>%
      dplyr::mutate(max_metric_value = max(metric_value,na.rm = TRUE)) %>%
      ungroup() %>%
      filter(metric_value == max_metric_value) %>%
      arrange(desc(colour_order), metric) %>%   #this gives prioroty to pm2.5annual or 24h, and no2_3yr over 1 yr
      group_by(parameter,year,airzone,tfee) %>%
      dplyr::mutate(max_colour_order = max(colour_order),index = 1:n()) %>%
      filter(colour_order == max_colour_order,index == 1) %>% ungroup() %>% select(-index) %>%
      COLUMN_REORDER(c('parameter','airzone','tfee','year')) %>%
      select(-max_colour_order,-max_metric_value) %>%
      arrange(parameter,airzone,tfee,year)

    return(df)
  }

}
