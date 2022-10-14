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

library("rcaaqs")
library("envreportutils")


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

#Air Zone Graphing Functions----

# Air Zone graphig functions
# Including long term trends

#' Plot Long term trends
#'
#' @param plot_metric is the metric to plot
#' @param station is the station that is emphasis of plot. It can also be a vector.
#' If NULL, the result will be a list of stations in the specified air zone
#' @param airzone is the airzone
#' @param tfee defines if data is adjusted for transboundary flow and exceptional events
#' @param df is the data file
#' @param lst_stations is the list of stations, use importBC_data()
#' If NULL, it will retrieve from the local test data location
plot_trends <- function(plot_metric,station=NULL,airzone = NULL, adjust_tfee = FALSE,
                        df=NULL,lst_stations=NULL) {

  if (0) {
    # readr::write_csv(lst_stations,'./test_data/listbc_stations.csv')
    lst_stations <- readr::read_csv('./test_data/listbc_stations.csv')
    plot_metric <- 'pm25_24h'
    station <- 'Squamish Elementary'
    adjust_tfee = FALSE
    #debug, load other files
    if (0) {
      lst_files <- c(list.files('./r/shiny',full.names = TRUE),
                     list.files('./r',full.names = TRUE)
      )

      for (lst in lst_files) {
        try(source(lst))
      }
    }
    station <- NULL
    df <- NULL
  }
  if (is.null(df)) {
    df <- readr::read_csv('./test_data/caaqs_results.csv')
  }

  print(paste('Looking for:',station,airzone))
  #units
  #define the units foe each metric
  df_unit <- tribble(
    ~metric,~units,
    'pm25_annual',bquote(~"Average "~PM[2.5]~","~mu~g/m^3),
    'pm25_24h',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'pm25_ann(1yr)',bquote("Annual "~PM[2.5]~","~mu~g/m^3),
    'pm25_24hr(1yr)',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'o3_8h',bquote("4th Highest"~O[3]~",ppb"),
    'o3_8h(1yr)',bquote("4th Highest"~O[3]~",ppb"),
    'no2_1hr',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann',bquote("Average "~NO[2]~",ppb"),
    'no2_1hr(1yr)',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann(1yr)',bquote("Average "~NO[2]~",ppb"),
    'so2_1hr',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann',bquote("Average "~SO[2]~",ppb"),
    'so2_1hr(1yr)',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann(1yr)',bquote("Average "~SO[2]~",ppb")
  )

  unit <- df_unit$units[df_unit$metric == plot_metric][[1]]
  #retrieve only list of stations that are in the data
  if (is.null(lst_stations)) {
    lst_stations <- listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE)
  }
  lst_stations <- lst_stations %>%
    filter(site %in% df$site) %>%
    arrange(STATUS) %>%
    group_by(site) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index ==1) %>% select(-index) %>% ungroup() %>%
    select(site,Label, LAT,LONG,AIRZONE)

  df <- df %>%
    left_join(lst_stations)
  #retrieve list of stations if station == NULL
  if (is.null(station)) {
    if (is.null(airzone)) {
      airzone <- df%>%
        pull(AIRZONE) %>%
        unique()
    }
    print('extracting list of sites')
    a <- df %>%
      filter(AIRZONE %in% airzone,
             metric == plot_metric) %>%
      pull(site) %>%
      unique() %>%
      sort()

    return(a)
  }



  #get maximum value for an airzone
  df_airzone <- df %>%
    group_by(AIRZONE,year,metric,tfee) %>%
    dplyr::summarise(max_value = max(metric_value,na.rm = TRUE),
                     min_value = min(metric_value,na.rm = TRUE))



  # filter based on user selection
  # and extract other filtering info
  df <- df %>%
    filter(metric %in% plot_metric,
           tolower(site) %in% tolower(station),
           tfee == adjust_tfee )
  if (is.null(airzone)) {
    airzone <- df %>%
      pull(AIRZONE) %>%
      unique()
  }
  df_airzone <- df_airzone %>%
    filter(metric %in% plot_metric,
           tfee == adjust_tfee,
           AIRZONE %in% airzone)



  #cleanup in case of duplicate entry for station
  df <- df %>%
    arrange(desc(metric_value)) %>%
    group_by(site,tfee,metric,year) %>%
    dplyr::mutate(index = 1:n(), count=n()) %>%
    filter(index == 1) %>% ungroup() %>%
    select(-index, - count)

  #make the dataframe match with the airzone dataframe
  df_ <- df_airzone %>%
    select(year) %>%
    left_join(df)

  df <- df_
  years <- min(df$year): max(df$year)
  a <- df %>%
    arrange(site,year) %>%
    ggplot(aes(x=year,y=metric_value,colour = site,fill = site)) +
    geom_point() +
    geom_line() +
    geom_ribbon(mapping = aes(x=df_airzone$year,
                              ymax=df_airzone$max_value,
                              ymin = df_airzone$min_value),alpha = 0.2,
                fill = 'grey', colour = 'black') +
    scale_x_continuous(breaks = years) +
    scale_y_continuous(limits = c(0,max(df_airzone$max_value, na.rm=TRUE) + 10)) +
    theme(legend.position = 'none',
          panel.background = element_rect(fill=NA,colour = 'black')) +
    xlab('Year')+
    ylab(unit)

  return(a)
}

#' List of metric and parameters
#'
#' @description This contains a list of metrics and the parameters these are related to.
#' Note that there are two types of parameters listed, these are created to ensure coverage for all
#' envair and rcaaqs have different list of metrics
#' parameter is from rcaaqs
#' metric is from envair
#'
#' @export
#'
df_metric_list <- function() {
  #define levels to put metrics and parameters in order
  levels_parameter <- c('pm2.5_annual','pm2.5_24h','o3','no2_1yr','no2_3yr','so2_1yr','so2_3y')
  levels_metric <- c('pm25_annual','pm25_24h','o3_8h','no2_ann','no2_1hr','so2_ann','so2_1hr')
  df_result <- tribble(
    ~pollutant,~parameter,~metric,
    'PM25','pm2.5_annual','pm25_annual',
    'O3','o3','o3_8h',
    'PM25','pm2.5_24h','pm25_24h',
    'NO2','no2_1yr','no2_ann',
    'NO2','no2_3yr','no2_1hr',
    'SO2','so2_1yr','so2_ann',
    'SO2','so2_3yr','so2_1hr'
  )

  df_result$parameter <- factor(df_result$parameter, levels = levels_parameter)
  df_result$metric <- factor(df_result$metric, levels = levels_metric)

  return(df_result)
}
