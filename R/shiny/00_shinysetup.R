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

# The functions here are meant to create functions and data to prepare shiny data




# CAAQS-related Calculatons-----

#' Calculate annual metrics
#'
#' @param years is vector listing the years for CAAQS calculation
#' @param savedirectory is the directory where the saved files will be located
#'
#' @return annual_results.csv file
create_metrics_annual <- function(years, savedirectory = NULL) {
  if (0) {
    years <- 2000:2010
    savedirectory <- './test_data'
  }

  if (is.null(savedirectory)) {
    savedirectory <- './'
  }


  if (0)
  {
    #debug, to retrieve from previous wide version
    #chose from these four
    savefile <- paste(savedirectory,'pm25_annual.csv',sep='/')
    savefile <- paste(savedirectory,'o3_annual.csv',sep='/')
    savefile <- paste(savedirectory,'no2_annual.csv',sep='/')
    savefile <- paste(savedirectory,'so2_annual.csv',sep='/')

    #run both
    df <- readr::read_csv(savefile)
    savefile <- paste(savedirectory,'annual_results.csv',sep='/')
  }

  # create annual metrics ----
  savefile <- paste(savedirectory,'annual_results.csv',sep='/')
  #define non-value columns
  cols_static <- c('parameter','year','station_name','site','instrument','station','station_name_full')


  #pm2.5
  # savefile <- paste(savedirectory,'pm25_annual.csv',sep='/')
  for (year in years) {

    try({
      df <- importBC_data_avg(parameter = 'pm25',years = year,
                              averaging_type = c('annual 98p 24h','annual mean 24h'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)
      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]

      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,tfee,year,metric,value)

      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })

  }

  #ozone
  # savefile <- paste(savedirectory,'o3_annual.csv',sep='/')
  for (year in years) {
    try({
      df <- importBC_data_avg(parameter = 'o3',years = year,
                              averaging_type = c('annual 4th d8hm'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)

      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]

      #make the result long
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,tfee,year,metric,value)

      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })

  }

  #no2
  # savefile <- paste(savedirectory,'no2_annual.csv',sep='/')
  for (year in years) {

    try({
      df <- importBC_data_avg(parameter = 'no2',years = year,
                              averaging_type = c('annual 98p d1hm', 'annual mean 1hr'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)

      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]

      #make the result long
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,tfee,year,metric,value)


      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })

  }

  #so2
  # savefile <- paste(savedirectory,'so2_annual.csv',sep='/')
  for (year in years) {

    try({
      df <- importBC_data_avg(parameter = 'so2',years = year,
                              averaging_type = c('annual 99p d1hm', 'annual mean 1hr'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)

      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]

      #make the result long
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,tfee,year,metric,value)

      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })

  }




  #create captures ------
  savefile <- paste(savedirectory,'captures.csv',sep='/')
  for (param in c('pm25','o3','no2','so2')) {
    for (year in years) {
      try({
        df <- get_captures(param = param, years = year, merge_Stations = TRUE)
        readr::write_csv(df,
                         file = savefile,
                         append = file.exists(savefile))
      })
    }
  }


}

#' Calculate annual CAAQS metrics
#'
#' @description This script is dependent on the rcaaqs package
#' it already properly applies GDAD in calculation of yearly metrics, and the CAAQS
#' Better than create_metrics_annual() but may take longer to complete
#'
#' @param years is vector listing the years for CAAQS calculation
#' @param savedirectory is the location where the result files are saved
#'
#' @return caaqs_results.csv file
create_caaqs_annual <- function(years, savedirectory = NULL) {
  if (0) {
    years <- 2013:2021
    savedirectory <- './test_data'

    for (files in list.files('././r/',full.names = TRUE)) {
      try(source(files))
    }
  }

  if (is.null(savedirectory)) {
    savedirectory <- '././test_data'
    list.files(savedirectory)
  }
  #where files will be saved
  savefile = paste(savedirectory,'caaqs_results.csv',sep='/')

  #defines the resulting column names in this order
  cols_final <- c('parameter','site','instrument','year',
                  'tfee','metric_value','metric','flag_two_of_three_years')

  for (param in c('pm25','o3','no2','so2')) {
    df <- NULL

    #Retrieve data, different retrieval for ozone
    if(param != 'o3') {
      try(
        df <- importBC_data(param,years = (min(years)-2):max(years),
                            flag_TFEE = TRUE,merge_Stations = TRUE)
      )
    } else {
      try(
        df <- importBC_data(param,years = (min(years)-3):max(years),
                            flag_TFEE = TRUE,merge_Stations = TRUE)
      )
    }

    #if there was no data retrieved
    if (is.null(df)) {
      next
    }

    if (nrow(df) == 0) {
      next
    }

    #remove duplicate entries
    #note for pm, instrument is included in grouping
    if (param == 'pm25') {
      df <- df %>%
        dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
        dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
        filter(!is.na(value)) %>%
        group_by(date_time,site,instrument) %>%
        dplyr::mutate(index = 1:n()) %>%
        filter(index == 1) %>% select(-index)
    } else {
      df <- df %>%
        dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
        dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
        filter(!is.na(value)) %>%
        group_by(date_time,site) %>%
        dplyr::mutate(index = 1:n()) %>%
        filter(index == 1) %>% select(-index)
    }


    if (param == 'pm25') {
      #without TFEE
      pm25_annual <- rcaaqs::pm_annual_caaqs(df,by=c('site','instrument'))
      pm25_24h <- rcaaqs::pm_24h_caaqs(df,by=c('site','instrument'))

      pm25_caaqs <- pm25_annual$caaqs %>%
        bind_rows(pm25_24h$caaqs) %>%
        mutate(parameter = 'PM25',tfee = FALSE) %>%
        dplyr::rename(year = caaqs_year) %>%
        select(cols_final)

      pm25_yoy <- pm25_annual$yearly_avg %>%
        bind_rows(pm25_24h$yearly_98)%>%
        mutate(parameter = 'PM25',tfee = FALSE,flag_two_of_three_years = NA) %>%
        select(parameter,site,instrument,year,tfee,ann_avg,ann_98_percentile,flag_two_of_three_years) %>%
        tidyr::pivot_longer(cols = c(ann_avg,ann_98_percentile)) %>%
        filter(!is.na(value)) %>%
        dplyr::rename(metric = name,
                      metric_value = value)%>%
        select(cols_final)

      df_result <- pm25_caaqs %>%
        bind_rows(pm25_yoy)

      #with TFEE
      pm25_annual <- rcaaqs::pm_annual_caaqs(df %>% filter(!flag_tfee),by=c('site','instrument'))
      pm25_24h <- rcaaqs::pm_24h_caaqs(df %>% filter(!flag_tfee),by=c('site','instrument'))

      pm25_caaqs <- pm25_annual$caaqs %>%
        bind_rows(pm25_24h$caaqs) %>%
        mutate(parameter = 'PM25',tfee = TRUE) %>%
        dplyr::rename(year = caaqs_year) %>%
        select(cols_final)

      pm25_yoy <- pm25_annual$yearly_avg %>%
        bind_rows(pm25_24h$yearly_98)%>%
        mutate(parameter = 'PM25',tfee = TRUE,flag_two_of_three_years = NA) %>%
        select(parameter,site,instrument,year,tfee,ann_avg,ann_98_percentile) %>%
        tidyr::pivot_longer(cols = c(ann_avg,ann_98_percentile)) %>%
        filter(!is.na(value)) %>%
        dplyr::rename(metric = name,
                      metric_value = value)

      df_result <- df_result %>%
        bind_rows(pm25_caaqs) %>%
        bind_rows(pm25_yoy) %>%
        mutate(metric = recode(metric,
                               'pm2.5_annual'='pm25_annual',
                               'pm2.5_24h'='pm25_24h',
                               'ann_98_percentile'='pm25_24hr(1yr)',
                               'ann_avg'='pm25_ann(1yr)'
        ))

      rm('pm25_annual')
      rm('pm25_24h')
    }

    if (param == 'o3') {

      #without TFEE
      o3_8h <- rcaaqs::o3_caaqs(df,by=c('site'))

      #with TFEE
      o3_8h_tfee <- rcaaqs::o3_caaqs(df %>% filter(!flag_tfee) ,by=c('site'))

      o3_caaqs <-  o3_8h$caaqs%>% mutate(tfee = FALSE) %>%
        bind_rows(o3_8h_tfee$caaqs%>% mutate(tfee = TRUE)) %>%
        mutate(parameter = 'O3', instrument = NA)%>%
        dplyr::rename(year = caaqs_year) %>%
        select(cols_final)

      o3_yoy <- o3_8h$ann_4th_highest %>%
        mutate(tfee = FALSE) %>%
        bind_rows(
          o3_8h_tfee$ann_4th_highest %>% mutate(tfee = TRUE)
        ) %>%
        select(site,year,tfee,ann_4th_highest) %>%
        dplyr::rename(metric_value = ann_4th_highest) %>%
        mutate(parameter = 'O3', instrument = NA,
               metric = 'o3_8h(1yr)',flag_two_of_three_years = NA) %>%
        select(cols_final)

      df_result <- o3_caaqs %>%
        bind_rows(o3_yoy) %>%
        mutate(metric = recode(metric,'o3'='o3_8h'))

      rm('o3_8h')
      rm('o3_8h_tfee')
    }

    if (param == 'no2') {

      #without TFEE
      no2_1hr <- rcaaqs::no2_3yr_caaqs(df,by=c('site'))
      no2_ann <- rcaaqs::no2_1yr_caaqs(df,by=c('site'))

      no2_caaqs <-  no2_1hr$caaqs%>%
        mutate(tfee = FALSE) %>%
        bind_rows(
          no2_ann$caaqs%>%
            mutate(tfee = FALSE)
        ) %>%
        dplyr::rename(year = caaqs_year) %>%
        mutate(parameter = 'NO2', instrument = NA) %>%
        select(cols_final)

      no2_yoy <- no2_1hr$yearly_98 %>%
        bind_rows(
          no2_ann$yearly_hr
        ) %>%
        select(site,year,ann_98_percentile,avg_yearly) %>%
        mutate(parameter = 'NO2',tfee = FALSE,instrument = NA,flag_two_of_three_years=NA) %>%
        tidyr::pivot_longer(cols = c(ann_98_percentile,avg_yearly)) %>%
        dplyr::rename(metric_value = value, metric = name) %>%
        filter(!is.na(metric_value)) %>%
        select(cols_final)

      df_result <- no2_caaqs %>%
        bind_rows(no2_yoy)%>%
        mutate(metric = recode(metric,
                               'no2_3yr'='no2_1hr',
                               'no2_1yr'='no2_ann',
                               'ann_98_percentile'='no2_1hr(1yr)',
                               'avg_yearly'='no2_ann(1yr)'
        ))

      rm('no2_1hr')
      rm('no2_ann')

    }

    if (param == 'so2') {


      #without TFEE
      so2_1hr <- rcaaqs::so2_3yr_caaqs(df,by=c('site'))
      so2_ann <- rcaaqs::so2_1yr_caaqs(df,by=c('site'))

      so2_caaqs <-  so2_1hr$caaqs%>% mutate(tfee = FALSE) %>%
        bind_rows(so2_ann$caaqs%>% mutate(tfee = FALSE)) %>%
        dplyr::rename(year = caaqs_year) %>%
        mutate(parameter = 'SO2', instrument = NA) %>%
        select(cols_final)

      so2_yoy <- so2_1hr$yearly_99 %>%
        bind_rows(so2_ann$yearly_hr) %>%
        select(site,year,ann_99_percentile,avg_yearly) %>%
        tidyr::pivot_longer(cols = c(ann_99_percentile,avg_yearly)) %>%
        dplyr::rename(metric = name,
                      metric_value = value) %>%
        filter(!is.na(metric_value)) %>%
        mutate(tfee = FALSE,parameter = 'SO2',
               instrument = NA,flag_two_of_three_years = NA) %>%
        select(cols_final)

      df_result <- so2_caaqs %>%
        bind_rows(so2_yoy) %>%
        select(cols_final)%>%
        mutate(metric = recode(metric,
                               'so2_3yr'='so2_1hr',
                               'so2_1yr'='so2_ann',
                               'ann_99_percentile'='so2_1hr(1yr)',
                               'avg_yearly'='so2_ann(1yr)'
        ))

      rm('so2_1hr')
      rm('so2_ann')

    }



    #save the result into a file
    #append if the file exist
    #standardize the names of columns
    df_result <- df_result %>%
      filter(year %in% years) %>%
      arrange(parameter,site,instrument,year,tfee)

    readr::write_csv(df_result,file = savefile,append = file.exists(savefile))
  }

  df_result <- readr::read_csv(savefile)
  return(df_result)
}


#' INCOMPLETE:
#' Create files that will make the CAAQS bar graph
#'
#' @param filedirectory is the location of the data files
#'
#' @return creates a file called air_data_summary.csv in the file directory
create_CAAQS_graph_files <- function(filedirectory = NULL) {

  if (0) {
    filedirectory <- NULL
  }

  if (is.null(filedirectory)) {
    filedirectory <- '././test_data'
    list.files(filedirectory)
  }

  file_annual <- paste(filedirectory,'annual_results.csv',sep='/')
  file_captures <- paste(filedirectory,'annual_results.csv',sep='/')
  file_ <- paste(filedirectory,'annual_results.csv',sep='/')
}

# Air zone-related calcuations----

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
get_management_summary <- function(outputtype = 'complete',df_preload = NULL,
                                   datafile = NULL) {


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

    df <- get_management(datafile = datafile)

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


