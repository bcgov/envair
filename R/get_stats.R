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

#' Calculate the statistucs for the pollutants based on CAAQS
#'
#' Note that these are based on the metrics defined by the CCME
#' Guidance Document of Achievement Determination
#'
#' @param param is the parameter or vector of parameters.
#' @param datetime is a string defining the datetime field. This field shoule be in time-ending format.
#' @param add_TFEE default FALSE. If TRUE, it will also calculate on data without TFEE
#' @param merge_Stations default FALSE. If TRUE, it will combine stations as practiced in air zone reporting
#' @examples
#'
#' @export
#'
get_stats <- function(param, years=NULL,add_TFEE = FALSE, merge_Stations = FALSE)
{
  if (0) {

    source('./r/importbc_data.R')
    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/get_captures.R')
    source('./r/envairfunctions.R')
    merge_Stations = TRUE
    # param <- c('pm25','no2')
    param <- 'o3'
    years <- 2021
    include_TFEE = TRUE
    merge_Stations <- TRUE

  }

  #re-defined, added only to stay consistent, use add_TFEE
  include_TFEE <- add_TFEE


  #list of stats for different parameter

  df_stats_list <- tidyr::tribble(
    ~PARAMETER, ~averaging_type,
    'PM25','annual mean 24hour',
    'PM25','annual 98p 24hour',
    'PM25','excess 27 24hour',
    'PM25','excess 25 24hour',
    'NO2','annual mean 1-hour',
    'NO2','annual 98p d1hm',
    'NO2','excess 60 d1hm',
    'SO2','annual mean 1-hour',
    'SO2','annual 99p d1hm',
    'SO2','excess 70 d1hm',
    'O3','annual 4th d8hm',
    'O3','excess 62 d8hm',
  )

  #define the list of data captures
  df_captures_list <- tidyr::tribble(
    ~PARAMETER, ~date_category,~capture_type,
    'PM25','year','valid_days',
    'PM25','quarter','valid_days',
    'NO2','year','valid_days',
    'NO2','quarter','valid_days',
    'SO2','year','valid_days',
    'SO2','quarter','valid_days',
    'O3','quarter','valid_days'

  )
  #add the total and percentage in the list to extract from data captures
  df_captures_list <- df_captures_list %>%
    dplyr::bind_rows(
      df_captures_list %>%
        dplyr::mutate(capture_type = gsub('valid','total',capture_type))
    ) %>%
    dplyr::bind_rows(
      df_captures_list %>%
        dplyr::mutate(capture_type = gsub('valid','perc',capture_type))
    ) %>%
    unique()

  #assigning default values
  # for years, use the current year
  if (is.null(years)){
    years <- lubridate::year(Sys.Date())
  }


  df <- importBC_data(param = param,years = years,flag_TFEE = include_TFEE,merge_Stations = merge_Stations)

  #for ozone, add extra previous year for averaging purposes
  if ('O3' %in% unique(df$PARAMETER)) {
    df <- df %>%
      dplyr::bind_rows(
        importBC_data(param = param,years = min(years)-1,flag_TFEE = include_TFEE,merge_Stations = merge_Stations)
      )
  }

  param <- unique(df$PARAMETER)   #to standardize parameter, extract from data


  #retrieve data captures
  df_captures <- get_captures(param=df)


  cols <- colnames(df_captures)
  cols_select <- cols[grepl('valid_',cols,ignore.case = TRUE) |
                        grepl('total_',cols,ignore.case = TRUE) |
                        grepl('perc_',cols,ignore.case = TRUE)]

  df_captures <-df_captures %>%
    tidyr::pivot_longer(cols=cols_select) %>%

    # View()
    dplyr::rename(capture_type = name) %>%
    dplyr::mutate(index=paste(PARAMETER,date_category,capture_type)) %>%
    filter(index %in% (
      df_captures_list %>% dplyr::mutate(index=paste(PARAMETER,date_category,capture_type)) %>% pull(index)
    )) %>%
    dplyr::mutate(capture_type = paste(capture_type,'(',date_value,')',sep='')) %>%
    select(-index,-date_category,-date_value)

  #change capture_type to a factor, to list in specific order
  cols <- unique(df_captures$capture_type)
  cols_select <- c('valid_days(year)','valid_days(Q1)','valid_days(Q2)','valid_days(Q3)','valid_days(Q4)',
                   'total_days(year)','total_days(Q1)','total_days(Q2)','total_days(Q3)','total_days(Q4)',
                   'perc_days(year)','perc_days(Q1)','perc_days(Q2)','perc_days(Q3)','perc_days(Q4)',
                   'valid_hours(year)','valid_hours(Q1)','valid_hours(Q2)','valid_hours(Q3)','valid_hours(Q4)',
                   'total_hours(year)','total_hours(Q1)','total_hours(Q2)','total_hours(Q3)','total_hours(Q4)',
                   'perc_hours(year)','perc_hours(Q1)','perc_hours(Q2)','perc_hours(Q3)','perc_hours(Q4)'

                   )

  cols_select <- cols_select[cols_select %in% cols]
  cols <- c(cols_select,cols[!cols %in% cols_select])
  df_captures$capture_type <- factor(df_captures$capture_type,levels=cols)

  df_stats_list <- df_stats_list %>%
    arrange(PARAMETER) %>%
    filter(PARAMETER %in% param)



  df_result <- NULL
  #extract data one type at a time
  for (i in 1:nrow(df_stats_list)) {
    param_i <- df_stats_list[i,]
    df_ <- df %>%
      filter(PARAMETER == param_i$PARAMETER) %>%
      importBC_data_avg(years=years,averaging_type = param_i$averaging_type,flag_TFEE = include_TFEE,merge_Stations = merge_Stations)

    cols <- colnames(df_)
    cols_select <- c('STATION_NAME','STATION_NAME_FULL','INSTRUMENT','PARAMETER','DATE','YEAR','TIME','DATE_PST','DATETIME')
    cols_select <- cols[tolower(cols) %in% tolower(cols_select)]
    cols_notselect <- cols[!cols %in% cols_select]

    df_result <- df_result %>%
      dplyr::bind_rows(
        df_ %>%
          tidyr::pivot_longer(cols = cols_notselect)
      )
  }

  #pivot wider and add captures
  df_result <- df_result %>%
    tidyr::pivot_wider(names_from = name,values_from = value)

  df_result <- df_result %>%
    left_join(
      df_captures %>%
        dplyr::rename(YEAR=year) %>%
        arrange(capture_type) %>%
        ungroup() %>% unique() %>%
        tidyr::pivot_wider(names_from = capture_type,values_from = value)
    ) %>%
    filter(YEAR %in% years)


  return(df_result)
}


