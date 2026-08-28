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

#' Calculate the statistics for the pollutants based on CAAQS
#'
#' Note that these are based on the metrics defined by the CCME
#' Guidance Document of Achievement Determination
#'
#' @param param is the parameter or vector of parameters.
#' @param datetime is a string defining the datetime field. This field shoule be in time-ending format.
#' @param add_TFEE default FALSE. If TRUE, it will also calculate on data without TFEE
#' @param merge_Stations default FALSE. If TRUE, it will combine stations as practiced in air zone reporting
#' @export
#'
get_stats_ <- function(param, years=NULL,add_TFEE = FALSE, merge_Stations = FALSE)
{
  if (0) {

    for (files in list.files('./r',full.names = TRUE)) {
      try(source(files))
    }

    merge_Stations = TRUE
    # param <- c('pm25','no2')
    param <- 'o3'
    years <- 2021
    add_TFEE = FALSE
    merge_Stations <- FALSE

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
    'NO2','excess 42 d1hm',
    'SO2','annual mean 1-hour',
    'SO2','annual 99p d1hm',
    'SO2','excess 70 d1hm',
    'SO2','excess 65 d1hm',
    'O3','annual 4th d8hm',
    'O3','excess 62 d8hm',
    'O3','excess 60 d8hm',
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
    distinct()

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
  df_captures <- get_captures0(param = df) %>%
    filter(year %in% years)


  cols <- colnames(df_captures)
  cols_select <- cols[grepl('valid_',cols,ignore.case = TRUE) |
                        grepl('total_',cols,ignore.case = TRUE) |
                        grepl('perc_',cols,ignore.case = TRUE)]

  df_captures <-df_captures %>%
    tidyr::pivot_longer(cols=cols_select) %>%

    # View()
    dplyr::rename(capture_type = name) %>%
    dplyr::mutate(index=paste(parameter,date_category,capture_type)) %>%
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
  #extract data one parameter at a time



  for (param_ in param) {

    print(param_)
    df_ <- importBC_data_avg(parameter = param_,
                             years = years,
                             averaging_type = df_stats_list$averaging_type,
                             flag_TFEE = include_TFEE,
                             merge_Stations = merge_Stations)

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
        ungroup() %>% distinct() %>%
        tidyr::pivot_wider(names_from = capture_type,values_from = value)
    ) %>%
    filter(YEAR %in% years)


  return(df_result)
}



#' Calculate the CAAQS metric values for the pollutants
#'
#' Returns a long (tidy) table with one row per station / parameter / year /
#' CAAQS metric. Both the \code{raw_} (un-rounded) statistic (\code{value}) and
#' the value rounded to the CAAQS-defined precision (\code{value_rounded}, using
#' envair's \code{round2}) are reported.
#'
#' Note that these are based on the metrics defined by the CCME
#' Guidance Document of Achievement Determination.
#'
#' @param param is the parameter or vector of parameters (PM25, NO2, SO2, O3).
#' @param years is the year or vector of years. If NULL, the current year is used.
#' @param add_TFEE default FALSE. If TRUE, it will also calculate on data without TFEE
#' @param merge_stations default FALSE. If TRUE, it will combine stations as practiced in air zone reporting
#'
#' @return A dataframe with columns \code{station_name}, \code{parameter},
#'   \code{instrument} (populated only for PM25, \code{NA} otherwise), \code{year},
#'   \code{metric} (the CAAQS averaging period, e.g. 'annual', '24h', '1h', '8h'),
#'   \code{value} (the raw metric value) and \code{value_rounded} (the metric
#'   value rounded to the CAAQS-defined precision via \code{round2}).
#' @export
#'
get_stats <- function(param, years = NULL, add_TFEE = FALSE, merge_stations = FALSE) {
  if (0) {
    param <- c('pm25', 'o3')
    years <- 2024:2025
    add_TFEE <- FALSE
    merge_stations <- FALSE
  }

  require(dplyr)

  # -lookup of CAAQS metric <-> raw statistic column for each parameter
  df_metrics <- get_metrics(parameter = param)

  # -retrieve the annual statistics, one parameter at a time
  #  (the importBC_data_avg back-end handles a single parameter at a time)
  # -averaging_type is left NULL so the CAAQS default statistics are used
  df_stats <- NULL
  for (param_ in unique(tolower(param))) {
    df_stats <- df_stats %>%
      dplyr::bind_rows(
        importBC_data_avg(parameter = param_,
                          years = years,
                          flag_TFEE = add_TFEE,
                          merge_stations = merge_stations)
      )
  }

  if (is.null(df_stats) || nrow(df_stats) == 0) {
    return(NULL)
  }

  # -keep only the identifier columns and the raw_ value columns
  cols <- colnames(df_stats)
  cols_raw <- cols[grepl('^raw_', cols, ignore.case = TRUE) &
                     !grepl('tfee', cols, ignore.case = TRUE)]

  df_stats <- df_stats %>%
    dplyr::select(dplyr::any_of(c('station_name', 'parameter', 'instrument', 'year')),
                  dplyr::all_of(cols_raw)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(cols_raw),
                        names_to = 'stat_form',
                        values_to = 'value') %>%
    dplyr::mutate(parameter = toupper(parameter),
                  stat_form = sub('^raw_', '', stat_form, ignore.case = TRUE))

  # -attach the CAAQS metric name; drop statistics that are not CAAQS metrics
  df_stats <- df_stats %>%
    dplyr::inner_join(df_metrics, by = c('parameter', 'stat_form')) %>%
    # -instrument is only meaningful for PM25
    dplyr::mutate(instrument = ifelse(toupper(parameter) == 'PM25',
                                      instrument, NA_character_)) %>%
    # -value_rounded applies the CAAQS-defined precision using envair's round2
    dplyr::mutate(value_rounded = round2(value, precision)) %>%
    dplyr::select(dplyr::any_of(c('station_name', 'parameter', 'instrument',
                                  'year', 'metric', 'value', 'value_rounded'))) %>%
    dplyr::arrange(station_name, parameter, metric)

  return(df_stats)
}



#' List the CAAQS metrics and their statistical form for each parameter
#'
#' Provides the lookup between a parameter (PM25, NO2, SO2, O3), its CAAQS
#' \code{metric} (the averaging period: '24h', '1h', '8h', or 'annual'), and the
#' \code{stat_form} - the annual statistic used to derive that metric. The
#' \code{stat_form} matches the \code{raw_} value column produced by
#' \code{importBC_data_avg()} once the \code{raw_} prefix is dropped
#' (e.g. 'mean_24h', '98p_24h', '98p_d1hm', '99p_d1hm', '4th_d8hm'). The
#' \code{precision} column gives the number of decimal places used by
#' \code{get_stats()} to round the metric value (via \code{round2}).
#'
#' @param parameter optional character vector used to filter the result. Matching
#'   is case-insensitive. If NULL (default), all parameters are returned.
#'
#' @examples
#' get_metrics()
#' get_metrics(c("pm25", "o3"))
#'
#' @export
get_metrics <- function(parameter = NULL) {

  parameter_select <- parameter

  df_metrics <- tidyr::tribble(
    ~parameter, ~metric,  ~stat_form,~precision,
    'PM25',     'annual', 'mean_24h',1,
    'PM25',     '24h',    '98p_24h',0,
    'NO2',      'annual', 'mean_1hr',1,
    'NO2',      '1h',     '98p_d1hm',0,
    'SO2',      'annual', 'mean_1hr',1,
    'SO2',      '1h',     '99p_d1hm',0,
    'O3',       '8h',     '4th_d8hm',0
  )

  if (!is.null(parameter_select)) {
    df_metrics <- df_metrics %>%
      dplyr::filter(toupper(parameter) %in% toupper(parameter_select))
  }

  return(df_metrics)
}
