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


#' Calculate the data capture for the station and parameter
#'
#' NOTE: This will output the valid days, valid days in quarter, valid
#'
#' @param param is the pollutant(param). Use list_parameters() to list availble parameters.
#' You can use vector for multi-pollutant query.
#' param can also be a dataframe retrieved from an importBC_data() function.
#' @param years is the year or years. You can use vectors to query multiple years. If NULL, it will be the current year
#' @param adjust default is FALSE. if TRUE (not working), it will combine some stations/instruments
#' based on the merging requirement applies in CAAQS
#' @param stop_at_present default is TRUE. It will remove values after the present date/time
#' The present date/time is determined from the latest PM2.5 data
#' @examples
#' get_captures('o3',years = 2017:2020)
#' df <- importBC_data('o3',years = 2017:2020)
#' get_captures(df)
#'
#' @export
#'
get_captures <- function(param,years=NULL,adjust=FALSE,stop_at_present = TRUE) {

  if (0) {
    param <- 'pm2.5'
    source('./r/importbc_data.R')
    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    years <- 2022
    stop_at_present <- TRUE
  }

  require(dplyr)
  if (!is.data.frame(param)) {
  df <- importBC_data(param,years=years)
  } else {
    df <- param
  }

  df_24h <- importBC_data_avg(df,averaging_type = '24h')

  #check if limited to current date
  if (stop_at_present & (lubridate::year(Sys.Date()) %in% years)) {
    #retrieve current date
    current_date <- importBC_data('pm25') %>%
      filter(!is.na(RAW_VALUE)) %>%
      pull(DATE_PST) %>%
      max()
  } else {
    current_date <- df %>%
      pull(DATE_PST) %>%
      max()
  }

  #calculate hours of data captured----
  #note: new station "TOTAL" created with perfect data captures
  df_full <- df %>%
    select(DATE_PST,PARAMETER) %>%
    unique() %>%
    dplyr::mutate(STATION_NAME = 'TOTAL',INSTRUMENT = 'TOTAL') %>%
    pad_data(add_DATETIME = TRUE) %>%
    filter(DATE_PST<=current_date) %>%
    dplyr::mutate(year = lubridate::year(DATE),
                  month = as.character(DATE,format='%Y-%b')) %>%
    dplyr::mutate(quarter = paste(year,'-Q',lubridate::quarter(DATE),sep=''))%>%
    arrange(DATE_PST)

  # #factorized to sort month and quarter chronologically
  df_full$month <- factor(df_full$month,levels = unique(df_full$month))
  df_full$quarter <- factor(df_full$quarter,levels = unique(df_full$quarter))


  captures_hrs <-  df %>%
    ungroup() %>%
    dplyr::filter(!is.na(RAW_VALUE)) %>%
    select(DATE_PST,DATE,STATION_NAME,INSTRUMENT,PARAMETER) %>%
    #insert station "TOTAL"
    dplyr::bind_rows(df_full %>% select(DATE_PST,DATE,STATION_NAME,INSTRUMENT,PARAMETER)) %>%
    dplyr::mutate(datetime = DATE_PST - lubridate::hours(1)) %>%
    dplyr::mutate(year = lubridate::year(datetime),
                  month = as.character(datetime,format='%Y-%b')) %>%
    dplyr::mutate(quarter = paste(year,'-Q',lubridate::quarter(datetime),sep='')) %>%
    #factorize month and quarter, to add chronological order
    dplyr::mutate(month = factor(month,levels = levels(df_full$month)),
                  quarter = factor(quarter,levels = levels(df_full$quarter))) %>%
    group_by(STATION_NAME,INSTRUMENT,PARAMETER,year,quarter,month) %>%
    dplyr::mutate(valid_hrs_month= n()) %>%
    ungroup() %>%
    group_by(STATION_NAME,INSTRUMENT,PARAMETER,year,quarter) %>%
    dplyr::mutate(valid_hrs_quarter = n()) %>%
    ungroup() %>%
    group_by(STATION_NAME,INSTRUMENT,PARAMETER,year) %>%
    dplyr::mutate(valid_hrs_year = n()) %>%
    ungroup()


  #categorize the entries into years, quarter, months
  df_captures <- captures_hrs %>%
    select(STATION_NAME,INSTRUMENT,PARAMETER,year,valid_hrs_year) %>% unique() %>%
    dplyr::rename(value = valid_hrs_year,
                  date_value = year) %>%
    dplyr::mutate(unit = 'hours',date_category = 'year',
                  date_value = as.character(date_value)) %>%
    dplyr::bind_rows(

      captures_hrs %>%
        select(STATION_NAME,INSTRUMENT,PARAMETER,quarter,valid_hrs_quarter) %>% unique() %>%
        dplyr::rename(value = valid_hrs_quarter,
                      date_value = quarter) %>%
        dplyr::mutate(unit = 'hours',date_category = 'quarter',
                      date_value = as.character(date_value))
    ) %>%
    dplyr::bind_rows(
      captures_hrs %>%
        select(STATION_NAME,INSTRUMENT,PARAMETER,month,valid_hrs_month) %>% unique() %>%
        dplyr::rename(value = valid_hrs_month,
                      date_value = month) %>%
        dplyr::mutate(unit = 'hours',date_category = 'month',
                      date_value = as.character(date_value))
    )


  #calculate days of data captured----

  captures_days <-  df_24h %>%
    ungroup() %>%
    dplyr::filter(!is.na(RAW_VALUE_24h)) %>%
    select(DATE,STATION_NAME,INSTRUMENT,PARAMETER) %>%
    #insert station "TOTAL"
    dplyr::bind_rows(df_full %>% select(DATE,STATION_NAME,INSTRUMENT,PARAMETER) %>% unique()) %>%
    dplyr::mutate(year = lubridate::year(DATE),
                  month = as.character(DATE,format='%Y-%b')) %>%
    dplyr::mutate(quarter = paste(year,'-Q',lubridate::quarter(DATE),sep='')) %>%
    #factorize month and quarter, to add chronological order
    dplyr::mutate(month = factor(month,levels = levels(df_full$month)),
                  quarter = factor(quarter,levels = levels(df_full$quarter))) %>%
    group_by(STATION_NAME,INSTRUMENT,PARAMETER,year,quarter,month) %>%
    dplyr::mutate(valid_days_month= n()) %>%
    ungroup() %>%
    group_by(STATION_NAME,INSTRUMENT,PARAMETER,year,quarter) %>%
    dplyr::mutate(valid_days_quarter = n()) %>%
    ungroup() %>%
    group_by(STATION_NAME,INSTRUMENT,PARAMETER,year) %>%
    dplyr::mutate(valid_days_year = n()) %>%
    ungroup()


  #categorize the entries into years, quarter, months
  df_captures <- df_captures %>%
    dplyr::bind_rows(
    captures_days %>%
    select(STATION_NAME,INSTRUMENT,PARAMETER,year,valid_days_year) %>% unique() %>%
    dplyr::rename(value = valid_days_year,
                  date_value = year) %>%
    dplyr::mutate(unit = 'days',date_category = 'year',
                  date_value = as.character(date_value))
    )%>%
    dplyr::bind_rows(

      captures_days %>%
        select(STATION_NAME,INSTRUMENT,PARAMETER,quarter,valid_days_quarter) %>% unique() %>%
        dplyr::rename(value = valid_days_quarter,
                      date_value = quarter) %>%
        dplyr::mutate(unit = 'days',date_category = 'quarter',
                      date_value = as.character(date_value))
    ) %>%
    dplyr::bind_rows(
      captures_days %>%
        select(STATION_NAME,INSTRUMENT,PARAMETER,month,valid_days_month) %>% unique() %>%
        dplyr::rename(value = valid_days_month,
                      date_value = month) %>%
        dplyr::mutate(unit = 'days',date_category = 'month',
                      date_value = as.character(date_value))
    )

  #add total valid column-----
  df_TOTAL <- df_captures %>%
    filter(STATION_NAME == 'TOTAL') %>%
    select(date_value,value,unit,date_category) %>% unique() %>%
    dplyr::rename(total = value) %>%
    merge(
      df_captures%>%
        select(STATION_NAME,INSTRUMENT,PARAMETER) %>%
        unique()
    )

  df_captures <- df_TOTAL %>%
    left_join(df_captures) %>%
    filter(STATION_NAME != 'TOTAL') %>%
    select(STATION_NAME,INSTRUMENT,PARAMETER,date_category,date_value,value,total,unit) %>%
    arrange(STATION_NAME,INSTRUMENT,PARAMETER) %>%
    dplyr::mutate(value = ifelse(is.na(value),0,value)) %>%
    tidyr::pivot_wider(names_from = unit, values_from = c(value,total))

  #change data_value to factor
  df_captures$date_value <- factor(df_captures$date_value,
                                      levels = unique(df_TOTAL$date_value))
    # View()
    return(df_captures)
}
