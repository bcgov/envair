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
#' @param adjust default is FALSE. if TRUE, it will combine some stations/instruments
#' based on the merging requirement applies in CAAQS
#' @param stop_at_present default is TRUE. It will remove values after the present date/time
#' The present date/time is determined from the latest PM2.5 data
#' @examples
#' get_captures('o3',years = 2017:2020)
#' df <- importBC_data('o3',years = 2017:2020)
#' get_captures(df)
#'
#' @export
get_captures <- function(param,years=NULL,merge_Stations=FALSE,stop_at_present = TRUE) {
  #seperate years in order to save memory

  if (0) {
    param <- 'pm25'
    years <- 2020
    merge_Stations=FALSE
    stop_at_present = TRUE
  }

  if (is.data.frame(param)) {
    get_captures0(param = param) %>%
      return()
  }

  df_result <- NULL


  for (year in years) {
    df_result <- df_result %>%
      dplyr::bind_rows(
        get_captures0(param = param, years = year,merge_Stations=merge_Stations , stop_at_present = stop_at_present)
      )
  }

  #make year column uppercase YEAR
  colnames(df_result)[colnames(df_result) == 'year'] <- 'YEAR'
  return(df_result)

}

#' backend function
#'
#' This function makes all the needed calculations to generate the captures data
get_captures0 <- function(param,years=NULL,merge_Stations=FALSE,stop_at_present = TRUE) {

  if (0) {
    param <- 'no2'
    source('./r/importbc_data.R')
    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/envairfunctions.R')
    source('./r/importBC_data_avg.R')

    years <- 2021
    stop_at_present <- TRUE
    merge_Stations <- TRUE
  }

  require(dplyr)

  #list of parameters where the instruments are NOT to be merged automatically
  do_not_mergeauto <- c('PM25','PM10')

  if (!is.data.frame(param)) {
    df <- importBC_data(param,years=years,merge_Stations = merge_Stations)
    #auto-merge Instruments if the stations are merged for non-PM instruments
    if (any(!df$PARAMETER %in% do_not_mergeauto) & merge_Stations) {
      df_instrument <- df %>%
        select(STATION_NAME,INSTRUMENT,PARAMETER) %>%
        distinct() %>%
        group_by(STATION_NAME,PARAMETER) %>%
        dplyr::mutate(new_INSTRUMENT = paste(INSTRUMENT,collapse ='/')) %>%
        distinct()

      df <- df %>%
        left_join(df_instrument) %>%
        mutate(INSTRUMENT_ORIGINAL = ifelse(INSTRUMENT != new_INSTRUMENT,INSTRUMENT,INSTRUMENT_ORIGINAL),
               INSTRUMENT = new_INSTRUMENT) %>%
        select(-new_INSTRUMENT)
    }

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
    distinct() %>%
    dplyr::mutate(STATION_NAME = 'TOTAL',INSTRUMENT = 'TOTAL') %>%
    pad_data(add_DATETIME = TRUE) %>%
    filter(DATE_PST<=current_date) %>%
    dplyr::mutate(year = lubridate::year(DATE),
                  month = as.character(DATE,format='%b')) %>%
    dplyr::mutate(quarter = paste('Q',lubridate::quarter(DATE),sep=''))%>%
    arrange(DATE_PST)

  #factorized to sort month and quarter chronologically
  df_full$month <- factor(df_full$month,levels = month.abb)
  df_full$quarter <- factor(df_full$quarter,levels = paste('Q',1:4,sep=''))


  captures_hrs <-  df %>%
    ungroup() %>%
    dplyr::filter(!is.na(RAW_VALUE)) %>%
    select(DATE_PST,DATE,STATION_NAME,INSTRUMENT,PARAMETER) %>%

    #insert station "TOTAL"
    dplyr::bind_rows(df_full %>% select(DATE_PST,DATE,STATION_NAME,INSTRUMENT,PARAMETER)) %>%
    dplyr::mutate(datetime = DATE_PST - lubridate::hours(1)) %>%
    dplyr::mutate(year = lubridate::year(datetime),
                  month = as.character(datetime,format='%b')) %>%
    dplyr::mutate(quarter = paste('Q',lubridate::quarter(datetime),sep='')) %>%
    #factorize month and quarter, to add chronological order
    dplyr::mutate(month = factor(month,levels = month.abb),
                  quarter = factor(quarter,levels = paste('Q',1:4,sep=''))) %>%
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
    select(STATION_NAME,INSTRUMENT,PARAMETER,year,valid_hrs_year) %>% distinct() %>%
    dplyr::rename(value = valid_hrs_year) %>%
    dplyr::mutate(date_value = 'year',unit = 'hours',date_category = 'year',
                  date_value = as.character(date_value)) %>%
    dplyr::bind_rows(

      captures_hrs %>%
        select(STATION_NAME,INSTRUMENT,PARAMETER,quarter,year,valid_hrs_quarter) %>% distinct() %>%
        dplyr::rename(value = valid_hrs_quarter,
                      date_value = quarter) %>%
        dplyr::mutate(unit = 'hours',date_category = 'quarter',
                      date_value = as.character(date_value))
    ) %>%
    dplyr::bind_rows(
      captures_hrs %>%
        select(STATION_NAME,INSTRUMENT,PARAMETER,year,month,valid_hrs_month) %>% distinct() %>%
        dplyr::rename(value = valid_hrs_month,
                      date_value = month) %>%
        dplyr::mutate(unit = 'hours',date_category = 'month',
                      date_value = as.character(date_value))
    )


  #calculate days of data captured----

  captures_days <-  df_24h %>%
    # dplyr::mutate(year = lubridate::year(DATE)) %>%
    ungroup() %>%
    dplyr::filter(!is.na(RAW_VALUE_24h)) %>%
    select(DATE,STATION_NAME,INSTRUMENT,PARAMETER) %>%
    #insert station "TOTAL"
    dplyr::bind_rows(
      df_full %>%
        select(DATE,STATION_NAME,INSTRUMENT,PARAMETER,year) %>%
        distinct()
      ) %>%
    dplyr::mutate(year = lubridate::year(DATE),
                  month = as.character(DATE,format='%b')) %>%
    dplyr::mutate(quarter = paste('Q',lubridate::quarter(DATE),sep='')) %>%
    #factorize month and quarter, to add chronological order
    dplyr::mutate(month = factor(month,levels =month.abb),
                  quarter = factor(quarter,levels = paste('Q',1:4,sep=''))) %>%
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
    select(STATION_NAME,INSTRUMENT,PARAMETER,year,valid_days_year) %>% distinct() %>%
    dplyr::rename(value = valid_days_year) %>%
    dplyr::mutate(unit = 'days',date_category = 'year',
                  date_value = 'year')
    )%>%
    dplyr::bind_rows(

      captures_days %>%
        select(STATION_NAME,INSTRUMENT,PARAMETER,year,quarter,valid_days_quarter) %>% distinct() %>%
        dplyr::rename(value = valid_days_quarter,
                      date_value = quarter) %>%
        dplyr::mutate(unit = 'days',date_category = 'quarter',
                      date_value = as.character(date_value))
    ) %>%
    dplyr::bind_rows(
      captures_days %>%
        select(STATION_NAME,INSTRUMENT,PARAMETER,year,month,valid_days_month) %>% distinct() %>%
        dplyr::rename(value = valid_days_month,
                      date_value = month) %>%
        dplyr::mutate(unit = 'days',date_category = 'month',
                      date_value = as.character(date_value))
    )

  #add total valid column-----
  df_TOTAL <- df_captures %>%
    filter(STATION_NAME == 'TOTAL') %>%
    select(date_value,value,unit,date_category,year) %>% distinct() %>%
    dplyr::rename(total = value) %>%
    merge(
      df_captures%>%
        select(STATION_NAME,INSTRUMENT,PARAMETER) %>%
        distinct()
    )

  df_captures <- df_TOTAL %>%
    left_join(df_captures) %>%
    filter(STATION_NAME != 'TOTAL') %>%
    select(STATION_NAME,INSTRUMENT,PARAMETER,year,date_category,date_value,value,total,unit) %>%
    arrange(STATION_NAME,INSTRUMENT,PARAMETER) %>%
    dplyr::mutate(value = ifelse(is.na(value),0,value)) %>%
    tidyr::pivot_wider(names_from = unit, values_from = c(value,total))


  # add Q2 + Q3 for ozone
  if ('O3' %in% df_captures$PARAMETER) {
    df_captures_ <- df_captures %>%
      filter(date_value %in% c('Q2','Q3')) %>%
      tidyr::pivot_longer(cols = c('value_hours','value_days','total_hours','total_days')) %>%
      group_by(STATION_NAME,INSTRUMENT,PARAMETER,year,date_category,name) %>%
      dplyr::summarise(`Q2+Q3` = sum(value,na.rm = TRUE)) %>%
      tidyr::pivot_wider(names_from = name, values_from = `Q2+Q3`) %>%
      ungroup() %>%
      mutate(date_value = 'Q2+Q3')


    df_captures <- df_captures %>%
      dplyr::bind_rows(df_captures_)
  }


  #change data_value to factor
  df_captures$date_value <- factor(df_captures$date_value,
                                      levels = c(unique(df_TOTAL$date_value),'Q2+Q3'))

  #rename columns

  colnames(df_captures) <- gsub('value_','valid_',colnames(df_captures),ignore.case = TRUE)
  #add percentage
  df_captures <- df_captures %>%
    dplyr::mutate(perc_hours = 100*(valid_hours)/total_hours,
                  perc_days = 100*(valid_days)/total_days) %>%
    dplyr::mutate(perc_hours = round2(perc_hours,2),
                  perc_days = round2(perc_days,2))


    return(df_captures)
}
