# Copyright 2024 Province of British Columbia
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

# -calculate the data captures

#' Calculate the data capture for the station and parameter
#'
#' NOTE: This will output the valid hours, valid days, valid days in quarter, valid Q2+Q3, valid
#'
#' @param parameter is the pollutant(param). Use list_parameters() to list availble parameters.
#' For one pollutant only
#' param can also be a dataframe retrieved from an importBC_data() function.
#' @param years is the year or years. You can use vectors to query multiple years. If NULL, it will be the current year
#' @param groupby is vector listing the columns that the data is grouped by, if null
#' it groups station and instrument for PM
#' groups by station for non-PM
#' @param merge_Stations default is FALSE. if TRUE, it will combine some stations/instruments
#' based on the merging requirement applies in CAAQS
#' @param stop_at_present default is TRUE if TRUE, it will stop the date at the present date
#' @examples
#' get_data_completeness(c('pm25','no2'), years = 2015:2020,merge_Stations = TRUE)
#'
#' @export
get_data_completeness <- function(parameter,years=NULL,groupby = NULL, merge_Stations=FALSE,stop_at_present = TRUE) {
  if (0) {
    parameter <- 'pm25'
    years <- 2020
    source('./r/importbc_data.R')
    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/envairfunctions.R')
    source('./r/importBC_data_avg.R')
    groupby <- NULL
  }

  require(dplyr)
  require(tidyr)
  require(janitor)
  require(lubridate)

  # -retrieve data if input is not dataframe
  if (!is.data.frame(parameter)) {
    df <- importBC_data(parameter_or_station = parameter,years = years,flag_TFEE = TRUE,merge_Stations = merge_Stations,clean_names = TRUE)
  } else {
    # -entered parameters is already a dataframe
    df <- parameter %>%
      clean_names()
  }

  # -define the parameter
  param_data <- unique(df$parameter)

  # -get column names, define relevant columns
  cols_select <- c('station_name','date_pst','date','time','parameter','instrument','raw_value')
  df <- df %>%
    ungroup() %>%
    select(all_of(cols_select))
  cols <- colnames(df)

  # -add a grouping column
  if (is.null(groupby)) {
    if (tolower(param_data) == 'pm25') {
      groupby <- c('station_name','instrument')
    } else {
      groupby <- c('station_name')
    }
  }


  # - ensure group by is in columns, include parameter in grouping
  groupby <- c('parameter',groupby)
  group_var <- unique(groupby[groupby %in% cols])

  # -create a dataframe with complete start and stop dates

  min_date <- year(min(df$date))
  min_date <- ymd(paste(min_date,'-01-01',sep=''))

  max_date <- year(max(df$date))
  max_date <- ymd(paste(max_date,'-12-31',sep=''))

  # -limit to previous day if stop_at_present specified
  if (stop_at_present) {
    current_date <- Sys.Date() - days(1)
    max_date <- min(max_date,current_date)

    # -limit data to current date time
    df <- df %>%
      filter(date<=max_date)
  }

  # -define start and stop time

  datetimerange <- seq(from = ymd_hm(paste(min_date,'00:00')),
                       to = ymd_hm(paste(max_date,'23:00')), by= 'hour')

  df_complete <- tibble(date_time = datetimerange) %>%
    mutate(date_pst = datetimerange + hours(1)) %>%
    mutate(date = date(date_time))

  # -calculate hourly captures
  cap_complete <- df_complete %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(total_hrs = n())

  cap_hour <- df %>%
    filter(!is.na(raw_value)) %>%
    mutate(year = year(date)) %>%
    group_by_at(c(group_var,'year')) %>%
    summarise(valid_hrs = n()) %>%
    left_join(cap_complete) %>%
    mutate(perc_hrs = (valid_hrs/total_hrs)*100)

  # -calcualate daily captures
  cap_complete <- df_complete %>%
    mutate(year = year(date)) %>%
    select(date,year) %>% distinct() %>%
    group_by(year) %>%
    summarise(total_days = n())

  cap_days <-   df %>%
    filter(!is.na(raw_value)) %>%
    mutate(year = year(date)) %>%
    group_by_at(c(group_var,'year','date')) %>%
    summarise(valid_hrs = n()) %>%
    filter(valid_hrs >= 0.75*24) %>%
    ungroup() %>%
    group_by_at(c(group_var,'year')) %>%
    summarise(valid_days = n()) %>%
    left_join(cap_complete) %>%
    mutate(perc_days = (valid_days/total_days)*100)


  # -calculate  quarterly captures of valid days
  cap_complete <- df_complete %>%
    mutate(year = year(date),
           quarter = quarter(date)) %>%
    select(date,year,quarter) %>% distinct() %>%
    group_by(year,quarter) %>%
    summarise(total_days_quarter = n())

  cap_days_quarter <-       df %>%
    filter(!is.na(raw_value)) %>%
    mutate(year = year(date),
           quarter = quarter(date)) %>%
    group_by_at(c(group_var,'year','date','quarter')) %>%
    summarise(valid_hrs = n()) %>%
    filter(valid_hrs >= 0.75*24) %>%
    ungroup() %>%
    group_by_at(c(group_var,'year','quarter')) %>%
    summarise(valid_days_quarter = n()) %>%
    left_join(cap_complete) %>%
    mutate(perc_days_quarter = (valid_days_quarter/total_days_quarter)*100)

  # -calculate q2+q3
  cap_complete <- df_complete %>%
    mutate(year = year(date),
           quarter = quarter(date)) %>%
    select(date,year,quarter) %>% distinct() %>%
    group_by(year,quarter) %>%
    summarise(total_days_quarter = n()) %>%
    filter(quarter %in% c(2,3)) %>%
    group_by(year) %>%
    summarise(`total_q2+q3` = sum(total_days_quarter,na.rm = TRUE))

  cap_days_q2_q3 <-       df %>%
    filter(!is.na(raw_value)) %>%
    mutate(year = year(date),
           quarter = quarter(date)) %>%
    group_by_at(c(group_var,'year','date','quarter')) %>%
    summarise(valid_hrs = n()) %>%
    filter(valid_hrs >= 0.75*24) %>%
    ungroup() %>%
    group_by_at(c(group_var,'year','quarter')) %>%
    summarise(valid_days_quarter = n()) %>%
    filter(quarter %in% c(2,3)) %>%
    group_by_at(c('year',group_var)) %>%
    summarise(`q2+q3` = sum(valid_days_quarter,na.rm = TRUE)) %>%
    left_join(cap_complete) %>%
    mutate(`perc_q2+q3` = (`q2+q3`/`total_q2+q3`)*100)

  # -create a list of the results
  resultlist <- list()

  resultlist[['annual_hour']] <- cap_hour
  resultlist[['annual_days']] <-cap_days
  resultlist[['quarter_days']] <-cap_days_quarter
  resultlist[['quarter_q2+q3']] <-cap_days_q2_q3

  return(resultlist)
}
