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
#' @param parameter is the pollutant(param). Use list_parameters() to list availble parameters.
#' @param years is the year or years. You can use vectors to query multiple years. If NULL, it will be the current year
#' @param adjust default is FALSE. if TRUE, it will combine some stations/instruments
#' based on the merging requirement applies in CAAQS
#' @param stop_at_present default is TRUE. It will remove values after the present date/time
#' The present date/time is determined from the latest PM2.5 data
#' @examples
#' get_captures('o3',years = 2017:2020)
#'
#' @export
get_captures <- function(parameter,years=NULL,merge_Stations=FALSE,stop_at_present = TRUE) {
  if (0) {
    parameter <- 'no2'
    years <- 2015
    merge_Stations <- TRUE
    stop_at_present = TRUE

    source('./r/importbc_data.R')
    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/envairfunctions.R')
    source('./r/importBC_data_avg.R')
    source('./r/get_data_completeness.R')
  }




  captures <- get_data_completeness(parameter = parameter,years = years, merge_Stations = merge_Stations,stop_at_present = stop_at_present )

  # -combine completeness results into one table
  # -also add a table that shows if passed or not

  # -retrieve data
  df_annual_hour <- captures$annual_hour
  df_annual_days <- captures$annual_days
  df_quarter_days <- captures$quarter_days
  df_q2q3 <- captures$`quarter_q2+q3`


  # -define grouping options
  # -removing grouping for non-PM by changing instrument name
  if (!grepl('pm',parameter,ignore.case = TRUE) ) {
    df_annual_hour$instrument <- NA
    df_annual_days$instrument <- NA
    df_quarter_days$instrument <- NA
    df_q2q3$instrument <- NA
  }

  # -assess annual daily completeness
  df_annual_days <- df_annual_days %>%
    mutate(annual_valid = ifelse(perc_days>=75,TRUE,FALSE))

  # assessment of quarter captures
  df_quarter_days <-   df_quarter_days %>%
    mutate(quarter = paste('Q',quarter,sep='')) %>%
    select(parameter,station_name,instrument,year,quarter,perc_days_quarter) %>%
    pivot_wider(names_from = quarter, values_from = c(perc_days_quarter)) %>%
    pivot_longer(cols = -c(parameter,station_name,instrument,year)) %>%
    mutate(value = ifelse(is.na(value),0,value)) %>%
    group_by(parameter,station_name,instrument,year) %>%
    dplyr::mutate(min_valid = min(value),
                  name = paste(name,'_perc',sep='')) %>%
    mutate(quarter_valid = ifelse(min_valid>=60,TRUE,FALSE)) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    ungroup %>%
    select(parameter,station_name,instrument,year,
           Q1_perc,Q2_perc,Q3_perc,Q4_perc,quarter_valid )

  # -assessment of q2 + q3
  df_q2q3 <- df_q2q3 %>%
    select(year,parameter,station_name,instrument,`perc_q2+q3`) %>%
    mutate(`q2+q3_valid` = ifelse(`perc_q2+q3`>=0.75,TRUE,FALSE))

  df_result <- df_annual_hour %>%
    left_join(df_annual_days) %>%
    left_join(df_quarter_days) %>%
    left_join(df_q2q3)

  # -rearrange columns, have all the valid booleans at the end
  cols <- colnames(df_result)

  cols_valid <- cols[grepl('_valid',cols)]
  cols <- cols[!cols %in% cols_valid]
  cols <- c(cols,cols_valid)

  df_result <- df_result %>%
    select(any_of(cols)) %>%
    COLUMN_REORDER(c('parameter','year','station_name','instrument'))

  message('DONE. Data capture and completeness retrieved.')
  return(df_result)
}
