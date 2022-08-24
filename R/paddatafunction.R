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

#' Pad data to completely represent entire years
#'
#' This function inserts fillers to data set. The function can identify separate columns of DATE and TIME
#'
#' @param df is the dataframe containing date_time parameters, and values columns
#' @param date_time defines the date_time column. Default is DATE_PST
#' @param values defines the columns with the reported measurements. If NULL, it wil use RAW_VALUE and ROUNDED_VALUE
#' @param time_ending if TRUE means date starts AFTER and EXCLUDES midnight
#' @param add_DATETIME if TRUE, means to force and include DATETIME
#'
#' @export
pad_data <- function(df, date_time = NULL,values = NULL,time_ending = TRUE,add_DATETIME = NULL)
{
  if (0)  {
    df <- readRDS('./test_data/raw_data.Rds')
    # df <- envair::importBC_data('no2',2020)
    df <- data.result
    date_time <- NULL
    values <- NULL
    time_ending <- TRUE
    add_DATETIME = TRUE

    df <- data.result
    date_time <-  c('date','DATE_PST')[c('date','DATE_PST') %in% cols_]
    values = c(cols_vals,cols_instrument,cols_unit)
    time_ending = !use_openairformat
    add_DATETIME = FALSE
  }

  require(dplyr)

  print('padding the data')

  df <- ungroup(df)
  cols_ <- colnames(df)

  if (is.null(add_DATETIME)) {
    # find out if separate DATE and TIME columns are included
    separate_DATETIME <- all(c('DATE','TIME') %in% cols_)
  } else {
    separate_DATETIME <- add_DATETIME
  }



  # assign values for default entries
  if (is.null(date_time)){
    date_time <- cols_[cols_ %in% c('DATE_PST','datetime','date_time',
                                    'date_pst','DATEPST')]
    date_time <- date_time[1]
  }
  #check if special condition for importBC_data, where use_openair
  #need to rename away from "date" as date_time column
  if (date_time == 'date') {
    special_openairformat <- TRUE
    date_time <- 'date_pst'
    values <- c(values,'ws','wd')
    add_DATETIME <- FALSE
    separate_DATETIME <- FALSE
    df <- df %>%
      dplyr::rename(date_pst = date)
    cols_ <- colnames(df)
    time_ending <- FALSE

  } else {
    special_openairformat <- FALSE
  }

  if (is.null(values)) {
    values <- cols_[cols_ %in% c('RAW_VALUE','ROUNDED_VALUE','metric_value')]
  }


  cols_selection <- c(cols_[!cols_ %in% c(values,date_time)])

  if (separate_DATETIME) {
    cols_selection <- cols_selection[!cols_selection %in% c('DATE','TIME')]
    try(df <- df %>%
      dplyr::select(-DATE,-TIME), silent = TRUE)
  }

  #convert all date and time to time-beginning
  if (time_ending) {
    df[[date_time]] <- df[[date_time]] - lubridate::hours(1)

  }

  #special cse for use_openairformat

  lst_datetime <- df %>%
    pull(date_time)

  start_date <- lubridate::ymd_hm(paste(lubridate::year(min(lst_datetime)),'-01-01 00:00',sep=''), tz='etc/GMT+8')
  end_date <- lubridate::ymd_hm(paste(lubridate::year(max(lst_datetime)),'-12-31 23:00',sep=''), tz='etc/GMT+8')

  df_datetime <- dplyr::tibble(!!date_time := seq.POSIXt(from = start_date, to=end_date , by='hour'))

  if (separate_DATETIME) {
    df_datetime$DATE <- lubridate::date(df_datetime[[date_time]])
    df_datetime$TIME <- lubridate::hour(df_datetime[[date_time]])

    if (time_ending)
    {
      df_datetime$TIME <- df_datetime$TIME +1
      df_datetime$TIME <- paste(df_datetime$TIME,':00',sep='')
    }
  }

  df_result <- df_datetime %>%
    merge(
      df %>%
        dplyr::select(cols_selection) %>%
        unique()
    ) %>%
    dplyr::left_join(df) %>%
    arrange(date_time)

  #convert back the time if time ending
  if (time_ending) {
    df_result[[date_time]] <- df_result[[date_time]] + lubridate::hours(1)

  }

  #fix back for the special case scenario

  if (special_openairformat) {

    df_result <- df_result %>%
      dplyr::rename(date = date_pst)
  }

  print(paste('Added/padded rows:',nrow(df_result) - nrow(df)))
  return(df_result)
}
