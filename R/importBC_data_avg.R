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


#' Average the imported data function
#'
#' This function averages the imported data
#'
#' @param parameter is the parameter to average.use list_parameters() for list of parameters.
#' parameter can also be a dataframe from  importBC_data(). If dataframe, it will process and retrieve average.
#' @param years specifies the years to include. Use vector for multiple years. If NULL, it uses the current year.
#' @param averaging_type is the sub-annual values of either "1-hour", "24-hour", "8-hour","d1hm", "d8hm". If NULL, it applies the parameter default
#' @param data_threshold  value between 0 to 1. This refers to the data capture requirements.
#' Data will less than that data_threshold are excluded from the output.
#' If data_threshold=0, it will include ALL values of that averaging type and will include
#'
#' @examples
#' importBC_data_avg('o3')   #retrieves the daily 8-hour maximum for current year
#' importBC_data_avg('o3',years = 2015, averaging_type = 'd1hm',data_threshold = 0)   #displays the daily 1-hour maximum for ozone in 2015. It will display all data even if less than 75% of the day are not available.
#'
#' @export
importBC_data_avg <- function(parameter, years = NULL, averaging_type =  NULL, data_threshold = 0.75)
{
  if (0) {
    source('./r/importBC_data.R')
    source('./r/listBC_stations.R')
    source('./r/paddatafunction.R')
    parameter <- 'OZONE'
    years <- NULL
    averaging_type <- NULL
    data_threshold <- 0.75
  }

  require(dplyr)

  if (!is.data.frame(parameter)) {
    #rename the parameter entries
    parameter <- tolower(parameter)
    parameter <- gsub('ozone','o3',parameter)
    parameter <- gsub('pm2.5','pm25',parameter)
    parameter <- gsub('wdir','wdir_vect',parameter)
    parameter <- gsub('wspd','wspd_sclr',parameter)
    parameter <- gsub('rh','humidity',parameter)

    #define the default averaging type
    df_defaults <- tibble::tribble(
      ~parameter,~averaging_type,
      'pm25','24-hour',
      'o3','d8hm',
      'so2','d1hm',
      'no2','d1hm',
      'pm10','24-hour'
    )

    lst_averaging_types <- c(
      'd1hm','d8hm',
      '24-hour','24hr','24-hr','24','day','24h','24 hr','24 h',
      '1-hour','1hr','1-hr','1','hr','1h','hour',
      '8hr','8-hr','8h','8-hour','8 hr','8 h'
    )

    #assigning default values
    if (is.null(years)){
      years <- lubridate::year(Sys.Date())
    }

    if (is.null(averaging_type)) {
      averaging_type <- df_defaults$averaging_type[df_defaults$parameter == parameter]
      if (length(averaging_type) ==0) {averaging_type <- '1-hour'}
    }

    averaging_type <- tolower(averaging_type)


    #add extra year for d8hm
    if (averaging_type == 'd8hm') {
      years_ <-  c(min(years)-1,years)
    } else {
      years_ <- years
    }
    #retrieve data
    df <- importBC_data(parameter = parameter,
                        years = years_ )
  } else {

    print('Dataframe entered')
    df <- parameter
    #check if averaging_type is null
    if (is.null(averaging_type)) {
      print('Dataframe entry, please specify averaging_type')
      return(NULL) #stops if averaging_type not defined
    }
  }

  #add datetime_ columns for time-beginning processing


  if (averaging_type %in% c('1-hour','1hr','1-hr','1','hr','1h','hour'))
  {
    print('Calculating 1-hour values')
    df <-   df %>%
      pad_data()
    return(df)
  }

  if (averaging_type %in% c('24-hour','24hr','24-hr','24','day','24h','24 hr','24 h'))
  {

    print('Calculating 24-hour values')
    df <- df %>%
      ungroup() %>%
      filter(!is.na(RAW_VALUE)) %>%
      group_by(DATE,STATION_NAME,INSTRUMENT,PARAMETER) %>%
      dplyr::summarise(RAW_VALUE_24h = mean(RAW_VALUE),
                       ROUNDED_VALUE_24h = mean(ROUNDED_VALUE),
                       valid_hrs = n()) %>%
      filter(valid_hrs>=data_threshold * 24) %>%
      pad_data(date_time = 'DATE',padby='day',
               values = c('RAW_VALUE_24h','ROUNDED_VALUE_24h','valid_hrs'))

    if(data_threshold != 0) {
      df <- df %>% dplyr::select(-valid_hrs)
    }

    return(df)

  }

  if (averaging_type %in% c('d1hm')){
    print('Calculating daily 1-hour maximum values')
    df <- df %>%
      ungroup() %>%
      filter(!is.na(RAW_VALUE)) %>%
      group_by(DATE,STATION_NAME,INSTRUMENT,PARAMETER) %>%
      dplyr::summarise(RAW_VALUE_D1HM = max(RAW_VALUE),
                       ROUNDED_VALUE_D1HM = max(ROUNDED_VALUE),
                       valid_hrs = n()) %>%
      filter(valid_hrs>=data_threshold * 24) %>%
      pad_data(date_time = 'DATE',padby='day',
               values = c('RAW_VALUE_D1HM','ROUNDED_VALUE_D1HM','valid_hrs'))

    if(data_threshold != 0) {
      df <- df %>% dplyr::select(-valid_hrs)
    }

    return(df)
  }

  if (averaging_type %in% c('d8hm')){
    print('Calculating daily 8-hour maximums')
    cols <- colnames(df)

    #pre-defined column for grouping
    cols_select <- c('STATION_NAME','INSTRUMENT','PARAMETER')

    #check columns based on actuals
    cols_select <- unique(cols_select[cols_select %in% cols])



    #additional columns value and date columns
    cols_select_all <- c(cols_select,'DATE_PST','RAW_VALUE','ROUNDED_VALUE')
    cols_select_all <- unique(cols_select_all[cols_select_all %in% cols])

    df_ <-  df %>%
      ungroup() %>%
      select(cols_select_all)

    #duplicate previous 7 hours to group together
    for (i in 1:7) {

      print(paste('Calculating running average:',i,'/7',sep=''))
      df_ <- df_ %>%
        dplyr::bind_rows(
          df %>%
            dplyr::select(cols_select_all) %>%
            dplyr::mutate(DATE_PST = DATE_PST + lubridate::hours(i))
        )
    }


    #calculate running average
    df <- df_ %>%
      filter(!is.na(RAW_VALUE)) %>%
      dplyr::group_by(dplyr::across(c('DATE_PST',cols_select))) %>%
      dplyr::summarise(RAW_VALUE_8h = mean(RAW_VALUE),
                       ROUNDED_VALUE_8h = mean(ROUNDED_VALUE),
                       valid_n = n()) %>%
      dplyr::filter(valid_n>=6) %>%  #6 hrs at least
      dplyr::select(-valid_n)



    # get the daily maximum
    df <-  df %>%
      ungroup() %>%
      dplyr::mutate(datetime = DATE_PST-lubridate::hours(1)) %>%
      dplyr::mutate(DATE = as.character(datetime,format='%Y-%m-%d')) %>%
      dplyr::group_by(dplyr::across(c('DATE',cols_select))) %>%
      dplyr::summarise(RAW_VALUE_D8HM = max(RAW_VALUE_8h),
                       ROUNDED_VALUE_D8HM = max(ROUNDED_VALUE_8h),
                       valid_hrs =n()
      ) %>%
      ungroup() %>%
      dplyr::filter(valid_hrs>=data_threshold*24) %>%
      dplyr::filter(lubridate::year(DATE) %in% years)

    if (data_threshold != 0)  {
      df <- df %>%
        select(-valid_hrs)
    }

    df <- df %>%
      pad_data(date_time = 'DATE',
               values =c('RAW_VALUE_D8HM','ROUNDED_VALUE_D8HM','valid_hrs'),
               padby = 'day')


    return(df)

  }

  if (averaging_type %in% c('8hr','8-hr','8h','8-hour','8 hr','8 h')) {

    print('Calculating 8-hour running average')
    cols <- colnames(df)

    #pre-defined column for grouping
    cols_select <- c('STATION_NAME','INSTRUMENT','PARAMETER')

    #check columns based on actuals
    cols_select <- unique(cols_select[cols_select %in% cols])



    #additional columns value and date columns
    cols_select_all <- c(cols_select,'DATE_PST','RAW_VALUE','ROUNDED_VALUE')
    cols_select_all <- unique(cols_select_all[cols_select_all %in% cols])

    df_ <-  df %>%
      ungroup() %>%
      select(cols_select_all)

    #duplicate previous 7 hours to group together
    for (i in 1:7) {

      print(paste('Calculating running average:',i,'/7',sep=''))
      df_ <- df_ %>%
        dplyr::bind_rows(
          df %>%
            dplyr::select(cols_select_all) %>%
            dplyr::mutate(DATE_PST = DATE_PST + lubridate::hours(i))
        )
    }


    #calculate running average
    df <- df_ %>%
      filter(!is.na(RAW_VALUE)) %>%
      dplyr::group_by(dplyr::across(c('DATE_PST',cols_select))) %>%
      dplyr::summarise(RAW_VALUE_8h = mean(RAW_VALUE),
                       ROUNDED_VALUE_8h = mean(ROUNDED_VALUE),
                       valid_n = n()) %>%
      dplyr::filter(valid_n>=data_threshold*8)


    if (data_threshold != 0)  {
      df <- df %>%
        select(-valid_n)
    }

    df <- df %>%
      pad_data(values =c('RAW_VALUE_8h','ROUNDED_VALUE_8h','valid_n'),
               padby = 'hour',add_DATETIME = TRUE)



    return(df)
  }

  #function would have exit before this point

  print(paste(averaging_type,'is not in the list of averaging types. Use d1hm, d8hm, 1-hour, or 24-hour'))
  return(NULL)

}
