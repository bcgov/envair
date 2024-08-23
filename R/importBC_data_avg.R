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
#' @param averaging_type is the sub-annual values of either "1-hour", "24-hour", "8-hour","d1hm", "d8hm"
#' You can also request annual summaries, percentiles. Add "annual <annual stat> <on what data>": 'annual 98p 24h','annual 99p d1hm','annual xxp 1hr';'annual mean 1-hr';
#' 'annual mean 24-hr';'annual 4th d1hm'
#' It can also calculate the number of exceedance to a specified value "exceedance 25.00 d1hm".
#' Function will round off based on the precision of the number entered after exceedance
#'  If NULL, it applies either the 1-hour, 8-hour, or 24-hour, depending on typical default for
#' @param data_threshold  value between 0 to 1. This refers to the data capture requirements for the hourly to daily values.
#' Data will less than that data_threshold are excluded from the output.
#' This excludes the values
#' If data_threshold=0, it will include ALL values of that averaging type and will include
#' @param flag_TFEE default is FALSE. If TRUE, it will evaluate data with TFEE adjustment and include it in the result.
#' The resultig column will have _TFEE for the TFEE-adjusted result
#' @param merge_stations default is FALSE. If TRUE, data from stations and their alternatives are merged. Alternative stations
#' are defined when station has relocated to a nearby area
#'
#' @examples
#' importBC_data_avg('o3')   #retrieves the daily 8-hour maximum for current year
#' importBC_data_avg('o3',years = 2015, averaging_type = 'd1hm',data_threshold = 0)   #displays the daily 1-hour maximum for ozone in 2015. It will display all data even if less than 75% of the day are not available.
#'
#' @return A wide dataframe listting the date, station name, instrument, parameter, and then the raw and rounded values with underscore
#' that specified if 24hour, and if adjusted for tfee
#' @export
importBC_data_avg <- function(parameter, years = NULL, averaging_type =  NULL, data_threshold = 0.75,
                              flag_TFEE = FALSE,merge_stations = FALSE) {

  if (0) {
    source('./r/importBC_data.R')
    source('./r/listBC_stations.R')
    source('./r/paddatafunction.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/envairfunctions.R')
    source('./r/importBC_data_avg.R')
    source('./r/parallel_process.R')

    parameter <- c('pm25')

    years <- 2023
    # averaging_type <- c('annual mean 1hr','annual mean 24h')
    averaging_type = 'exceedance 25 24h'
    data_threshold <- 0.75
    merge_stations <- TRUE
    flag_TFEE = TRUE


  }

  require(dplyr)
  require(lubridate)
  require(tidyr)
  # -check if parameter exists
  parameter <- tolower(parameter)
  lst_params <- list_parameters()
  if (!any(parameter %in% lst_params)) {
    message('Parameter not found. Please use list_parameters()')
    return(NULL)
  }

  #define the default averaging type
  #will be used if not defined, averaging type is null
  df_defaults <- tibble::tribble(
    ~parameter,~averaging_type,
    'pm25',c('annual mean 24hr','annual 98p 24hr'),
    'o3','d8hm',
    'so2',c('annual 99p d1hm','annual mean 1hr'),
    'no2',c('annual 98p d1hm','annual mean 1hr'),
    'pm10',c('annual mean 24hr')
  )

  # -retrieve default avaeraging types if this was not specified
  if (is.null(averaging_type)) {

    df_averaging_type_ <- df_defaults %>%
      ungroup() %>%
      filter(parameter %in% !!parameter)

    averaging_type <- NULL
    for (i in 1:nrow(df_averaging_type_)) {
      averaging_type <- c(averaging_type,
                          df_averaging_type_$averaging_type[[i]])
    }

    averaging_type <- unique(averaging_type)
  }
  #standardize the names of user entry
  averaging_type <- tolower(averaging_type)
  averaging_type <- gsub('max','1st',averaging_type)
  averaging_type <- gsub('excess','EXCEED',averaging_type,ignore.case = TRUE)
  averaging_type <- gsub('over','EXCEED',averaging_type,ignore.case = TRUE)
  averaging_type <- gsub('more','EXCEED',averaging_type,ignore.case = TRUE)
  averaging_type <- gsub('avg','mean',averaging_type,ignore.case = TRUE)
  averaging_type <- gsub('average','mean',averaging_type,ignore.case = TRUE)
  averaging_type <- gsub('ave','mean',averaging_type,ignore.case = TRUE)


  # -retrieve result one averaging_type at a time
  df_result <- NULL
  for (averaging_type_ in averaging_type) {
    message(paste('Calculating:',averaging_type_))
    df <- importBC_data_avg0(parameter = parameter,years = years,averaging_type = averaging_type_,
                             data_threshold =data_threshold,flag_tfee = flag_TFEE,merge_stations= merge_stations)

    message(paste('retrieved data complete. added rows:',nrow(df)))
    cols <- colnames(df)
    cols_values <- cols[grepl('raw|rounded|exceed',cols,ignore.case = TRUE)]
    df <- df %>%
      pivot_longer(cols = cols_values)
    gc()
    df_result <- bind_rows(df_result,df)
    gc()
  }

  df_result <- df_result %>%
    pivot_wider()
  gc()
  return(df_result)
}



#' Backend function of importBC_data_avg_
#'
#' Note that averaging_type here is the simple (not compounded) format.
importBC_data_avg_ <- function(df, averaging_type, data_threshold = 0.75) {

  if (0) {
    df <- importBC_data('pm25',2015,clean_names = TRUE)
    years = NULL
    averaging_type =  '24h'
    data_threshold = 0.75
  }

  require(lubridate)

  # -add datetime_ columns for time-beginning processing
  df$date_time = df$date_pst - hours(1)
  years = sort(unique(year(df$date_time)))

  #list of alias names for the averaging_type
  #should be in order of significance
  df_avg_types <- tidyr::tribble(
    ~averaging_type_std,~alias,
    'd1hm','d1hm',
    'd1hm','daily1hr',
    'd1hm','daily1hrmax',
    'd8hm','d8hm',
    'd8hm','daily8hr',
    'd8hm','daily8hrmax',
    '24hr','24hr',
    '24hr','24-hour',
    '24hr','24 h',
    '24hr','24h',
    '24hr','day',
    '24hr','days',
    '24hr','24hour',
    '1hr','1hr',
    '1hr','1-hour',
    '1hr','1-hr',
    '1hr','1',
    '1hr','hr',
    '1hr','1h',
    '1hr','hour',
    '8hr','8hr',
    '8hr','8hour',
    '8hr','8-hr',
    '8hr','8h',
    '8hr','8-hour',
    '8hr','8 hr',
    '8hr','8 h'
  )

  #if user enter "max" change to 1st
  averaging_type <- gsub('max','1st',averaging_type,ignore.case = TRUE)

  #convert averaging_type to standard format
  if (!averaging_type %in% c('d1hm','d8hm','24hr','1hr','8hr')) {
    for (i in 1:nrow(df_avg_types)) {
      if (grepl(df_avg_types$alias[i],averaging_type,ignore.case=TRUE)) {
        averaging_type <- df_avg_types$averaging_type_std[i]
        break
      }
    }}


  if (averaging_type %in% c('1hr'))
  {
    message('Calculating 1-hour values')
    df <-   df %>%
      dplyr::rename(raw_value_1hr = raw_value,
                    rounded_value_1hr = rounded_value)
    return(df)
  }

  if (averaging_type %in% c('24hr'))
  {

    message('Calculating 24-hour values')
    df <-     df %>%
      ungroup() %>%
      filter(!is.na(raw_value)) %>%
      group_by(date,station_name,instrument,parameter) %>%
      dplyr::summarise(raw_value_24h = mean(raw_value),
                       rounded_value_24h = mean(rounded_value),
                       valid_hrs = n()) %>%
      filter(valid_hrs>=data_threshold * 24) %>%
      pad_data(date_time = 'date',padby='day',
               values = c('raw_value_24h','rounded_value_24h','valid_hrs'))

    if(data_threshold != 0) {
      df <- df %>% dplyr::select(-valid_hrs)
    }

    return(df)

  }

  if (averaging_type %in% c('d1hm')){
    message('Calculating daily 1-hour maximum values')
    df <- df %>%
      ungroup() %>%
      filter(!is.na(raw_value)) %>%
      group_by(date,station_name,instrument,parameter) %>%
      dplyr::summarise(raw_value_d1hm = max(raw_value),
                       rounded_value_d1hm = max(rounded_value,na.rm = TRUE),
                       valid_hrs = n()) %>%
      filter(valid_hrs>=data_threshold * 24) %>%
      pad_data(date_time = 'date',padby='day',
               values = c('raw_value_d1hm','rounded_value_d1hm','valid_hrs'))

    if(data_threshold != 0) {
      df <- df %>% dplyr::select(-valid_hrs)
    }

    return(df)
  }

  if (averaging_type %in% c('d8hm')){
    message('Calculating daily 8-hour maximums')
    cols <- colnames(df)

    #pre-defined column for grouping
    cols_select <- c('station_name','instrument','parameter')

    #check columns based on actuals
    cols_select <- unique(cols_select[cols_select %in% cols])



    #additional columns value and date columns
    cols_select_all <- c(cols_select,'date_pst','raw_value','rounded_value')
    cols_select_all <- unique(cols_select_all[cols_select_all %in% cols])

    df_ <-  df %>%
      ungroup() %>%
      select(all_of(cols_select_all))

    #duplicate previous 7 hours to group together
    for (i in 1:7) {

      message(paste('Calculating running average:',i,'/7',sep=''))
      gc()
      df_ <- df_ %>%
        dplyr::bind_rows(
          df %>%
            dplyr::select(cols_select_all) %>%
            dplyr::mutate(date_pst = date_pst + lubridate::hours(i))
        )
    }

    gc() #-clear memory

    #calculate running average
    df <- df_ %>%
      filter(!is.na(raw_value)) %>%
      dplyr::group_by(dplyr::across(c('date_pst',cols_select))) %>%
      dplyr::summarise(raw_value_8h = mean(raw_value),
                       rounded_value_8h = mean(rounded_value),
                       valid_n = n()) %>%
      dplyr::filter(valid_n>=6) %>%  #6 hrs at least
      dplyr::select(-valid_n)

    gc()

    # get the daily maximum
    df <- df %>%
      ungroup() %>%
      dplyr::mutate(datetime = date_pst-lubridate::hours(1)) %>%
      dplyr::mutate(date = format(datetime,'%Y-%m-%d')) %>%
      dplyr::group_by(dplyr::across(c('date',cols_select))) %>%
      dplyr::summarise(raw_value_d8hm = max(raw_value_8h),
                       rounded_value_d8hm = max(rounded_value_8h),
                       valid_hrs =n()
      ) %>%
      ungroup() %>%
      dplyr::filter(valid_hrs>=data_threshold*24) %>%
      dplyr::filter(lubridate::year(date) %in% years)

    if (data_threshold != 0)  {
      df <- df %>%
        select(-valid_hrs)
    }

    gc()
    df <- df %>%
      pad_data(date_time = 'date',
               values =c('raw_value_d8hm','rounded_value_d8hm','valid_hrs'),
               padby = 'day')


    return(df)

  }

  if (averaging_type %in% c('8hr')) {

    message('Calculating 8-hour running average')
    cols <- colnames(df)

    #pre-defined column for grouping
    cols_select <- c('station_name','instrument','parameter')

    #check columns based on actuals
    cols_select <- unique(cols_select[cols_select %in% cols])



    #additional columns value and date columns
    cols_select_all <- c(cols_select,'date_pst','raw_value','rounded_value')
    cols_select_all <- unique(cols_select_all[cols_select_all %in% cols])

    df_ <-  df %>%
      ungroup() %>%
      select(all_of(cols_select_all))

    #duplicate previous 7 hours to group together
    for (i in 1:7) {

      message(paste('Calculating running average:',i,'/7',sep=''))
      df_ <- df_ %>%
        dplyr::bind_rows(
          df %>%
            dplyr::select(cols_select_all) %>%
            dplyr::mutate(date_pst = date_pst + lubridate::hours(i))
        )
    }


    #calculate running average
    df <- df_ %>%
      filter(!is.na(raw_value)) %>%
      dplyr::group_by(dplyr::across(c('date_pst',cols_select))) %>%
      dplyr::summarise(raw_value_8h = mean(raw_value),
                       rounded_value_8h = mean(rounded_value),
                       valid_n = n()) %>%
      dplyr::filter(valid_n>=data_threshold*8)


    if (data_threshold != 0)  {
      df <- df %>%
        select(-valid_n)
    }

    df <- df %>%
      pad_data(values =c('raw_value_8h','rounded_value_8h','valid_n'),
               padby = 'hour',add_dateTIME = TRUE)



    return(df)
  }

  #function would have exit before this point

  message(paste(averaging_type,'is not in the list of averaging types. Use d1hm, d8hm, 1-hour, or 24-hour'))
  return(NULL)

}

#' Back end function, see importBC_data_avg for main script
#'
#' This function is capable of compoung averaging_type (e.g., "annual 98p d1hm")
#' But not ideal for multiple years or parameters
#'
importBC_data_avg0 <- function(parameter, years = NULL, averaging_type, data_threshold = 0.75,
                               flag_tfee = FALSE,merge_stations = FALSE)
{
  if (0) {
    source('./r/importBC_data.R')
    source('./r/listBC_stations.R')
    source('./r/paddatafunction.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/envairfunctions.R')
    # parameter <- 'pm25'
    parameter <- 'no2'
    years <- 2020
    averaging_type <- 'annual 98p 24hour'
    data_threshold <- 0.75
    merge_stations <- FALSE
    flag_tfee = FALSE

  }


  require(dplyr)
  require(lubridate)

  # -retrieve data if parameter entered
  # -process dataframe if dataframe entered
  # -output is df
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



    #assigning default values
    if (is.null(years)){
      years <- lubridate::year(Sys.Date())
    }

    if (is.null(averaging_type)) {
      averaging_type <- df_defaults$averaging_type[df_defaults$parameter == parameter]
      if (length(averaging_type) ==0) {averaging_type <- '1-hour'}
    }

    averaging_type <- tolower(averaging_type)

    # - standardize the averaging type
    averaging_type <- gsub('excess','EXCEED',averaging_type,ignore.case = TRUE)
    averaging_type <- gsub('over','EXCEED',averaging_type,ignore.case = TRUE)
    averaging_type <- gsub('more','EXCEED',averaging_type,ignore.case = TRUE)
    averaging_type <- gsub('avg','mean',averaging_type,ignore.case = TRUE)
    averaging_type <- gsub('average','mean',averaging_type,ignore.case = TRUE)
    averaging_type <- gsub('ave','mean',averaging_type,ignore.case = TRUE)


    # -rename the averaging type to standard
    # -identify if calculate for exceedance or annual (mean, percentiles, rank)
    if (grepl('annual|exceed',averaging_type,ignore.case = TRUE))
    {
      is_annual <- TRUE

      #splits the averaging _type, separated by space
      str_avg <- unlist(stringr::str_split(averaging_type,' '))
      if (length(str_avg) !=3)
      {
        message('Please check averaging_type= value. Type ?importBC_data_avg for details')
        return(NULL)
      }
      averaging_method <- str_avg[3]
      annual_summary <- str_avg[2]


    } else
    {
      is_annual <- FALSE
      averaging_method <- averaging_type
      annual_summary <- NA
    }


    #add extra year for d8hm
    if (grepl('hm',averaging_method,ignore.case = TRUE) &
        averaging_method != 'd1hm'
    ) {
      years_ <-  c(min(years)-1,years)
    } else {
      years_ <- years
    }
    #retrieve data
    df <- importBC_data(parameter_or_station = parameter,
                        years = years_,
                        flag_TFEE = flag_tfee,
                        merge_Stations = merge_stations,
                        clean_names = TRUE)

    # -remove instrument name when it is not PM
    df$instrument[!grepl('pm',df$parameter,ignore.case = TRUE)] <- 'UNSPECIFIED'



    # -add value to flag_TFEE even if not to be included
    if (!flag_tfee) {
      df$flag_tfee <- FALSE
    }
  } else {

    message('Dataframe data entered')
    df <- clean_names(parameter)
    parameter <- unique(df$parameter)
    #get years based on the dataframe
    years <- unique(year(ymd(df$date)))

    #identify if there is TFEE flag in the data
    #then redefine add_TFEE
    flag_tfee <- 'flag_tfee' %in% colnames(df)


    #check if averaging_type is null
    if (is.null(averaging_type)) {
      message('Dataframe entry, please specify averaging_type')
      return(NULL) #stops if averaging_type not defined
    }
  }

  # -cleanup, remove unneeded columns, add date, time columns
  cols <- colnames(df)
  if (!all(c('date','time') %in% cols)) {
    df$date_time  <-  df$date_pst - hours(1)
    df$date <-  date(df$date_time)
    df$time <-  format(df$date_pst,'%H:00')
    df$time <- gsub('00:00','24:00',df$time)
  }

  df <- df %>%
    select(parameter,station_name,instrument,date_pst,date,time,rounded_value,raw_value,flag_tfee)



  df_result <- NULL

  #retrieve data without TFEE
  suppressWarnings(
    df_no_tfee <- importBC_data_avg_(df, averaging_type =  averaging_method, data_threshold = data_threshold)
  )
  if (!flag_tfee) {
    message('sending result with no TFEE')
    df_result <- df_no_tfee
  } else {

    df_tfee <- df %>%
      filter(!flag_tfee) %>% #remove data with TFEE Flags
      # View()
      importBC_data_avg_(averaging_type =  averaging_method, data_threshold = data_threshold)

    # -rename the column names to include tfee and no tfee
    colnames(df_tfee)[grepl('_value',colnames(df_tfee))] <- paste(colnames(df_tfee)[grepl('_value',colnames(df_tfee))],
                                                                  '_tfee',sep='')
    message('sending result with TFEE')
    # print(colnames(df_tfee))
    df_result <- df_no_tfee %>%
      dplyr::left_join(df_tfee)
  }

  #exit if null
  if (is.null(df_result)) {
    return(NULL)
  }

  # -filter for the year specified
  # -cleanup: change column names
  df_result$year <- lubridate::year(ymd(df_result$date))
  df_result <- df_result[df_result$year %in% years,]
  cols <- colnames(df_result)
  cols_values <- cols[grepl('value',cols)]

  if (is_annual) {
    message('processing annual statistics')


    cols <- colnames(df_result)
    cols_values <- cols[grepl('value',cols,ignore.case = TRUE)]

    if (grepl('exceed',averaging_type,ignore.case = TRUE)) {


      #if this is calculation of exceedances
      #obtain the precision based on the entered value
      #check if user entered a period on the value
      annual_summary <- as.character(annual_summary)
      if (grepl('.',annual_summary)) {
        precision <- unlist(stringr::str_split(annual_summary,pattern = '\\.'))[2]
        precision <- length(precision)
      } else {
        #take only whole numbers
        precision <- 0
      }

      message(paste('Calculating the number of times above',annual_summary,'with precision of',precision))
      cols_widen <- cols[grepl('value',cols,ignore.case = TRUE)]
      annual_summary <- as.numeric(annual_summary)
      df_result <- df_result %>%
        tidyr::pivot_longer(cols = cols_widen) %>%
        filter(!grepl('rounded',name,ignore.case = TRUE)) %>%
        mutate(value = round2(value,n=precision)) %>%
        dplyr::mutate(excess_count = ifelse(value>annual_summary,1,0)) %>%
        group_by(year,station_name,instrument,parameter,name) %>%
        dplyr::summarise(exceed = sum(excess_count,na.rm = TRUE)) %>%
        dplyr::mutate(name = gsub('value',
                                  paste('(>',annual_summary,')',sep=''),
                                  name,ignore.case = TRUE)) %>%
        dplyr::mutate(name = gsub('raw_','exceed',name,ignore.case = TRUE)) %>%
        tidyr::pivot_wider(names_from = name,values_from = exceed)

    }

    # -calculatio for percentiles
    # -follows the procedures outlined in the CCME, which is based on ranked order
    if (grepl('p',annual_summary,ignore.case = TRUE)) {
      message('Calculating the percentiles of the year')
      quantile <- as.numeric(gsub('p','',annual_summary,ignore.case = TRUE))/100


      df_result <-  df_result %>%
        tidyr::pivot_longer(cols =cols_values) %>%
        group_by(parameter,year,station_name,instrument,name) %>%
        filter(!is.na(value)) %>%
        arrange(desc(value)) %>%
        dplyr::mutate(quant_idx=ceiling(n()*(1-quantile)),count=n(),index=1:n()) %>%
        filter(index == quant_idx) %>%
        select(-c(quant_idx,count,index,date)) %>%

        # -old method using quantile() replaced by CCME method
        # arrange(parameter,station_name,instrument,year,name)
        # dplyr::summarise(value = stats::quantile(value,probs = quantile,na.rm = TRUE,type = 2)) %>%

        dplyr::mutate(name = gsub('value',annual_summary,name,ignore.case = TRUE)) %>%
        dplyr::mutate(name = gsub(annual_summary,
                                  paste(tolower(annual_summary),sep='_'),
                                  name,ignore.case = TRUE)) %>%
        tidyr::pivot_wider() %>%
        arrange(parameter,station_name,instrument,year)
    }

    #for mean

    if (grepl('mean',annual_summary,ignore.case = TRUE)) {
      message('Calculating the average values of the year')
      df_result <- df_result %>%
        ungroup() %>%
        tidyr::pivot_longer(cols =cols_values) %>%
        group_by(parameter,year,station_name,instrument,name) %>%
        dplyr::summarise(value = mean(value,na.rm = TRUE)) %>%
        dplyr::mutate(name = gsub('value',annual_summary,name,ignore.case = TRUE)) %>%
        dplyr::mutate(name = gsub(annual_summary,
                                  paste(tolower(annual_summary),sep='_'),
                                  name,ignore.case = TRUE)) %>%
        tidyr::pivot_wider()
    }

    #for nth highest,standardize number suffix (-rd,-th,-st)
    if (gsub('[0-9]+', '', annual_summary) %in% c('rd','th','st','nd')) {


      nth_order <- stringr::str_extract(annual_summary,'\\d+')
      nth_order <- as.numeric(nth_order)
      message(paste('Calculating the nth highest of the year. nth =',nth_order))
      cols_remove <- cols[!cols %in% c('station_name','instrument','parameter','year',cols_values)]

      df_result <-   df_result %>%
        tidyr::pivot_longer(cols =cols_values) %>%
        filter(!is.na(value)) %>%
        arrange(desc(value)) %>%
        dplyr::select(-cols_remove) %>%
        group_by(parameter,year,station_name,instrument,name) %>%
        slice(nth_order) %>%
        distinct() %>%
        dplyr::mutate(name = gsub('value',annual_summary,name,ignore.case = TRUE)) %>%
        dplyr::mutate(name = gsub(annual_summary,
                                  paste(tolower(annual_summary),sep='_'),
                                  name,ignore.case = TRUE)) %>%
        arrange(name,year,station_name) %>%
        tidyr::pivot_wider()
    }


  }

  return(ungroup(df_result))
}
