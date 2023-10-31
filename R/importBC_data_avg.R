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
#' @param data_threshold  value between 0 to 1. This refers to the data capture requirements.
#' Data will less than that data_threshold are excluded from the output.
#' If data_threshold=0, it will include ALL values of that averaging type and will include
#' @param flag_TFEE default is FALSE. If TRUE, it will evaluate data with TFEE adjustment and include it in the result.
#' The resultig column will have _TFEE for the TFEE-adjusted result
#' @param merge_Stations default is FALSE. If TRUE, data from stations and their alternatives are merged. Alternative stations
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
                              flag_TFEE = FALSE,merge_Stations = FALSE) {

  if (0) {
    source('./r/importBC_data.R')
    source('./r/listBC_stations.R')
    source('./r/paddatafunction.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/envairfunctions.R')
    source('./r/importBC_data_avg.R')
    parameter <- 'O3'

    years <- 1990:2022
    # averaging_type <- c('annual mean 1hr','annual mean 24h')
    averaging_type = 'd8hm'
    data_threshold <- 0.75
    merge_Stations <- TRUE
    flag_TFEE = TRUE


  }

  #include instrument in grouping
  #for these parameters, the instruments are considered
  do_not_mergeauto <- c('PM25','PM10')


  #standardize the names of user entry
  averaging_type <- tolower(averaging_type)
  averaging_type <- gsub('max','1st',averaging_type)

  #this main script was made to reduce memory usage when multiple year and averaging types are entered
  # applies where annual or exceedance are specified

  if (any(grepl('annual',averaging_type,ignore.case = TRUE)) |
      any(grepl('exceed',averaging_type,ignore.case = TRUE))) {

    averaging_type <- unique(averaging_type)

    #non-value data (metadata)
    cols_meta <- c('PARAMETER','YEAR','STATION_NAME','INSTRUMENT')


    df_result <- NULL
    for (year in years) {

      #consideration for d8hm, and rolling 8-hours, need to include previous year data
      if (any(grepl('8h',averaging_type,ignore.case = TRUE)) |
          any(grepl('8 h',averaging_type,ignore.case = TRUE))) {
        year_ <- (year-1):year

      } else {
        year_ <- year
      }


      df_data <- importBC_data(parameter = parameter,years = year_,
                               flag_TFEE = flag_TFEE,merge_Stations = merge_Stations,clean_names = FALSE)

      # auto-merge instrument, for stations that merged
      if (any(!df_data$PARAMETER %in% do_not_mergeauto) & merge_Stations) {
        df_instrument <- df_data %>%
          select(STATION_NAME,INSTRUMENT,PARAMETER) %>%
          distinct() %>%
          group_by(STATION_NAME,PARAMETER) %>%
          dplyr::mutate(new_INSTRUMENT = paste(INSTRUMENT,collapse ='/')) %>%
          distinct()

        df_data <- df_data %>%
          left_join(df_instrument) %>%
          mutate(INSTRUMENT_ORIGINAL = ifelse(INSTRUMENT != new_INSTRUMENT,INSTRUMENT,INSTRUMENT_ORIGINAL),
                 INSTRUMENT = new_INSTRUMENT) %>%
          select(-new_INSTRUMENT)
      }


      for (avg_ in averaging_type) {
        message(paste('Calculating:',parameter,year,avg_))
        df_ <- importBC_data_avg0(df_data,averaging_type = avg_,data_threshold = data_threshold,
                                  flag_TFEE = flag_TFEE, merge_Stations = merge_Stations) %>%
          filter(YEAR == year)

        cols_meta_ <- cols_meta[cols_meta %in% colnames(df_)]

        df_result <- df_result %>%
          dplyr::bind_rows(
            df_ %>%
              tidyr::pivot_longer(cols = -c(cols_meta_))
          )
      }

    }
    df_result <- df_result %>%
      tidyr::pivot_wider()

  } else {
    df_result <- importBC_data_avg0(parameter = parameter, years = years,averaging_type = averaging_type,data_threshold = data_threshold,
                                    flag_TFEE = flag_TFEE, merge_Stations = merge_Stations)
  }

  return(df_result)
}


#' Back end function, see importBC_data_avg for main script
#'
#' This function is capable of compoung averaging_type (e.g., "annual 98p d1hm")
#' But not ideal for multiple years or parameters
#'
importBC_data_avg0 <- function(parameter, years = NULL, averaging_type =  NULL, data_threshold = 0.75,
                               flag_TFEE = FALSE,merge_Stations = FALSE)
{
  if (0) {
    source('./r/importBC_data.R')
    source('./r/listBC_stations.R')
    source('./r/paddatafunction.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/envairfunctions.R')
    # parameter <- 'pm25'
    parameter <- 'o3'
    years <- 2018
    averaging_type <- 'd8hm'
    data_threshold <- 0.75
    merge_Stations <- TRUE
    flag_tfee = TRUE

  }

  flag_tfee <- flag_TFEE #added to stay consistent
  require(dplyr)

  # Determine if user entered dataframe or specified a parameter
  #
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


    #add extra year for d8hm
    if (averaging_type == 'd8hm') {
      years_ <-  c(min(years)-1,years)
    } else {
      years_ <- years
    }
    #retrieve data
    df <- importBC_data(parameter = parameter,
                        years = years_,
                        flag_TFEE = flag_tfee,
                        merge_Stations = merge_Stations,
                        clean_names = FALSE)
  } else {

    message('Dataframe entered')
    df <- parameter

    #get years based on the dataframe
    years <- df %>%
      mutate(year=lubridate::year(DATE)) %>%
      pull(year) %>% unique()
    #check if averaging_type is null
    if (is.null(averaging_type)) {
      message('Dataframe entry, please specify averaging_type')
      return(NULL) #stops if averaging_type not defined
    }
  }

  #identify if there is TFEE flag in the data
  #then redefine add_TFEE
  flag_tfee <- 'flag_tfee' %in% colnames(df)

  #identify if it is annual summary, exceedance, or regular data query
  #re-define averaging_type based on the
  averaging_type <- gsub('excess','EXCEED',averaging_type,ignore.case = TRUE)
  averaging_type <- gsub('over','EXCEED',averaging_type,ignore.case = TRUE)
  averaging_type <- gsub('more','EXCEED',averaging_type,ignore.case = TRUE)
  #rename the averaging type
  if (grepl('annual',averaging_type,ignore.case = TRUE) | grepl('exceed',averaging_type,ignore.case = TRUE))
  {
    is_annual <- TRUE
    is_exceedance <- grepl('exceed',averaging_type,ignore.case = TRUE)
    #splits the averaging _type, separated by space
    str_avg <- unlist(stringr::str_split(averaging_type,' '))
    if (length(str_avg) !=3)
    {
      message('Please check averaging_type= value. Type ?importBC_data_avg for details')
      return(NULL)
    }
    averaging_type <- str_avg[3]
    annual_summary <- str_avg[2]


  } else
  {
    is_annual <- FALSE
  }


  df_result <- NULL

  #retrieve data without TFEE
  df_no_tfee <- df %>%
    importBC_data_avg_(years = years, averaging_type =  averaging_type, data_threshold = data_threshold)

  if (!flag_tfee) {
    message('sending result with no TFEE')
    df_result <- df_no_tfee
  } else {

    df_tfee <- df %>%
      filter(!flag_tfee) %>% #remove data with TFEE Flags
      # View()
      importBC_data_avg_(years = years, averaging_type =  averaging_type, data_threshold = data_threshold)

    colnames(df_tfee)[grepl('_VALUE',colnames(df_tfee))] <- paste(colnames(df_tfee)[grepl('_VALUE',colnames(df_tfee))],
                                                                  '_TFEE',sep='')
    message('sending result with TFEE')
    # print(colnames(df_tfee))
    df_result <- df_no_tfee %>%
      dplyr::left_join(df_tfee)
  }

  #exit if null
  if (is.null(df_result)) {return(NULL)}

  #re-define the averaging_type based on data output
  cols <- colnames(df_no_tfee)
  averaging_type <- cols[grepl('RAW_VALUE',cols,ignore.case = TRUE)][1]
  averaging_type <- gsub('RAW_VALUE_','',averaging_type,ignore.case = TRUE)

  if (is_annual) {
    message('processing annual statistics')
    df_result <- ungroup(df_result) %>%
      dplyr::mutate(YEAR = lubridate::year(DATE))

    cols <- colnames(df_result)
    cols_values <- cols[grepl('value',cols,ignore.case = TRUE)]

    if (is_exceedance) {


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
        filter(!grepl('ROUNDED',name,ignore.case = TRUE)) %>%
        mutate(value = round2(value,n=precision)) %>%
        dplyr::mutate(excess_count = ifelse(value>annual_summary,1,0)) %>%
        group_by(YEAR,STATION_NAME,INSTRUMENT,PARAMETER,name) %>%
        dplyr::summarise(exceed = sum(excess_count,na.rm = TRUE)) %>%
        dplyr::mutate(name = gsub('value',
                                  paste('(>',annual_summary,')',sep=''),
                                  name,ignore.case = TRUE)) %>%
        dplyr::mutate(name = gsub('raw_','EXCEED',name,ignore.case = TRUE)) %>%
        tidyr::pivot_wider(names_from = name,values_from = exceed)

    } else {
      #if this is NOT calculation of exceedances
      #for percentiles
      if (grepl('p',annual_summary,ignore.case = TRUE)) {
        message('Calculating the percentiles of the year')
        quantile <- as.numeric(gsub('p','',annual_summary,ignore.case = TRUE))/100

        df_result <- df_result %>%
          tidyr::pivot_longer(cols =cols_values) %>%
          group_by(PARAMETER,YEAR,STATION_NAME,INSTRUMENT,name) %>%
          dplyr::summarise(value = stats::quantile(value,probs = quantile,na.rm = TRUE,type = 2)) %>%
          dplyr::mutate(name = gsub('VALUE','ANNUAL',name,ignore.case = TRUE)) %>%
          dplyr::mutate(name = gsub(averaging_type,
                                    paste(toupper(annual_summary),averaging_type,sep='_'),
                                    name,ignore.case = TRUE)) %>%
          tidyr::pivot_wider()
      }

      #for mean
      annual_summary <- gsub('avg','mean',annual_summary)
      annual_summary <- gsub('average','mean',annual_summary)
      if (grepl('mean',annual_summary,ignore.case = TRUE)) {
        message('Calculating the average values of the year')
        df_result <- df_result %>%
          tidyr::pivot_longer(cols =cols_values) %>%
          group_by(PARAMETER,YEAR,STATION_NAME,INSTRUMENT,name) %>%
          dplyr::summarise(value = mean(value,na.rm = TRUE)) %>%
          dplyr::mutate(name = gsub('VALUE','ANNUAL',name,ignore.case = TRUE)) %>%
          dplyr::mutate(name = gsub(averaging_type,
                                    paste(toupper(annual_summary),averaging_type,sep='_'),
                                    name,ignore.case = TRUE)) %>%
          tidyr::pivot_wider()
      }

      #for nth highest,standardize number suffix (-rd,-th,-st)
      if (gsub('[0-9]+', '', annual_summary) %in% c('rd','th','st','nd')) {


        nth_order <- stringr::str_extract(annual_summary,'\\d+')
        nth_order <- as.numeric(nth_order)
        message(paste('Calculating the nth highest of the year. nth =',nth_order))
        cols_remove <- cols[!cols %in% c('STATION_NAME','INSTRUMENT','PARAMETER','YEAR',cols_values)]

        df_result <-   df_result %>%
          tidyr::pivot_longer(cols =cols_values) %>%
          filter(!is.na(value)) %>%
          arrange(desc(value)) %>%
          dplyr::select(-cols_remove) %>%
          group_by(PARAMETER,YEAR,STATION_NAME,INSTRUMENT,name) %>%
          slice(nth_order) %>%
          distinct() %>%
          dplyr::mutate(name = gsub('VALUE','ANNUAL',name,ignore.case = TRUE)) %>%
          dplyr::mutate(name = gsub(averaging_type,
                                    paste(toupper(annual_summary),averaging_type,sep='_'),
                                    name,ignore.case = TRUE)) %>%
          arrange(name,YEAR,STATION_NAME) %>%
          tidyr::pivot_wider()
      }

    }
  }

  return(ungroup(df_result))
}


#' Backend function of importBC_data_avg_
#'
#' Note that averaging_type here is the simple (not compounded) format.
importBC_data_avg_ <- function(parameter, years = NULL, averaging_type =  NULL, data_threshold = 0.75) {

  if (0) {
    parameter <- importBC_data('o3',2015)
    years = 2018
    averaging_type =  'd8hm'
    data_threshold = 0.75
  }
  #add datetime_ columns for time-beginning processing

  df <- parameter

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
      pad_data() %>%
      dplyr::rename(RAW_VALUE_1HR = RAW_VALUE,
                    ROUNDED_VALUE_1HR = ROUNDED_VALUE)
    return(df)
  }

  if (averaging_type %in% c('24hr'))
  {

    message('Calculating 24-hour values')
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
    message('Calculating daily 1-hour maximum values')
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
    message('Calculating daily 8-hour maximums')
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

      message(paste('Calculating running average:',i,'/7',sep=''))
      gc()
      df_ <- df_ %>%
        dplyr::bind_rows(
          df %>%
            dplyr::select(cols_select_all) %>%
            dplyr::mutate(DATE_PST = DATE_PST + lubridate::hours(i))
        )
    }

    gc() #-clear memory

    #calculate running average
    df <- df_ %>%
      filter(!is.na(RAW_VALUE)) %>%
      dplyr::group_by(dplyr::across(c('DATE_PST',cols_select))) %>%
      dplyr::summarise(RAW_VALUE_8h = mean(RAW_VALUE),
                       ROUNDED_VALUE_8h = mean(ROUNDED_VALUE),
                       valid_n = n()) %>%
      dplyr::filter(valid_n>=6) %>%  #6 hrs at least
      dplyr::select(-valid_n)

    gc()

    # get the daily maximum
    df <- df %>%
      ungroup() %>%
      dplyr::mutate(datetime = DATE_PST-lubridate::hours(1)) %>%
      dplyr::mutate(DATE = format(datetime,'%Y-%m-%d')) %>%
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

    gc()
    df <- df %>%
      pad_data(date_time = 'DATE',
               values =c('RAW_VALUE_D8HM','ROUNDED_VALUE_D8HM','valid_hrs'),
               padby = 'day')


    return(df)

  }

  if (averaging_type %in% c('8hr')) {

    message('Calculating 8-hour running average')
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

      message(paste('Calculating running average:',i,'/7',sep=''))
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

  message(paste(averaging_type,'is not in the list of averaging types. Use d1hm, d8hm, 1-hour, or 24-hour'))
  return(NULL)

}
