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


#' Import Hourly BC Data from station or parameter
#'
#' This function retrieves station or parameter hourly data from the BC open data portal
#' Data includes verified and unverified data depending on whether Level 2 has been completed for that date
#'
#' @param parameter_or_station vector list of air quality parameters or station (automatic).
#' use the function listBC_stations() to get a detailed list of BC Air Quality Monitoring stations
#' if there is no exact station match, it will use partial match
#' so that user can just enter 'Prince George' and it will search for all stations
#' with Prince  George in the station name (Prince George Plaza 400, Prince George Glenview)
#' List of parameters can be obtained using list_parameters() command
#' List of stations can be retrieved using listBC_stations() command
#' Mutliple stations can be specified
#' @param years the years that will be retrieved. For sequence, use 2009:2015. For non
#' sequential years, use c(2010,2015,2018)
#' If not declared, the current year will be used
#' @param use_openairformat is boolean,if TRUE, output is compatible with openair. Apples only to station queries
#' @param use_ws_vector use vector wind speed? default is FALSE, if TRUE and use_openairformat is TRUE, ws is the vector wind speed
#' @param pad default is TRUE. if FALSE, it removes all NaNs
#' @param flag_TFEE default is FALSE. If TRUE, it will add TFEE flags on days TFEE were verified
#' This function retrieves TFEE from the CAAQS station history excel file
#' ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/
#' @param merge_Stations default is FALSE. it will combine data from stations and alternative stations
#' This function retrieves thsoe details from the CAAQS station history excel file
#'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/
#'
#'@examples
#' importBC_data('Prince George Plaza 400')
#' importBC_data('pm25',2015:2016,use_openairformat = FALSE)
#' importBC_data(c('Prince George','Kamloops'),c(2010,2015))
#'
#' @export
importBC_data <- function(parameter_or_station,
                          years=NULL,use_openairformat=TRUE,
                          use_ws_vector = FALSE,pad = TRUE,
                          flag_TFEE = FALSE, merge_Stations = FALSE) {

  #debug
  if (0)
  {

    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    source('./r/envairfunctions.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/importbc_data.R')
    parameter_or_station <- c('pm25')
    # parameter_or_station <- 'smithers'
    years <- c(2021:2022)
    pad = TRUE
    use_openairformat <- TRUE
    use_ws_vector <- FALSE
    flag_TFEE = TRUE
    merge_Stations = TRUE

  }


  library(lubridate)
  library(dplyr)
  library(tibble)
  library(arrow)

  #default parameter list
  #list will still be populated from year_to_date csv
  lst_params <- c('aqhi','co','h2s','hf','humidity','no','no2','nox',
                  'o3','pm10','pm25','precip','pressure','snow','so2','temp_mean','trs',
                  'vapour_pressure','wdir_uvec','wdir_vect','wspd_sclr','wspd_vect')

  #list of columns based on pollutant
  cols_aqhi <- c('PARAMETER','DATE_PST','DATE','TIME','AQHI_AREA','CGNDB_NAME',
                 'STATION_NAME','REPORTED_AQHI','REPORTED_AQHI_CHAR',
                 'AQHI_CLASSIC','AQHI_PLUS_PARAMETER','SO2_GT_36')
  cols_nonaqhi <- c('PARAMETER','DATE_PST','DATE','TIME','STATION_NAME',
                    'STATION_NAME_FULL','EMS_ID','NAPS_ID','UNIT','INSTRUMENT',
                    'OWNER','REGION','RAW_VALUE','ROUNDED_VALUE','VALIDATION_STATUS')



  if (0) {
    df_ %>%
      filter(!is.na(AQHI_AREA)) %>%
      slice(10) %>%
      View()
  }

  #parameter name into their standard names
  #note that ^xxxyyy$ means start and end is xxxyyy
  parameter_or_station <- tolower(parameter_or_station)
  parameter_or_station <- gsub('^pm2.5$','pm25',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^ozone$','o3',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^temp$','temp_mean',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^tempmean$','temp_mean',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^tempmean$','temp_mean',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^precipitation$','precip',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^rh$','humidity',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^wspd$','wspd_vect',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^wdir$','wdir_vect',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^wspd_vec$','wspd_vect',parameter_or_station,ignore.case=TRUE)
  parameter_or_station <- gsub('^wdir_vec$','wdir_vect',parameter_or_station,ignore.case=TRUE)


  #primary data location
  data_source1<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/'
  data_source2<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/'

  #determine if station or parameter
  #will base parameter list on some list and on new list from current data
  check_datalist <- GET_FTP_DETAILS(data_source2) %>%
    filter(!grepl('dir',INDEX,ignore.case = TRUE),
           !grepl('station',FILENAME,ignore.case = TRUE),
           grepl('csv',FILENAME,ignore.case = TRUE)) %>%
    mutate(FILENAME = tolower(FILENAME)) %>%
    mutate(FILENAME = gsub('.csv','',FILENAME,ignore.case = TRUE)) %>%
    pull(FILENAME)

  check_datalist <- tolower(check_datalist)
  check_datalist <- tolower(unique(c(lst_params,check_datalist)))

  #create list of columns that will be selected
  cols_selected <- c('PARAMETER','DATE_PST','DATE','TIME')

  if ('aqhi' %in% parameter_or_station) {
    cols_selected <- c(cols_selected,cols_aqhi)
  }

  if (any(parameter_or_station %in% check_datalist[check_datalist != 'aqhi'])) {
    cols_selected <- c(cols_selected,cols_nonaqhi)
  }


  if (any(parameter_or_station %in% check_datalist)) {
    print('parameter/s entered. retrieving data....')
    is_parameter <- TRUE


  } else {
    print('station name entered. retrieving data....')
    is_parameter <- FALSE
  }



  #ensure the
  #no padding, quick query if flag_TFEE or merge_station
  if (flag_TFEE | merge_Stations){
    #better remove duplicates, and fix names
    pad <- TRUE
  }

  #get FTP details
  suppressWarnings({
    df_datasource1 <- GET_FTP_DETAILS(data_source1) %>%
      filter(as.numeric(FILENAME)>=1970) %>%
      mutate(year = as.numeric(FILENAME))
  })

  #latest validation year
  max_year <- max(as.numeric(df_datasource1$year),na.rm = TRUE)
  current_year <- lubridate::year(Sys.Date())


  #find out if there is a parquet file
  #if parquet available, use it
  #otherwise, use the csv files if year is 2021 and later
  df_datasource2 <- GET_FTP_DETAILS(data_source2) %>%
    merge(tibble(
      year = (max_year +1):current_year
    ))


  if (any(grepl('.parquet',df_datasource2$FILENAME,ignore.case = TRUE))) {
    # df_datasource2 <-
    df_datasource2 <- df_datasource2[grepl('.parquet',df_datasource2$FILENAME,ignore.case = TRUE),]
  } else {
    #still all csv
    df_datasource2 <- df_datasource2[grepl('.csv',df_datasource2$FILENAME,ignore.case = TRUE),]
    #exclude monitoring station csv
    df_datasource2 <- df_datasource2[!grepl('station',df_datasource2$FILENAME,ignore.case = TRUE),]

    #filter based on the parameter
    if (is_parameter) {
      df_datasource2 <- df_datasource2 %>%
        filter(grepl(paste(parameter_or_station,collapse = '|'),FILENAME,ignore.case = TRUE))
    }
  }

  df_datasource <- df_datasource1 %>%
    bind_rows(df_datasource2) %>%
    filter(year %in% years)


  #for DIRECTORIES, check for the parquet files
  lst_dirs <- unique(df_datasource$URL[grepl('dir',df_datasource$INDEX,ignore.case = TRUE)])
  lst_dirs <- paste(lst_dirs,'/',sep='')
  for (dirs_ in lst_dirs) {
    try({
      df_ <- GET_FTP_DETAILS(dirs_) %>%
        filter(grepl('.parquet',FILENAME,ignore.case = TRUE))

      df_datasource$INDEX[df_datasource$URL == gsub('/$','',dirs_)] <- '111111'
      df_datasource$FILENAME[df_datasource$URL == gsub('/$','',dirs_)] <- df_$FILENAME
      df_datasource$URL[df_datasource$URL == gsub('/$','',dirs_)] <- df_$URL

    })
  }

  #retrieve data one at a time
  lst_source <- unique(df_datasource$URL)
  #remove aqhi if station name query
  if (!is_parameter) {
    lst_source <- lst_source[!grepl('aqhi',lst_source,ignore.case = TRUE)]
  }

  df_data <- NULL
  for (lst_ in lst_source) {
    if (0) {
      lst_ <- lst_source[[1]]
    }
    print(paste('reading the file:',lst_))
    #read file, need to identify if parquet or csv
    years_selected <- df_datasource$year[df_datasource$URL %in% lst_]
    try({
      a <- tempfile()
      curl::curl_download(lst_,a,quiet = FALSE)

      #read the parquet or csv file
      if (grepl('.parquet',lst_,ignore.case = TRUE)) {

        df_ <- arrow::read_parquet(a)
        df_ <- df_ %>%
          mutate(TIME = format(ymd_hms(DATE_PST),format = '%H:%M')) %>%
          mutate(TIME = ifelse(TIME == '00:00','24:00',TIME))
        try({
          df_ <- df_ %>%
            mutate(STATION_NAME = gsub('[^[:alnum:]]',' ',STATION_NAME)) %>%
            mutate(year = year(DATE)) %>%
            filter(year %in% years_selected) %>%
            select(-year)
        })

        if (is_parameter) {
          df_ <- df_ %>%
            filter(tolower(PARAMETER) %in% parameter_or_station) %>%
            dplyr::select(any_of(cols_selected))
        } else {
          #station query, so select station only
          df_ <- df_ %>%
            dplyr::select(any_of(cols_nonaqhi))
        }
      }

      if (grepl('.csv',lst_,ignore.case = TRUE)) {
        df_ <- readr::read_csv(a)


        df_ <-df_ %>%
          mutate(date_time = DATE_PST - hours(1)) %>%
          mutate(year = year(date_time)) %>%
          filter(year %in% years_selected) %>%
          mutate(DATE = date(date_time),
                 TIME = format(DATE_PST,  '%H:%M')) %>%
          mutate(TIME = ifelse(TIME == '00:00','24:00', TIME),
                 DATE_PST = format(DATE_PST,'%Y-%m-%d %H:%M:%S')) %>%
          select(-date_time,-year)

        if (!('VALIDATION_STATUS' %in% colnames(df_))) {
          df_ <- df_ %>%
            mutate(VALIDATION_STATUS = 'Level 0')
        }
        try({
          df_ <- df_ %>%
            mutate(STATION_NAME = gsub('[^[:alnum:]]',' ',STATION_NAME))
        })
        if (is_parameter) {
          df_ <- df_ %>%
            select(any_of(cols_selected))
        } else {
          #station query, so select station only
          df_ <- df_ %>%
            select(any_of(cols_nonaqhi))
        }



        #add parameter colname if not available
        if (!'PARAMETER' %in% colnames(df_)) {
          df_ <- df_ %>%
            mutate(PARAMETER = gsub('.*/','',lst_)) %>%
            mutate(PARAMETER = gsub('.csv','',PARAMETER))
        }
      }




      #add if duplicate removal needed
      if (0) {#remove duplicate data entries
        df_data <- df_data %>%
          filter(!is.na(RAW_VALUE)) %>%
          group_by(DATE_PST,STATION_NAME,INSTRUMENT,PARAMETER) %>%
          dplyr::mutate(index =1:n(), count =n()) %>%
          filter(index ==1) %>%
          ungroup()

        if (any(df_data$count>1)) {
          warning('Duplicate data detected but removed.')
        } else {
          df_data <- df_data %>%
            ungroup() %>%
            select(-count,-index)
        }}


    })
    df_data <-  df_data %>%
      bind_rows(df_)
  }


  #convert to datetime
  df_data$DATE_PST <- lubridate::ymd_hms(df_data$DATE_PST)
  #perform other functions
  if (flag_TFEE) {
    print('adding tfee flags')
    df_tfee <- get_tfee() %>%
      select(PARAMETER,STATION_NAME,DATE) %>%
      mutate(DATE = as.Date(DATE)) %>%
      mutate(flag_tfee  = TRUE)


    df_data <- df_data %>%
      left_join(df_tfee, by=c('PARAMETER', 'DATE', 'STATION_NAME')) %>%
      dplyr::mutate(flag_tfee = ifelse(is.na(flag_tfee),FALSE,flag_tfee))

  }

  if (merge_Stations) {

    lst_history <- get_station_history() %>%
      select(STATION_NAME,INSTRUMENT,`Merged Station Name`,`Merged Instrument Name`)

    df_data <- df_data %>%
      left_join(lst_history) %>%
      mutate(STATION_NAME_NEW = ifelse(is.na(`Merged Station Name`),STATION_NAME,`Merged Station Name`),
             INSTRUMENT_NEW = ifelse(is.na(`Merged Instrument Name`),INSTRUMENT,`Merged Instrument Name`)) %>%
      select(-`Merged Station Name`,-`Merged Instrument Name`) %>%
      dplyr::rename(STATION_NAME_ORIGINAL = STATION_NAME,
                    INSTRUMENT_ORIGINAL = INSTRUMENT) %>%
      dplyr::rename(STATION_NAME = STATION_NAME_NEW,INSTRUMENT = INSTRUMENT_NEW) %>%
      COLUMN_REORDER(c('PARAMETER','DATE_PST','DATE','TIME','STATION_NAME','STATION_NAME_ORIGINAL',
                       'INSTRUMENT','INSTRUMENT_ORIGINAL'))
  }

  df_data$STATION_NAME_FULL <- toupper(df_data$STATION_NAME_FULL)
  if (pad) {

    if (0) {
      df0 <- df_data
      colnames(df0)
    }
    df_data <- df_data %>%
      select(PARAMETER,DATE_PST,DATE,TIME,STATION_NAME,STATION_NAME_FULL,INSTRUMENT,
             RAW_VALUE,ROUNDED_VALUE,VALIDATION_STATUS,flag_tfee)

    df_data <- pad_data(df_data,date_time = 'DATE_PST',values = c('RAW_VALUE','ROUNDED_VALUE','flag_tfee',
                                                                  'VALIDATION_STATUS','STATION_NAME_FULL',
                                                                  'INSTRUMENT'))
  }
  #perform options
  #more process if station is selected
  if (is_parameter) {

    df_data <- df_data %>%
      arrange(PARAMETER,STATION_NAME,INSTRUMENT,DATE_PST) %>%
      COLUMN_REORDER(c('PARAMETER','DATE_PST','DATE','TIME','STATION_NAME','STATION_NAME_FULL','INSTRUMENT'))
    #done, sending results
    return(df_data)
  } else
  {
    #station was selected
    df_data <- df_data %>%
      filter(grepl(paste(parameter_or_station,collapse = '|'),STATION_NAME,ignore.case = TRUE))

    return(df_data)

  }

}





