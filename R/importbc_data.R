# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


#' Extracts the date time string into a format YYYY-MM-DD hh:mm
#'
#' @param dateTimeString is a vector containing datetime string
#'
#' @export
extractDateTime <- function(dateTimeString) {

  if (0) {
    dateTimeString <- df_$DATE_PST
    dateTimeString <- c(df_data$DATE_PST)
    dateTimeString <- c(df_data$DATE_PST,
                        paste(df_$DATE_PST,'00',sep=':'))
  }


  if (is.POSIXct(dateTimeString)) {
    dateTimeString <- format(dateTimeString,'%Y-%m-%d %H:%M')
  }
  df_datetime <- tibble(
    datetime = dateTimeString) %>%
    # mutate(length = nchar(datetime)) %>%
    mutate(result =gsub("^([^:]*:[^:]*):.*", "\\1", datetime))

  #for those that are 24:00
  df_datetime$result[!grepl(':',df_datetime$result)] <-
    paste(df_datetime$result[!grepl(':',df_datetime$result)],'00:00' )


  return(df_datetime$result)

}

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
#' @param use_openairformat is boolean,if TRUE, output is compatible with openair. Applies only to station queries
#' @param flag_TFEE default is FALSE. If TRUE, it will add TFEE flags on days TFEE were verified
#' @param merge_Stations default is FALSE. it will combine data from stations and alternative stations
#' @param clean_names makes the output columns in lower case letters as acceptable with tidyverse
#' This function retrieves thsoe details from the CAAQS station history excel file
#'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/
#'
#'
#'@examples
#' importBC_data('Prince George Plaza 400')
#' importBC_data('pm25',2015:2016,use_openairformat = FALSE)
#' importBC_data(c('Prince George','Kamloops'),c(2010,2015))
#'
#' @export
importBC_data <- function(parameter_or_station,
                          years=NULL,
                          flag_TFEE = TRUE,
                          merge_Stations = TRUE,
                          clean_names = FALSE,use_openairformat = TRUE) {

  #debug
  if (0)
  {

    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    source('./r/envairfunctions.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/importbc_data.R')

    parameter_or_station <- 'temp_mean'

    years=2022
    flag_TFEE = TRUE
    merge_Stations = TRUE
    clean_names = TRUE
    use_openairformat = TRUE

    parameter_or_station = c("wdir_vect")
    years = 2020:2021
    use_openairformat = FALSE
  }


  library(lubridate)
  library(dplyr)
  library(tibble)
  library(arrow)
  library(janitor)

  parameter_or_station <- tolower(parameter_or_station)

  # check for aqhi
  # -if aqhi is selected, it will not combine with other parameters
  if ('aqhi' %in% parameter_or_station) {
    parameter_or_station <- 'aqhi'
    message(paste('AQHI selected. Note that all other parameters are ignored.'))
  }


  # initial parameter list
  # list will still be populated from year_to_date csv
  lst_params <- c('aqhi','co','h2s','hf','humidity','no','no2','nox',
                  'o3','pm10','pm25','precip','pressure','snow','so2','temp_mean','trs',
                  'vapour_pressure','wdir_uvec','wdir_vect','wspd_sclr','wspd_vect')

  #list of columns based on pollutant
  cols_aqhi <- c('PARAMETER','DATE_PST','DATE','TIME','AQHI_AREA','CGNDB_NAME',
                 'STATION_NAME','REPORTED_AQHI','REPORTED_AQHI_CHAR','AQHI_VALUE',
                 'AQHI_CLASSIC','AQHI_PLUS_PARAMETER','SO2_GT_36','AQHI_PLUS_PM25_VALUE',
                 'VALIDATION_STATUS','AQHI_SO2')
  cols_nonaqhi <- c('PARAMETER','DATE_PST','DATE','TIME','STATION_NAME',
                    'STATION_NAME_FULL','EMS_ID','NAPS_ID','UNIT','INSTRUMENT',
                    'OWNER','REGION','RAW_VALUE','ROUNDED_VALUE','VALIDATION_STATUS')



  if (0) {
    df_ %>%
      filter(!is.na(AQHI_AREA)) %>%
      slice(10) %>%
      View()
  }

  # -change parameter name into their standard names
  #     -note that ^xxxyyy$ means start and end is xxxyyy
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

  #remove other options if parameter_or_station is selected

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
    message(paste('Retrieving data from the following parameters:',paste(parameter_or_station,collapse = ',')))
    is_parameter <- TRUE


  } else {
    message('station name entered. retrieving data....')
    is_parameter <- FALSE
  }



  #get FTP details frr all datasources
  suppressWarnings({
    df_datasource1 <- GET_FTP_DETAILS(data_source1) %>%
      filter(as.numeric(FILENAME)>=1970) %>%
      mutate(year = as.numeric(FILENAME))

    df_datasource1$URL <- paste(df_datasource1$URL,'/binary',sep='')


    #latest validation year
    max_year <- max(as.numeric(df_datasource1$year),na.rm = TRUE)
    current_year <- lubridate::year(Sys.Date())
    df_datasource2 <- GET_FTP_DETAILS(data_source2) %>%
      merge(tibble(
        year = (max_year +1):current_year
      )) %>%
      filter(grepl('dir',INDEX,ignore.case = TRUE))

    df_datasource <- df_datasource1 %>%
      bind_rows(df_datasource2)
    try({
      df_datasource <- df_datasource %>%
        filter(!grepl('station',URL,ignore.case = TRUE)) %>%
        select(-FILENAME)

    })


  })

  #include several years of data for wspd and wdir
  #added due to validation issue
  #it will always include the current year (unverified)
  #this also includes the station lookup
  #2021 onwards
  years_source <- years
  if (grepl('WDIR|WSPD',paste(parameter_or_station,collapse = ','),ignore.case = TRUE) & any(years >= 2021)) {
    years_source <- c(years,current_year)
  }



  df_datasource <- df_datasource %>%
    filter(year %in% years_source)


  # get list of all parquet files

  df_datasource_result <- NULL
  for (urls in unique(df_datasource$URL)) {
    if (0) {
      urls <- df_datasource$URL[1]
    }
    df_meta <- df_datasource[df_datasource$URL == urls,] %>%
      select(year) %>% distinct()
    try({
      df_ <- GET_FTP_DETAILS(urls) %>%
        merge(df_meta)

      df_datasource_result <- df_datasource_result %>%
        bind_rows(df_)

    })

  }

  #if parameter, retrieve only that parameter
  # if station, then everything except AQHI
  if (is_parameter) {
    df_datasource_result <- df_datasource_result %>%
      filter(grepl(paste(parameter_or_station,collapse = '|'),FILENAME,ignore.case = TRUE))
  } else {
    df_datasource_result <- df_datasource_result %>%
      filter(!grepl('aqhi',FILENAME,ignore.case = TRUE))
  }

  lst_source <- df_datasource_result %>%
    pull(URL) %>%
    unique()

  #retrieve data----
  #scan one file at a time
  df_data <- NULL
  for (lst_ in lst_source) {
    if (0) {
      lst_ <- lst_source[[1]]
    }
    message(paste('reading the file:',lst_))


    #read parquet file
    try({
      a <- tempfile()
      curl::curl_download(lst_,a,quiet = FALSE)

      df_ <- NULL
      if (grepl('.parquet',lst_,ignore.case = TRUE)) {
        df_ <- arrow::read_parquet(a)
      }
      if (grepl('.csv',lst_,ignore.case = TRUE)) {
        df_ <- readr::read_csv(a)
      }

      #filter based on parameter if parameter
      #or station name if not isparametr
      if (is_parameter) {
        df_ <- df_ %>%
          filter(grepl(paste(parameter_or_station,collapse = '|'),
                       PARAMETER,ignore.case = TRUE))%>%
          dplyr::select(any_of(cols_selected))
      } else {
        df_ <- df_ %>%
          filter(!grepl('aqhi',PARAMETER,ignore.case = TRUE)) %>%
          filter(grepl(paste(parameter_or_station,collapse = '|'),
                       STATION_NAME,ignore.case = TRUE)) %>%
          dplyr::select(any_of(cols_nonaqhi))
      }

      cols_df <- colnames(df_)

      #debug
      if (0) {
        # df0 <- df_
        df_ <- df0
        df_ %>%
          filter(TIME == '01:00:00')
      }
      #process the datetime from DATE_PST
      df_$DATE_PST <- extractDateTime(df_$DATE_PST )


      df_ <- df_ %>%
        mutate(DATE_PST = ymd_hm(DATE_PST)) %>%
        mutate(datetime = DATE_PST - hours(1)) %>%
        mutate(DATE = lubridate::date(datetime)) %>%
        mutate(TIME = format(DATE_PST, '%H:%M')) %>%
        mutate(TIME = ifelse(TIME == '00:00','24:00',TIME)) %>%
        mutate(STATION_NAME = gsub('[^[:alnum:]]',' ',STATION_NAME)) %>%
        mutate(STATION_NAME = gsub('\\s{2,}',' ',STATION_NAME)) %>%
        mutate(year = year(DATE)) %>%
        filter(year %in% years) %>%
        select(-year,-datetime)

      if (!('VALIDATION_STATUS' %in% cols_df)) {
        df_ <- df_ %>%
          mutate(VALIDATION_STATUS = 'Level 0')
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

    if (nrow(df_)>0) {
      df_data <-  df_data %>%
        bind_rows(df_)
    }

    # -clear memory
    gc()
  }


  # -for aqhi data-----
  # return results immediately
  if ('aqhi' %in% tolower(parameter_or_station)) {
    try(df_data <- df_data %>%
          select(-flag_tfee), silent = TRUE)
    try(df_data <- df_data %>%
          select(-VALIDATION_STATUS), silent = TRUE)
    try(
      df_data <- df_data %>%
        select(PARAMETER,DATE_PST,DATE,TIME,everything()),
      silent = TRUE
    )
    # -filter the correct year
    df_data$year = lubridate::year(df_data$DATE)
    df_data <- df_data[df_data$year %in% years,]
    gc()
    return(df_data)
  }

  # -for non-aqhi data-----
  #DEBUG
  #when hour is 24, seems to be blank
  if (0) {
    # df0 <- df_data
    df_data <- df0   #reset

    df_data %>%
      filter(TIME == '24:00')
  }


  #perform other functions
  if (flag_TFEE) {
    message('adding tfee flags')

    df_tfee <- get_tfee() %>%
      select(PARAMETER,STATION_NAME,DATE) %>%
      mutate(DATE = as.Date(DATE)) %>%
      mutate(flag_tfee  = TRUE)


    df_data <- df_data %>%
      left_join(df_tfee, by=c('PARAMETER', 'DATE', 'STATION_NAME')) %>%
      dplyr::mutate(flag_tfee = ifelse(is.na(flag_tfee),FALSE,flag_tfee))

  }

  if (merge_Stations) {

    #add index to data to make reference easy
    df_data <- ungroup(df_data) %>%
      mutate(index = 1:n())

    lst_history <- get_station_history() %>%
      select(STATION_NAME,INSTRUMENT,`Merged Station Name`,`Merged Instrument Name`,`Start Date`,`End Date`) %>%
      mutate(start = lubridate::date(`Start Date`),
             end =lubridate::date(`End Date`))

    #note that instrument matching only applies to PM
    #but renaming of station names applies to all

    lst_remove <- NULL  #start of indexing

    # -remove station and instrument before start date
    lst_remove_start <- lst_history %>%
      filter(!is.na(`Start Date`))
    for (i in 1:nrow(lst_remove_start)) {
      index_remove <- df_data %>%
        filter(STATION_NAME == lst_remove_start$STATION_NAME[i],
               INSTRUMENT == lst_remove_start$INSTRUMENT[i],
               DATE < lst_remove_start$start[i]) %>%
        pull(index)

      lst_remove <- c(lst_remove,index_remove)
    }

    # -remove station and instrument after end date
    lst_remove_end <- lst_history %>%
      filter(!is.na(`End Date`))
    for (i in 1:nrow(lst_remove_end)) {
      index_remove <- df_data %>%
        filter(STATION_NAME == lst_remove_end$STATION_NAME[i],
               INSTRUMENT == lst_remove_end$INSTRUMENT[i],
               DATE >= lst_remove_end$end[i]) %>%
        pull(index)

      lst_remove <- c(lst_remove,index_remove)
    }

    # -remove from data
    df_data <- df_data %>%
      filter(!index %in% lst_remove) %>%
      select(-index)
    #change the instrument name
    suppressMessages(
      df_data <- df_data %>%
        left_join(lst_history %>%
                    select(STATION_NAME,INSTRUMENT,`Merged Station Name`,
                           `Merged Instrument Name`)) %>%
        mutate(INSTRUMENT_NEW = ifelse(is.na(`Merged Instrument Name`),INSTRUMENT,`Merged Instrument Name`)) %>%
        select(-`Merged Station Name`,-`Merged Instrument Name`) %>%
        dplyr::rename(INSTRUMENT_ORIGINAL = INSTRUMENT) %>%
        dplyr::rename(INSTRUMENT = INSTRUMENT_NEW)
    )
    #change the station name
    suppressMessages(
      df_data <-  df_data %>%
        left_join(lst_history %>%
                    select(STATION_NAME,`Merged Station Name`) %>%
                    distinct()) %>%
        mutate(STATION_NAME_NEW = ifelse(is.na(`Merged Station Name`),STATION_NAME,`Merged Station Name`)) %>%
        select(-`Merged Station Name`) %>%
        dplyr::rename(STATION_NAME_ORIGINAL = STATION_NAME) %>%
        dplyr::rename(STATION_NAME = STATION_NAME_NEW) %>%
        COLUMN_REORDER(c('PARAMETER','DATE_PST','DATE','TIME','STATION_NAME','STATION_NAME_ORIGINAL',
                         'INSTRUMENT','INSTRUMENT_ORIGINAL'))
    )
  }


  if ('STATION_NAME_FULL' %in% colnames(df_data)) {
    df_data$STATION_NAME_FULL <- toupper(df_data$STATION_NAME_FULL)

  } else {
    df_data$STATION_NAME_FULL = df_data$STATION_NAME
  }


  # - select columns
  if (0) {
    df0 <- df_data
    colnames(df0)
    unique(df0$VALIDATION_STATUS)
    df_data %>%
      filter(STATION_NAME == 'Warfield Haley Park') %>%
      View()
    group_by(PARAMETER,DATE_PST,STATION_NAME) %>%
      dplyr::mutate(count =n()) %>%
      filter(count>1)


    df_data %>%
      filter(is.na(RAW_VALUE)) %>%
      View()
  }

  cols_select <- unique(c('PARAMETER','DATE_PST','DATE','TIME','STATION_NAME','STATION_NAME_FULL','INSTRUMENT',
                          'RAW_VALUE','ROUNDED_VALUE','VALIDATION_STATUS','flag_tfee',cols_nonaqhi))
  df_data <-   df_data %>%
    select(any_of(cols_select))


  # -filter the correct year
  message('removing extra data')
  df_data <- ungroup(df_data)
  df_data$year = lubridate::year(df_data$DATE)
  df_data <- df_data[df_data$year %in% years,]
  df_data <- df_data %>% select(-year)
  gc()


  # -pad data
  suppressMessages({

    df_data <- df_data %>%
      filter(!is.na(RAW_VALUE))

    df_data <- pad_data(df_data,date_time = 'DATE_PST',values = c('RAW_VALUE','ROUNDED_VALUE','flag_tfee'))

    df_data$flag_tfee[is.na(df_data$flag_tfee)] <- FALSE
    gc()
  })



  try({
    if (is_parameter) {

      df_data <- df_data %>%
        arrange(PARAMETER,STATION_NAME,INSTRUMENT,DATE_PST) %>%
        COLUMN_REORDER(c('PARAMETER','DATE_PST','DATE','TIME','STATION_NAME','STATION_NAME_FULL','INSTRUMENT'))
      #done, sending results

    } else
    {
      #station was selected
      df_data <- df_data %>%
        filter(grepl(paste(parameter_or_station,collapse = '|'),STATION_NAME,ignore.case = TRUE))

      if (use_openairformat) {
        if (0) {
          # df_data <- test
        }
        colnames(df_data) <- tolower(colnames(df_data))
        df_data$parameter <- tolower(df_data$parameter)
        df_data <- df_data %>%
          mutate(date_time = date_pst - hours(1)) %>%
          rename(site = station_name,
                 value = raw_value) %>%
          COLUMN_REORDER(c('parameter','date_time','date_pst','date','time','site','instrument','value'))

        df_data <- df_data %>%
          select(parameter,date_time,site,parameter,value) %>%
          filter(!is.na(value)) %>%
          group_by(date_time,site,parameter) %>%
          slice(1) %>%
          tidyr::pivot_wider(names_from = parameter, values_from = value) %>%
          rename(date = date_time)

        #RENAME WDIR_VECT AND WSPD_SCLR as ws and wd
        try({
          df_data <- df_data %>%
            rename(ws = wspd_sclr,
                   wd = wdir_vect)
        })

      }
    }
  })

  if (clean_names) {
    df_data <- clean_names(df_data)
  }
  return(df_data)

}





