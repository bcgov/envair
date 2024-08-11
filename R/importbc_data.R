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

#' Downloads files into the URL
#'
#' @param file_url defines the URLs and destinaton
download_file <- function(file_url) {


  # Download the file using RCurl
  tryCatch({
    curl::curl_download(file_url$url, file_url$destfile)
    # download.file(file_url$url, file_url$destfile, method = "curl",mode='wb')
    return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = "Downloaded"))
  }, error = function(e) {
    return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = paste("Failed:", e$message)))
  })
}



#' Download a list of FTP files
#'
#' uses parallel CPU processing to perform simultaneous downloads
#'
#' @param url_list is a vector containing list of URLs to download
download_files <- function(url_list) {

  library(parallel)
  library(curl)


  # create list of tempfiles
  df_urls <- tibble(url = url_list) %>%
    group_by(url) %>%
    mutate(destfile = tempfile()) %>%
    ungroup()

  df_urls <- split(df_urls, seq(nrow(df_urls)))
  # Set up parallel processing
  num_cores <- detectCores() - 1  # Use one less than the number of available cores
  cl <- makeCluster(num_cores)

  # Export the necessary function to the cluster
  clusterExport(cl, c("download_file", "tempfile"))

  # Perform the download in parallel and combine the results into a dataframe
  download_results <- do.call(rbind, parLapply(cl, df_urls, download_file))

  # Stop the cluster
  stopCluster(cl)

  # Print the results
  return(download_results)
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
#' @param merge_Stations default is FALSE it will combine data from stations and alternative stations
#' @param clean_names makes the output columns in lower case letters as acceptable with tidyverse
#' @param pad_data FALSE by default. if TRUE, it inserts and pads data gaps with NA
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
                          merge_Stations = FALSE,
                          clean_names = FALSE,use_openairformat = TRUE,
                          pad_data = FALSE) {

  #debug
  if (0)
  {

    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    source('./r/envairfunctions.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/importbc_data.R')

    parameter_or_station <- c('kamloops')
    parameter_or_station <- 'wspd_sclr'
    years=2022
    flag_TFEE = TRUE
    merge_Stations = TRUE
    clean_names = TRUE
    use_openairformat = TRUE

    # parameter_or_station = c("wdir_vect")
    # years = 1980:2021
    # use_openairformat = FALSE
  }


  library(lubridate)
  library(dplyr)
  library(tibble)
  library(arrow)
  library(janitor)
  library(stringr)

  parameter_or_station <- tolower(parameter_or_station)



  # check for aqhi
  # -if aqhi is selected, it will not combine with other parameters
  if ('aqhi' %in% parameter_or_station) {
    parameter_or_station <- 'aqhi'
    message(paste('AQHI selected. Note that all other parameters are ignored.'))
  }

  # -check for year entry
  if (is.null(years)) {
    years <- year(Sys.Date())
    message('years not specified, retrieving current year')
  }

  # initial parameter list
  # list will still be populated from year_to_date csv
  lst_params <- c('aqhi','co','h2s','hf','humidity','no','no2','nox',
                  'o3','pm10','pm25','precip','pressure','snow','so2','temp_mean','trs',
                  'vapour_pressure','wdir_uvec','wdir_vect','wspd_sclr','wspd_vect')

  #list of columns based on pollutant
  cols_aqhi <- c('PARAMETER','DATETIME','DATE_PST','DATE','TIME','AQHI_AREA','CGNDB_NAME',
                 'STATION_NAME','REPORTED_AQHI','REPORTED_AQHI_CHAR','AQHI_VALUE',
                 'AQHI_CLASSIC','AQHI_PLUS_PM25_VALUE','AQHI_SO2',
                 'AQHI_SO2','AQHI_REPORTED','LABEL_TEXT','GEN_POP_TEXT','RISK_POP_TEXT','SPECIAL_MESSAGE')
  cols_nonaqhi <- c('PARAMETER','DATETIME','DATE_PST','DATE','TIME','STATION_NAME',
                    'STATION_NAME_FULL','EMS_ID','NAPS_ID','UNIT','INSTRUMENT',
                    'OWNER','REGION','RAW_VALUE','ROUNDED_VALUE','VALIDATION_STATUS')

  # -define column names that will be renamed for aqhi
  cols_aqhi_rename <- tibble(
    orig_name = c('LABEL_TEXT','GEN_POP_TEXT','RISK_POP_TEXT','AQHI_PLUS_PM25_VALUE'),
    new_name = c('HEALTH_RISK','GEN_POP_MESSAGE','RISK_POP_MESSAGE','AQHI_PLUS_PM25')
  )

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


  # -determine if parameter or station was specified
  if (any(parameter_or_station %in% check_datalist)) {
    # -parameter entered
    message(paste('Retrieving data from the following parameters:',paste(parameter_or_station,collapse = ',')))
    is_parameter <- TRUE


  } else {
    # -identify what station was specified
    lstBC_stations <- listBC_stations() %>%
      pull(STATION_NAME) %>%
      unique()
    parameter_or_station <- paste(parameter_or_station,collapse ='|')
    parameter_or_station <- lstBC_stations[grepl(parameter_or_station,lstBC_stations,ignore.case = TRUE)]

    # -check if there are no entries
    if (length(parameter_or_station) == 0) {
      message('parameter/station not found: check parameter_or_station')
      return(NULL)
    }
    message(paste('station name entered. stations found:',length(parameter_or_station)))

    is_parameter <- FALSE


  }


  # -remove test
  message('extracting details of available BC Data...')
  suppressWarnings(
    df_datasource1 <- GET_FTP_DETAILS(data_source1) %>%
      filter(as.numeric(FILENAME)>=1970) %>%
      mutate(year = as.numeric(FILENAME))
  )
  df_datasource1$URL <- paste(df_datasource1$URL,'/binary',sep='')
  df_datasource1_result <- GET_FTP_DETAILS(df_datasource1$URL )

  # -retrieve details from datasource 1
  df_datasource1_result <- df_datasource1_result %>%
    ungroup() %>%
    filter(grepl('.parquet',FILENAME,ignore.case = TRUE),
           FILENAME != 'NA.parquet') %>%
    mutate(index = 1:n()) %>%
    group_by(index) %>%
    mutate(url_str = (stringr::str_split(URL,'/'))) %>%
    mutate(len_url = length(unlist(url_str))) %>%
    mutate( year = unlist(url_str)[[len_url-2]]) %>%
    ungroup() %>%
    select(FILENAME,URL,year) %>%
    mutate(parameter = toupper(gsub('.parquet','',FILENAME)),
           year = as.numeric(year))

  # -retrieve details from datasource2
  df_datasource2 <- GET_FTP_DETAILS(paste(data_source2,'binary',sep=''))%>%
    ungroup() %>%
    filter(grepl('.parquet',FILENAME,ignore.case = TRUE),
           FILENAME != 'NA.parquet')%>%
    select(FILENAME,URL) %>%
    mutate(parameter = toupper(gsub('.parquet','',FILENAME)))


  # -calculate years covered by each parameter
  current_year <- lubridate::year(Sys.Date())

  # -combime the results from the two sources
  # -and insert the years covered vy
  # -create a combined list of sources
  # -by creating complete list of all parameters
  # -parameters where the sourcefile is missing will take from the unverified data
  df_param_source <- tibble(
    year = min(df_datasource1_result$year):current_year
  ) %>%
    merge(
      df_datasource1_result %>%
        select(parameter,FILENAME)
    ) %>%
    left_join(df_datasource1_result)

  # -insert values from datasource2
  # -for those in datasource1 that are NA in URL
  df_param_source <- df_param_source %>%
    bind_rows(
      df_param_source[(is.na(df_param_source$URL) & df_param_source$year>2000),] %>%
        select(-URL) %>%
        left_join(df_datasource2)
    ) %>%
    filter(!is.na(URL)) %>%
    distinct()


  df_datasource_result <- df_param_source %>%
    filter(year %in% years)




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

  # - donwload data using parallel processing

  message(paste('downloading files:',length(lst_source)))

  df_datasource <- download_files(lst_source)

  df_data <- NULL
  for (df_file in df_datasource$TempFile) {
    df_ <- NULL
    try({
      message(paste('Reading:',df_datasource$URL[df_datasource$TempFile %in% df_file]))
      df_ <- arrow::read_parquet(df_file)


      # -process individual data

      # -fix/standardize some parameter names, all caps for parameter name
      col_ <- colnames(df_)
      if ('parameter' %in% tolower(col_)) {
        col_param <- col_[tolower(col_) %in% 'parameter']
        df_[[col_param]] <- toupper(df_[[col_param]])

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

      #process the datetime from DATE_PST
      df_$DATE_PST <- extractDateTime(df_$DATE_PST )


      df_ <- df_ %>%
        mutate(DATE_PST = ymd_hm(DATE_PST)) %>%
        mutate(datetime = DATE_PST - hours(1)) %>%
        mutate(DATE = lubridate::date(datetime)) %>%
        mutate(TIME = format(DATE_PST, '%H:%M')) %>%
        mutate(TIME = ifelse(TIME == '00:00','24:00',TIME)) %>%
        mutate(STATION_NAME = gsub('[^[:alnum:]]',' ',STATION_NAME)) %>%
        mutate(STATION_NAME = gsub('\\s+',' ',STATION_NAME)) %>%
        mutate(year = year(DATE)) %>%
        filter(year %in% years) %>%
        select(-year,-datetime)

      if (!('VALIDATION_STATUS' %in% cols_df)) {
        df_ <- df_ %>%
          mutate(VALIDATION_STATUS = 'Level 0')
      }


      # -modification for AQHI data retrieval
      if ('aqhi' %in% tolower(parameter_or_station)) {

        # -insert value of reported_AQHI
        if ('REPORTED_AQHI_CHAR' %in% cols_df)   {
          df_ <- df_ %>%
            mutate(AQHI_REPORTED = REPORTED_AQHI_CHAR) %>%
            select(-any_of(c('REPORTED_AQHI_CHAR','REPORTED_AQHI')))
        }

        # -insert AQHI_REPORTED column if it is not included

        if (!'AQHI_REPORTED' %in% colnames(df_)) {
          df_ <- df_ %>%
            mutate(AQHI_REPORTED = round2(AQHI_VALUE,n=0)) %>%
            mutate(AQHI_REPORTED =ifelse(AQHI_REPORTED>10,'10+',as.character(AQHI_REPORTED)))

        }

        # -ensure AQHI_REPORTED is char
        if (!is.character(df_$AQHI_REPORTED)) {
          df_ <- df_ %>%
            mutate(AQHI_REPORTED = round2(AQHI_REPORTED,n=0)) %>%
            mutate(AQHI_REPORTED =ifelse(AQHI_REPORTED>10,'10+',as.character(AQHI_REPORTED)))
        }




      }

      if (nrow(df_)>0) {
        df_data <- bind_rows(df_data,df_)
      }
    })
    # -clear memory
    gc()


  }


  # -for aqhi data-----
  # return results immediately
  if ('aqhi' %in% tolower(parameter_or_station)) {

    if (0) {
      a <- df_data
    }
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


    df_data <-  df_data %>%
      select(any_of(cols_aqhi))

    # -fix AQHI_VALUE and AQHI_CLASSIC
    # -all negative values change to NA
    try({
      df_data$AQHI_VALUE[df_data$AQHI_VALUE<0] <- NA
      df_data$AQHI_CLASSIC[df_data$AQHI_CLASSIC<0] <- NA
    })


    # -fix AQHI_REPORTED if it is NA but AQHI_VALUE is available
    # -change value to factor so it can be sorted
    try({

      lvls_aqhi <- 1:10
      lvls_aqhi <- c(as.character(lvls_aqhi),'10+',NA)
      df_data <- df_data %>%
        mutate(AQHI_REPORTED = ifelse(is.na(AQHI_REPORTED),
                                      ifelse(AQHI_VALUE == 11,'10+',AQHI_VALUE),
                                      AQHI_REPORTED)) %>%
        mutate(AQHI_REPORTED = factor(AQHI_REPORTED, level = lvls_aqhi))
    })

    # -fix AQHI_PLUS_PM25_VALUE, change to YES, NO for AQHI_PLUS_PM25 value
    try({
      df_data <- df_data %>%
        mutate(AQHI_PLUS_PM25_VALUE = ifelse(round2(AQHI_VALUE,0) == round2(AQHI_CLASSIC,0),
                                             'No','Yes'))
    })
    # -rename the columns
    df_data <- RENAME_COLUMN(df_data,cols_aqhi_rename$orig_name,cols_aqhi_rename$new_name)
    #df_data$DATE_PST <- extractDateTime(df_data$DATE_PST)

    # -add DATETIME time-beginning column
    df_data <- df_data %>%
      #mutate(DATE_PST = ymd_hm(DATE_PST)) %>%
      mutate(DATETIME = DATE_PST - lubridate::hours(1)) %>%
      select(PARAMETER,DATETIME,everything())

    if (clean_names == TRUE) {
      df_data <- clean_names(df_data)
    }
    message('DONE.AQHI data retrieved')

    return(df_data)
  }

  # -for non-aqhi data-----
  #DEBUG
  #when hour is 24, seems to be blank
  if (0) {
    # df0 <- df_data
    # readr::write_csv(df_data,'C:/temp/data_test.csv')
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
    if (0) {
      df0 <- df_data
    }
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

  gc()

  # -change STATION_NAME_FULL to all caps
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

  if (0) {
    #-mark for debug
    df0 <- df_data
  }

  if (pad_data) {
    suppressMessages({

      df_data <- df_data %>%
        ungroup() %>%
        filter(!is.na(RAW_VALUE))

      # -pad data for each year
      df_data$year = lubridate::year(df_data$DATE)
      years_pad <- unique(df_data$year)

      df_pad <- NULL
      for (yr in years_pad) {

        message(paste('padding data  for year:',yr))
        df_ <- df_data %>%
          filter(year == yr) %>%
          select(-year) %>%
          pad_data(date_time = 'DATE_PST',values = c('RAW_VALUE','ROUNDED_VALUE','flag_tfee','STATION_NAME_FULL'))


        # -fix NA in STATION_NAME_FULL that results from the padding process
        df_station_names_full <- ungroup(df_) %>%
          select(STATION_NAME,STATION_NAME_FULL) %>%
          distinct() %>%
          group_by(STATION_NAME) %>%
          filter(!is.na(STATION_NAME_FULL)) %>%
          slice(1) %>%
          ungroup() %>%
          RENAME_COLUMN('STATION_NAME_FULL','STATION_NAME_FULLBACKUP')

        df_ <- df_ %>%
          left_join(df_station_names_full) %>%
          mutate(STATION_NAME_FULL = ifelse(is.na(STATION_NAME_FULL),
                                            STATION_NAME_FULLBACKUP,
                                            STATION_NAME_FULL)) %>%
          mutate(STATION_NAME_FULL = ifelse(is.na(STATION_NAME_FULL),
                                            STATION_NAME,
                                            STATION_NAME_FULL)) %>%
          select(-STATION_NAME_FULLBACKUP)





        df_pad <- bind_rows(df_pad,df_)
        gc()
      }

      df_data <- df_pad
      rm(df_pad)

      # -add flagtfee column values
      # -adds NA column if flag_TFEE is FALSE
      if (flag_TFEE) {
        df_data$flag_tfee[is.na(df_data$flag_tfee)] <- FALSE
      } else {
        df_data$flag_tfee <- NA
      }

      gc()
    })
  }

  # -select the columns, remove unneeded ones
  try({
    if (is_parameter) {

      if (0) {
        df_data0 <- df_data
      }
      df_data <- df_data %>%
        arrange(PARAMETER,STATION_NAME,INSTRUMENT,DATE_PST) %>%
        COLUMN_REORDER(c('PARAMETER','DATE_PST','DATE','TIME','STATION_NAME','STATION_NAME_FULL','INSTRUMENT'))
      #done, sending results

    } else
    {

      if (use_openairformat) {
        if (0) {
          # df_data <- test
        }
        df_data <- clean_names(df_data)
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
        },silent = TRUE)

      }

      # -sort results
      # -incorporate different column options
      try({df_data <- df_data %>%
        arrange(STATION_NAME,DATE_PST)},
        silent = TRUE)
      try({df_data <- df_data %>%
        arrange(site,date)},
        silent = TRUE)

    }
  })
  # -add DATETIME time-beginning column

  try({
    df_data <- df_data %>%
      mutate(DATETIME = DATE_PST - lubridate::hours(1)) %>%
      select(PARAMETER,DATETIME,everything())
  },silent = TRUE)

  try({
    df_data <- df_data %>%
      mutate(datetime = date_pst - lubridate::hours(1)) %>%
      select(parameter,datetime,everything())
  },silent = TRUE)


  try({
    df_data <- df_data %>%
      mutate(DATETIME = DATE_PST - lubridate::hours(1)) %>%
      select(PARAMETER,DATETIME,everything())
  },silent = TRUE)

  try({
    df_data$date_pst <- extractDateTime(df_data$date_pst)
    df_data <- df_data %>%
      mutate(date_pst = ymd_hm(date_pst)) %>%
      mutate(datetime = date_pst - lubridate::hours(1)) %>%
      select(parameter,datetime,everything())
  },silent = TRUE)

  if (clean_names) {
    try({
    df_data <- clean_names(df_data)
    },silent = TRUE)
  }

  # -fix for flag_TFEE is false
  if (!flag_TFEE) {
    try({
      df_data <- df_data %>%
        select(-flag_tfee)
    },silent = TRUE)
  }
  message('DONE. Data retrieved.')
  return(df_data)

}





