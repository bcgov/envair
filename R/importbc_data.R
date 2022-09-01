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
#'
#'@examples
#' importBC_data('Prince George Plaza 400')
#' importBC_data('pm25',2015:2016,use_openairformat = FALSE)
#' importBC_data(c('Prince George','Kamloops'),c(2010,2015))
#'
#' @export
importBC_data<-function(parameter_or_station,
                        years=NULL,use_openairformat=TRUE,
                        use_ws_vector = FALSE,pad = TRUE)

{
  #debug
  if (0)
  {

    parameter_or_station <- 'no2'
    parameter_or_station <- 'smithers'
    years <- 2019
    pad = FALSE
    use_openairformat <- TRUE

    use_ws_vector <- FALSE
  }

  #load packages
  require(dplyr)
  # RUN_PACKAGE(c('plyr','dplyr','RCurl','readr','lubridate','tidyr','stringi'))  #,'feather'
  if (is.null(years))
  {
    years=as.numeric(format(Sys.Date(),tz='etc/GMT+8',format = '%Y'))
  }

  if (is.null(pad)) {
    pad <- TRUE
  }

  #primary data location
  data.source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/'
  data.unvalidated_source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/'


  #identify if parameter or station based on the list of parameters that are in unvalidated source
  # temp_<-as.character(unlist(strsplit(RCurl::getURL(data.unvalidated_source,dirlistonly=TRUE),split='\r\n')))

  temp_ <- as.character(unlist(stringi::stri_split_lines(RCurl::getURL(data.unvalidated_source,dirlistonly=TRUE))))
  temp_<-gsub('.csv','',temp_,ignore.case=TRUE)

  #process parameter or station
  parameter_or_station <- tolower(parameter_or_station)
  parameter_or_station <- gsub('ozone','o3',parameter_or_station)
  parameter_or_station <- gsub('pm2.5','pm25',parameter_or_station)
  parameter_or_station <- gsub('wdir','wdir_vect',parameter_or_station)
  parameter_or_station <- gsub('wspd','wspd_sclr',parameter_or_station)
  parameter_or_station <- gsub('rh','humidity',parameter_or_station)


  if (any(tolower(parameter_or_station) %in% tolower(temp_)))
  {
    parameters <- parameter_or_station
    stations <- NULL

  } else
  {
    stations  <- parameter_or_station
    parameters <- NULL
  }


  #identify the latest validation cycle data
  temp_<-as.character(unlist(stringi::stri_split_lines(RCurl::getURL(data.source,dirlistonly=TRUE))))
  temp_<-temp_[nchar(temp_)==4] #get only 4-digit folders
  valcycle<-max(as.numeric(temp_),na.rm = TRUE)

  data.result<-NULL

  #data retrieval-----
  if (!is.null(parameters))
  {
    # retrieval based on parameters (not stations)----
    # scan one parameter at a time
    use_openairformat = FALSE #it will not be openair format
    for (parameter in parameters)
    {
      #scan one year at a time, create a dataframe combining all years
      for (data.year in years)
      {

        #get the file source
        if (data.year>valcycle)
        {
          source_<-data.unvalidated_source
        } else
        {
          source_<-paste(data.source,data.year,'/',sep='')
        }

        list.data<-paste(source_,parameter,".csv",sep='')

        print(paste('Retrieving data from:',list.data))

        df_data <- NULL
        try(df_data <-readr::read_csv(list.data,
                                      col_types = readr::cols(
                                        DATE_PST = readr::col_datetime(),
                                        NAPS_ID = readr::col_character(),
                                        EMS_ID = readr::col_character(),
                                        RAW_VALUE = readr::col_double(),
                                        ROUNDED_VALUE = readr::col_double()
                                      )) %>%
              dplyr::mutate(VALIDATION_STATUS=ifelse(data.year<= valcycle,
                                                     'VALID','UNVERIFIED'))
        )

        try(df_data$DATE_PST <- lubridate::force_tz(df_data$DATE_PST,tz='etc/GMT+8'))
        #remove DATE,TIME column if it is there
        try(
          df_data <- df_data %>%
            dplyr::select(-DATE,-TIME),
          silent = TRUE
        )

        #filter data to specified year
        #dplyr::filter year----
        if (0)
        {
          df_data <- df_data %>%
            dplyr::filter(STATION_NAME == 'Prince George Plaza 400')
        }




        if (!is.null(df_data))
        {
          print('adding the file to retrieved data')

          df_data$year_ <-  lubridate::year(df_data$DATE_PST - lubridate::hours(1))
          df_data <- df_data[df_data$year_ == data.year,]
          df_data <- subset(df_data,select=-year_)
          data.result <- dplyr::bind_rows(data.result, df_data)

        }


      }
    }



    #stop if there are no result
    if (is.null(data.result))
    {
      return(NULL)
    }

    #fix for no NAPS bug
    #insert NAPS ID
    if (!'NAPS_ID' %in% colnames(data.result))
    {
      data.result <- data.result %>%
        left_join(
          listBC_stations(data.year) %>%
            dplyr::select(STATION_NAME,NAPS_ID) %>%
            unique()
        )
    }

    #fix for no STATION_NAME_FULL
    if (!'STATION_NAME_FULL' %in% colnames(data.result))
    {
      data.result <- data.result %>%
        dplyr::mutate(STATION_NAME_FULL = STATION_NAME)
    }

    # remove duplicates (added 2022-08-18)
    rows_rawdata <- nrow(data.result)   #rows before removing duplicates
    print('checking for duplicates')
    data.result <- data.result %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(RAW_VALUE)) %>%
      arrange(STATION_NAME,DATE_PST) %>%
      dplyr::group_by(DATE_PST,STATION_NAME,PARAMETER,INSTRUMENT) %>%
      dplyr::mutate(index = 1:n(),count=n()) %>%
      dplyr::filter(index==1) %>%
      dplyr::select(-index,-count) %>%
      dplyr::ungroup()

    if (pad) {
      data.result <- pad_data(data.result,add_DATETIME = TRUE)
    }



  } else
  {
    #retrieve station data, not parameters-----
    #search based on the station name,if it matches
    list.stations<-listBC_stations()%>%
      dplyr::filter(tolower(STATION_NAME) %in% tolower(stations))

    #if no match is found in stations, it will just use the search item
    #as a keyword
    if (nrow(list.stations)==0)
    {
      print('Exact match not found, locating for similar stations')
      list.stations<-listBC_stations()%>%
        dplyr::filter(grepl(stations,STATION_NAME,ignore.case=TRUE))
    }

    if (nrow(list.stations)>0)
    {
      #that means there were stations on the list
      #retrieve data from each station
      for (ems_ in unique(list.stations$EMS_ID))
      {
        if (0) {
          ems_ <- 'E315110'
          data.year <- 2018
          data.year <- 2019
        }
        #retrieve data from each year for that specified station
        for (data.year in years)
        {
          print(paste('Retrieving data from: EMS=',ems_,'Year=',data.year))
          #determine the source of data
          if (data.year> valcycle)
          {
            source_<-paste(data.unvalidated_source,'STATION_DATA/',ems_,'.csv',sep='')
          } else
          {
            source_<-paste(data.source,data.year,'/STATION_DATA/',ems_,'.csv',sep='')
          }

          temp_<-NULL
          temp_<-try(as.character(unlist(stringi::stri_split_lines(RCurl::getURL(source_,dirlistonly=TRUE)))))

          sourcefile_<-unlist(strsplit(source_,split='/'))
          sourcefile_<-sourcefile_[length(sourcefile_)]

          if (sourcefile_ %in% temp_)
          {

            print(paste('Downloading data from:',source_))

            data.result_ <- NULL
            try(
              data.result_ <- readr::read_csv(source_,
                                                 col_types = readr::cols(
                                                   DATE_PST = readr::col_datetime(),
                                                   NAPS_ID = readr::col_character(),
                                                   EMS_ID = readr::col_character(),
                                                   RAW_VALUE = readr::col_double(),
                                                   ROUNDED_VALUE = readr::col_double()
                                                 )) %>%
                                   dplyr::mutate(VALIDATION_STATUS=ifelse(data.year<= valcycle,
                                                                          'VALID','UNVERIFIED'))
            )

            #process date, filter by year
            try(data.result_$DATE_PST <- lubridate::force_tz(data.result_$DATE_PST,tz='etc/GMT+8'))
            try({
              data.result_$year <- lubridate::year(data.result_$DATE_PST - lubridate::hours(1))
              data.result_ <- data.result_[data.result_$year == data.year,]
              data.result_ <- data.result_ %>% dplyr::select(-year)
            })


            #remove some columns, make it consistent with data from other years
            try({

              data.result_ <- data.result_[,!grepl('_UNITS',colnames(data.result_),)]
              data.result_ <- data.result_[,!(colnames(data.result_) %in% c('NAPS_ID','LATITUDE','LONGITUDE'))]


            })

            try(
              data.result <- dplyr::bind_rows(data.result,data.result_)
            )
          }

        }
      }
    }

    #stop if there are no result
    if (is.null(data.result))
    {
      print('No Data found.')
      return(NULL)
    }


    try(
      data.result <- data.result%>%
        select(-DATE,-TIME),
      silent = TRUE
    )

    #debug pre-load data
    if (0)
    {
      data.result <- readRDS('./test_data/data_station.Rds')
      use_openairformat <- TRUE
      pad <- TRUE
      use_ws_vector <- TRUE
    }


    if (use_openairformat)
    {

      #subtract one hour from time
      #switch to time-beginning
      data.result <-  data.result%>%
        dplyr::mutate(DATE_PST = DATE_PST - lubridate::hours(1))

      #change column names to lower case
      colnames(data.result) <- tolower(colnames(data.result))

      #remove date, time column
      try(
        data.result <- data.result %>%
          select(-date,-time),
        silent = TRUE
      )

      #date becomes the date_time column
      data.result <- data.result %>%
        dplyr::rename(date = date_pst)

      if (use_ws_vector)
      {
        #ws is vector wind speed (not default)
        # data.result<-

        try(data.result <- data.result%>%
              dplyr::rename(ws = wspd_vect)
        )

        try(data.result <- data.result%>%
              dplyr::rename(wd = wdir_vect)
        )

        #use wspd_scalar as ws if ws is not available
        #use wdir_uvec as wd if wd is  not available

        column_<-colnames(data.result)
        if (!('ws' %in% column_))
        {
          try(
            data.result<-data.result%>%
              dplyr::rename(ws = wspd_vect)
          )
        }
        if (!('wd' %in% column_))
        {
          try(
            data.result<-data.result%>%
              dplyr::rename(wd = wdir_uvec)
          )

        }


      } else
      {
        #ws is scalar wind speed #default
        try(
          data.result<-data.result%>%
            dplyr::rename(ws = wspd_sclr)
        )

        try(
          data.result<-data.result%>%
            dplyr::rename(wd = wdir_vect)
        )


        #use wspd_scalar as ws if not available
        #use wdir_uvec as wd if not available

        column_<-colnames(data.result)
        if (!('ws' %in% column_))
        {
          try(
            data.result<-data.result%>%
              dplyr::rename(ws = wspd_sclr)
          )
        }
        if (!('wd' %in% column_))
        {
          try(
            data.result<-data.result%>%
              dplyr::rename(wd = wdir_uvec)
          )
        }
      }


      #check again if there is now wind speed/direction, if not, just create ws, wd columns
      column_<-colnames(data.result)
      if (!('ws' %in% column_))
      {
        data.result<-data.result%>%
          dplyr::mutate(ws=NA)
      }
      if (!('wd' %in% column_))
      {
        data.result<-data.result%>%
          dplyr::mutate(wd=NA)
      }


    }


    if (pad)
    {
       cols_ <- colnames(data.result)
       cols_instrument <- cols_[grepl('_instrument',cols_,ignore.case=TRUE)]
       cols_unit <- cols_[grepl('_units',cols_,ignore.case=TRUE)]
       cols_vals <- cols_[grepl('_raw',cols_,ignore.case=TRUE)]
       cols_vals <- unique(c('validation_status','VALIDATION_STATUS','WS','WD','ws','wd',cols_vals,gsub('_raw','',cols_vals,ignore.case =TRUE)))


       data.result <- data.result %>%
         pad_data(date_time= c('date','DATE_PST')[c('date','DATE_PST') %in% cols_],
                  values = c(cols_vals,cols_instrument,cols_unit),
                  add_DATETIME = !use_openairformat)
    }

  }


  #stop if there are no result
  if (is.null(data.result))
  {
    print('No data found.')
    return(NULL)
  }

  print(paste('Done.',nrow(data.result),'rows'))
  return(data.result)
}

#' Average the imported data function
#'
#' This function averages the imported data
#'
#' @param parameter is the parameter to average.use list_parameters() for list of parameters
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


  #add datetime_ columns for time-beginning processing


  if (averaging_type %in% c('1-hour','1hr','1-hr','1','hr','1h','hour'))
  {

    df <-   df %>%
      pad_data()
    return(df)
  }

  if (averaging_type %in% c('24-hour','24hr','24-hr','24','day','24h','24 hr','24 h'))
  {
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

    print('Retrieving 8-hour running average')
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
