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
    parameter_or_station <- 'wdir_vect'
    parameter_or_station <- 'smithers'
    years <- 2020
    pad = TRUE
    use_openairformat <- TRUE
    use_ws_vector <- FALSE
    flag_TFEE = FALSE
    merge_Stations = FALSE

  }

  #no padding, quick query if flag_TFEE or merge_station
  if (flag_TFEE | merge_Stations){
    #better remove duplicates, and fix names
    pad <- TRUE
  }
  #use original function to retrieve data
  df <- importBC_data_(parameter_or_station = parameter_or_station,
                       years=years,
                       use_openairformat=use_openairformat,
                       use_ws_vector = use_ws_vector,
                       pad = pad
  )

  #return with original query if no TFEE flags or merging are needed

  if (flag_TFEE) {

    #if there is no paramter column, it means it is a station data listing
    if (!'PARAMETER' %in% colnames(df)) {
      print('flag_TFEE=TRUE option is currently not available with Station Data query')
      return(NULL)
    }

    print('Adding TFEE flags')
    df <- df %>%
      add_TFEE(station_column = 'STATION_NAME',date_column = 'DATE',param_column = 'PARAMETER')
    print('Done. TFEE flags added.')

  }

  if (merge_Stations) {
    #if there is no paramter column, it means it is a station data listing
    if (!'PARAMETER' %in% colnames(df)) {
      print('merge_Stations=TRUE option is currently not available with Station Data query.
            Please use parameter (PM2.5, O3, NO2, SO2,etc)')
      return(NULL)
    }
    #merge station names
    print('Merging Stations based on CAAQS/station history.....')
    df <- merge_STATIONS(df)
    print('Done. Stations merged')
  }
  return(df)
}

#' Main data retrieval function
#'
#' Deprecated since envair 0.2.1.000. use importBC_data() instead
#' This was original function since envair 0.1.0.000
#' It is still the back-end to retrieving data
#'
#' @param years the years that will be retrieved. For sequence, use 2009:2015. For non
#' sequential years, use c(2010,2015,2018)
#' If not declared, the current year will be used
#' @param use_openairformat is boolean,if TRUE, output is compatible with openair. Apples only to station queries
#' @param use_ws_vector use vector wind speed? default is FALSE, if TRUE and use_openairformat is TRUE, ws is the vector wind speed
#' @param pad default is TRUE. if FALSE, it removes all NaNs
#' @param caaqs default is FALSE. If TRUE, it adds fields for TFEE, \n
#' and merges station names
importBC_data_<-function(parameter_or_station,
                         years=NULL,use_openairformat=TRUE,
                         use_ws_vector = FALSE,pad = TRUE)

{
  #debug
  if (0)
  {
    parameter_or_station <-"Crofton Substation"
    years <- 2019:2021



    parameter_or_station <- 'aqhi'
    parameter_or_station <- 'smithers'
    years <- 2020
    pad = TRUE
    use_openairformat <- FALSE

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
  # rename to a matching standard naming system
  parameter_or_station <- tolower(parameter_or_station)

  df_parameter_list <- tribble(
    ~parameter,~standard_name,
    'ozone','o3',
    'pm2.5','pm25',
    'wdir','wdir_vect',
    'wspd','wspd_sclr',
    'rh','humidity',
    'precipitation','precip',
    'temperature','temp_mean',
    'temp','temp_mean'
  )



  #identify if the user enter parameter or station
  if (any(tolower(parameter_or_station) %in% tolower(c(temp_,df_parameter_list$parameter))))
  {
    if (0) {
      parameter_or_station <- c('pm2.5','aqhi','temp')
    }
    parameters <- parameter_or_station
    stations <- NULL
    use_openairformat <- FALSE


    #process parameter, rename to standard name
    parameters_ <- NULL
    for (param_ in parameters) {
      if (param_ %in% df_parameter_list$parameter) {
        #rename to the standard name
        param_ <- df_parameter_list$standard_name[df_parameter_list$parameter == param_]
      }
      parameters_ <- c(parameters_,param_)
    }

    parameters <- unique(parameters_)
  } else  {
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

        #scan data to find column names, etc
        try(df_data_scan <- readr::read_csv(list.data,n_max=0))
        cols_data <- colnames(df_data_scan)

        #if column include RAW_DATA
        #then process like below to ensure proper data type
        if ('RAW_VALUE' %in% cols_data) {
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
        } else {
          #define all possible raw value
          cols_rawvalue <- c('AQHI_REPORTED')


          try({
            df_data <- readr::read_csv(list.data,col_types = readr::cols(.default = "c"))
            df_data$DATE_PST <- lubridate::ymd_hm(df_data$DATE_PST, tz='etc/GMT+8')
            cols_rawvalue <- cols_data[tolower(cols_data) %in% tolower(cols_rawvalue)][1]
            df_data <-  df_data %>%
              dplyr::mutate_(RAW_VALUE = cols_rawvalue) %>%
              mutate(date_ = DATE_PST - lubridate::hours(1)) %>%
              mutate(DATE = lubridate::date(date_),
                     TIME = as.character(DATE_PST,format = '%H:%M')) %>%
              mutate(TIME = ifelse(TIME == '00:00','24:00', TIME)) %>%
              select(-date_) %>%
              mutate(RAW_VALUE= as.numeric(RAW_VALUE)) %>%
              mutate(ROUNDED_VALUE = round2(RAW_VALUE,0),
                     PARAMETER = toupper(parameter), INSTRUMENT=NA)

            #reorder columns
            cols_ordered <- c('DATE_PST','DATE','TIME','PARAMETER','AQHI_AREA','STATION_NAME',
                              'STATION_NAME_FULL','RAW_VALUE','ROUNDED_VALUE','AQHI_REPORTED')
            cols_ordered <- cols_ordered[tolower(cols_ordered) %in% tolower(colnames(df_data))]

            df_data <- COLUMN_REORDER(df_data,c(cols_ordered,'RAw_VALUE','ROUNDED_VALUE'))

          })

        }

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
          print(paste('Adding',nrow(df_data),'hours of data'))

          df_data$year_ <-  lubridate::year(df_data$DATE_PST - lubridate::hours(1))
          df_data <- df_data[df_data$year_ == data.year,]
          df_data <- subset(df_data,select=-year_)
          data.result <- dplyr::bind_rows(data.result, df_data)
          print(paste('Total rows of data:',nrow(data.result)))

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
            distinct()
        )
    }

    #fix for no STATION_NAME_FULL
    if (!'STATION_NAME_FULL' %in% colnames(data.result))
    {
      data.result <- data.result %>%
        dplyr::mutate(STATION_NAME_FULL = STATION_NAME)
    }


    #if AQHI parameter, send the result
    if (tolower(parameter) == 'aqhi') {
      return(data.result)
    }
    # remove duplicates (added 2022-08-18)
    rows_rawdata <- nrow(data.result)   #rows before removing duplicates
    print('checking for duplicates')
    data.result <- data.result %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(RAW_VALUE)) %>%
      arrange(STATION_NAME,DATE_PST) %>%
      dplyr::group_by(DATE_PST,STATION_NAME,PARAMETER,INSTRUMENT) %>%
      dplyr::mutate(index = 1:n()) %>% #slice(1) is too slow
      filter(index == 1) %>% select(-index) %>%
      dplyr::ungroup()

    if (pad) {

      data.result <- pad_data(data.result,add_DATETIME = TRUE)
    }



  } else  {
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

      cols_ <- colnames(data.result)
      cols_instrument <- cols_[grepl('_instrument',cols_,ignore.case=TRUE)]
      cols_unit <- cols_[grepl('_units',cols_,ignore.case=TRUE)]
      cols_vals <- cols_[grepl('_raw',cols_,ignore.case=TRUE)]
      cols_vals <- unique(c('validation_status','VALIDATION_STATUS','WS','WD','ws','wd',cols_vals,gsub('_raw','',cols_vals,ignore.case =TRUE)))
      cols_remove <- c('WEB_STATUS','API_STATUS','')

      cols_remove <- cols_[!cols_ %in% c('DATE_PST','STATION_NAME',cols_instrument,cols_unit,cols_vals)]

      data.result <- data.result %>%
        RENAME_COLUMN(cols_remove)

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
      try(data.result <- data.result %>%
            dplyr::select(-DATE,-TIME),silent = TRUE)
      cols_ <- colnames(data.result)
      cols_instrument <- cols_[grepl('_instrument',cols_,ignore.case=TRUE)]
      cols_unit <- cols_[grepl('_units',cols_,ignore.case=TRUE)]
      cols_vals <- cols_[grepl('_raw',cols_,ignore.case=TRUE)]
      cols_vals <- unique(c('validation_status','VALIDATION_STATUS','WS','WD','ws','wd',cols_vals,gsub('_raw','',cols_vals,ignore.case =TRUE)))

      #added to fix duplicated when STATION_NAME_FULL is null in other years
      #added 2022-11-21
      #analyze the merge criteria and fix for any problem with merge

      #remove DATE and TIME during pad
      #This will be reintroduced anyways
      try(data.result <- data.result %>%
            select(-DATE,-TIME),silent = TRUE)

      #fix for bug, for duplicate data due to NULL STATION_NAME_FULL

      # try({
      #   if ('STATION_NAME_FULL' %in% cols_ & any(is.na(data.result$STATION_NAME_FULL))) {
      #
      #     cols_vals <- cols_[cols_ %in% cols_vals]
      #
      #     # data.result <- data.result %>%
      #     #   ungroup() %>%
      #     #   dplyr::mutate(dataindex = 1:n())
      #
      #     # adding data index
      #     data.result <- data.result %>%
      #       ungroup()
      #
      #     merge_criteria <- data.result %>%
      #       filter(STATION_NAME %in% data.result$STATION_NAME[is.na(data.result$STATION_NAME_FULL)]) %>%
      #       select(-cols_unit,-cols_vals,-cols_instrument,-DATE_PST) %>%
      #       distinct() %>%
      #       arrange(STATION_NAME_FULL) %>%
      #       group_by(STATION_NAME) %>%
      #       dplyr::mutate(count = n()) %>%
      #       filter(count>1) %>%
      #       tidyr::pivot_longer(cols = -c('STATION_NAME'),
      #                           names_to = "key",
      #                           values_to = "val",
      #                           values_transform = list(val = as.character))
      #
      #     if (nrow(merge_criteria)>0) {
      #       remove_criteria <- merge_criteria %>%
      #         arrange(val) %>%
      #         group_by(STATION_NAME,key) %>%
      #         dplyr::mutate(count = n(), index = 1:n()) %>%
      #         filter(count>1)
      #
      #       #separate result into those with issue in STATION_NAME
      #       data.result0 <- data.result %>%
      #         filter(!STATION_NAME %in% remove_criteria$STATION_NAME)
      #
      #
      #
      #       data.result1 <- data.result  %>%
      #         filter(STATION_NAME %in% remove_criteria$STATION_NAME) %>%
      #         RENAME_COLUMN(remove_criteria$key)
      #
      #       #reinsert the columns, removing the duplicates
      #       remove_criteria <- remove_criteria %>%
      #         filter(index==1) %>%
      #         select(-count,-index) %>%
      #         tidyr::pivot_wider(names_from = key,values_from = val)
      #
      #       data.result1 <- data.result1 %>%
      #         merge(remove_criteria)
      #
      #       #change data types to match with original dataset
      #
      #       df_1 <- as.data.frame(data.result0)
      #       df_2 <- as.data.frame(data.result1)
      #       common <- names(df_2)[names(df_2) %in% colnames(remove_criteria)]
      #
      #       df_1[common] <- lapply(common, function(x) {
      #         as.character(df_1[[x]])
      #       })
      #       df_2[common] <- lapply(common, function(x) {
      #        as.character(df_2[[x]])
      #       })
      #
      #
      #       data.result <- df_1 %>%
      #         dplyr::bind_rows(df_2)
      #       try(data.result <- data.result %>% select(-count), silent = TRUE)
      #       try(data.result <- data.result %>% select(-index), silent = TRUE)
      #
      #       data.result <-  data.result %>%
      #
      #         tidyr::pivot_longer(cols = c(cols_vals,cols_instrument),
      #                             names_to = "key",
      #                             values_to = "val",
      #                             values_transform = list(val = as.character)) %>%
      #         group_by(STATION_NAME,STATION_NAME_FULL,DATE_PST,key) %>%
      #         arrange(val) %>%
      #         dplyr::mutate(count =n(), index =1:n()) %>%
      #         filter(index==1) %>%
      #         ungroup() %>%
      #         select(-index,-count) %>%
      #         tidyr::pivot_wider(names_from = key, values_from = val) %>%
      #         arrange(STATION_NAME,DATE_PST) %>%
      #         COLUMN_REORDER(cols_)
      #
      #
      #     }

#
#         }
#       })

      # data.result <-
      data.result %>%
        pad_data(date_time= c('date','DATE_PST')[c('date','DATE_PST') %in% cols_],
                 values = c(cols_vals,cols_instrument,cols_unit),
                 add_DATETIME = !use_openairformat) %>%
        View()

    }

  }


  #stop if there are no result
  if (is.null(data.result))
  {
    print('No data found.')
    return(NULL)
  }

  print(paste('Done.',nrow(data.result),'rows'))

  #add DATE, and TIME columns if not included (sometimes it is not)
  cols <- colnames(data.result)
  if (!all(c('DATE','TIME') %in% cols) & !use_openairformat)
  {
    print('adding DATE,TIME columns')
    data.result <- data.result %>%
      dplyr::mutate(datetime_ = DATE_PST - lubridate::hours(1)) %>%
      dplyr::mutate(DATE = lubridate::date(datetime_),
                    TIME = as.character(DATE_PST,format = '%H:%M')) %>%
      dplyr::mutate(TIME = ifelse(TIME == '00:00','24:00',TIME)) %>%
      COLUMN_REORDER(c('DATE_PST','DATE','TIME')) %>%
      dplyr::select(-datetime_)
  }

  data.result %>%
    ungroup() %>%
    as.data.frame() %>%
    return()
}



