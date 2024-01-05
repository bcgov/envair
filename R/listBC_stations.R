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


#' List BC Air Quality Monitoring Stations
#'
#' This function retrieves latest station details or deteails during specific year from the ftp feed
#'
#' @param year the year where station details are retrieved from. Defaults to current year if undefined
#' This is not a vector
#' @param use_CAAQS default FALSE. If TRUE, it will return the station list that is
#' used for air zone and other reporting purposes. This list is from the
#' Excel file located in:'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx'

#' @examples
#' listBC_stations()
#' listBC_stations(2015)
#' listBC_stations(use_CAAQS = TRUE)
#'
#' @export
#'
listBC_stations <- function(year=NULL,use_CAAQS = FALSE,merge_Stations = FALSE)
{
  if (0) {
    source('./r/get_caaqs_Stn_history.R')
    source('./r/envairfunctions.R')
    year <- 2005
    use_CAAQS = TRUE
  merge_Stations = FALSE
year <- NULL
}
  require(dplyr)

  # -retrieve initial value from station list
  df_result <- NULL
  df_result <- listBC_stations_()
  df_result <- clean_stationnames(df_result)


  # -if CAAQS is specified, more details from the BC_CAAQS_station_history.xlsx file
  if (use_CAAQS) {
    message('Retrieving Station List from CAAQS History Table')

    # -retrieve data from the CAAQS station excel sheet
    result <- get_excel_table('ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx',
                    sheet = 'Monitoring Station',header_row = 2) %>%
      clean_stationnames()


    resultpurpose <- get_excel_table('ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx',
                              sheet = 'Station Purpose',header_row = 2)%>%
      clean_stationnames()

    # -remove any conflicting columns
    cols_result <- colnames(result)
    cols_resultpurpose <- colnames(resultpurpose)

    cols_result_remove <- cols_result[cols_result %in% cols_resultpurpose]
    cols_result_remove <- cols_result_remove[!grepl('STATION_NAME',cols_result_remove)]

    result <- result %>%
      select(-one_of(cols_result_remove)) %>%
      left_join(resultpurpose)

    df_result <- df_result %>%
      select(STATION_NAME,STATION_NAME_FULL) %>%
      distinct() %>%
      left_join(result)

    df_result <- dplyr::filter(df_result,!is.na(STATION_NAME))

    #retrieve station history for merging details
    df_merge <- get_station_history() %>%
      select(STATION_NAME,`Merged Station Name`) %>%
      filter(!is.na(`Merged Station Name`)) %>%
      dplyr::rename(site = `Merged Station Name`) %>%
      unique()
    #merge
    df_result <- df_result %>%
      left_join(df_merge) %>%
      mutate(site = ifelse(is.na(site),STATION_NAME,site)) %>%
      mutate(Label = ifelse(is.na(Label),site,Label)) %>%
      COLUMN_REORDER(c('STATION_NAME','STATION_NAME_FULL','site','Label'))
  }


  if (!is.null(year)) {
    result_prev <- NULL
    try(
      {
        result_prev <- listBC_stations_(year=year)
        cols_now <- colnames(result_now)
        cols_prev <- colnames(result_prev)
        cols_add <- cols_prev[!cols_now %in% cols_prev]
        cols_add <- c(cols_add,'STATION_NAME_FULL')
        result_prev <- result_prev %>%
          dplyr::left_join(result_now %>%
                             select(cols_add))

        df_result <- result_prev %>%
          dplyr::filter(!is.na(STATION_NAME))

      }
    )
  }



  df_result <- df_result %>%
    dplyr::filter(!is.na(STATION_NAME))




  return(df_result)
}


#' Main listBC_stations() function
#'
#' this was created to ensure output is up-to-date
#' when user specifies a year
listBC_stations_<-function(year=NULL)
{
  #2019-05-13
  #retrieves the station details from the BC ftp link
  #if useLATEST, it will retrieve from the web BC station details
  #contains CGNDB information


  #debug
  if (0)
  {
    year<-NULL
  }
  require(dplyr)

  if (is.null(year))
  {year<- as.numeric(format(Sys.Date(),'%Y'))}

  #RUN_PACKAGE(c('dplyr','RCurl','readr','tibble','stringi'))

  # dir.temp<-paste(getwd(),'/TEMP',sep="")
  # file.temp<-paste(dir.temp,'/stationdetails.csv',sep="")
  # dir.create(dir.temp,showWarnings = FALSE)

  #identify the latest validation cycle
  temp_<-as.character(unlist(stringi::stri_split_lines(RCurl::getURL("ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/",
                                                                     dirlistonly=TRUE))))
  temp_<-temp_[nchar(temp_)==4] #get only 4-digit folders
  valcycle<-max(as.numeric(temp_),na.rm = TRUE)


  if (year> valcycle)
  {ftp.station<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Air_Monitoring_Stations/bc_air_monitoring_stations.csv'
  } else
  { ftp.station<-paste("ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/",
                       year,"/bc_air_monitoring_stations.csv",sep="")
  }

  message('Retrieving station details from FTP...')
  #  dir.temp<-paste(getwd(),'/TEMP',sep="")
  # file.temp<-paste(dir.temp,'/stationdetails.csv',sep="")
  #temp<-RCurl::getURL(ftp.station,ftp.use.epsv=FALSE,header=TRUE)

  # download.file(ftp.station,destfile=file.temp,quiet=FALSE)
  #updated 2020-06-10 fix for ON,1 for active, OFF, 0 for inactive
  station.details<-readr::read_csv(ftp.station)%>%
    dplyr::mutate(STATUS = as.character(STATUS)) %>%
    dplyr::mutate(STATUS=dplyr::recode(STATUS,"ON"="ACTIVE",
                                       "OFF"="INACTIVE",
                                       "1" = "ACTIVE",
                                       "0" = "INACTIVE")
    ) %>%
    mutate(STATION_NAME = gsub('[^[:alnum:]]',' ',STATION_NAME))


  #fix if there are no NOTES column
  if (!any('NOTES' %in% colnames(station.details)))
  {
    station.details<-station.details%>%
      dplyr::mutate(NOTES='N/A')
  }
  if (!any('SERIAL_CODE' %in% colnames(station.details)))
  {
    station.details<-station.details%>%
      dplyr::mutate(SERIAL_CODE='UNKNOWN')
  }

  station.details.aqhi<-station.details%>%
    dplyr::filter(grepl("CGNDB=",NOTES))%>%
    dplyr::select(SERIAL_CODE,STATION_NAME,NOTES)

  station.details.aqhi.CGNDB<-NULL
  for (i in 1:nrow(station.details.aqhi))
  {
    #extract CGNDB

    temp.station<-station.details.aqhi[i,]
    temp.NOTES<-as.character(temp.station$NOTES[1])
    temp.NOTES.CGNDB<-as.vector(unlist(strsplit(temp.NOTES,";")))

    temp.CGNDB<-as.character(temp.NOTES.CGNDB[grepl('CGNDB=',temp.NOTES.CGNDB)][1])
    temp.CGNDB<-unlist(strsplit(temp.CGNDB,"="))[2]

    if (!is.null(temp.CGNDB))
    {
      temp<-temp.station%>%
        dplyr::mutate(CGNDB=temp.CGNDB)
      station.details.aqhi.CGNDB<-station.details.aqhi.CGNDB%>%
        dplyr::bind_rows(temp)
    }
  }

  #Create detailed list of AQHI stations with CGNDB
  station.details.aqhi<-station.details%>%
    dplyr::filter(SERIAL_CODE %in% station.details.aqhi.CGNDB$SERIAL_CODE)%>%
    merge(station.details.aqhi.CGNDB,all.y=TRUE)

  station.details<-station.details%>%
    dplyr::filter(!SERIAL_CODE %in% station.details.aqhi.CGNDB$SERIAL_CODE)%>%
    dplyr::mutate(CGNDB="N/A")%>%
    dplyr::bind_rows(station.details.aqhi)

  #add station_name_full is not there yet
  if (!any('STATION_NAME_FULL' %in% colnames(station.details)))
  {
    #this makes column called STATION_NAME that has no suffix
    #and also creates a column called STATION_NAME_FULL
    station.details<-station.details%>%
      dplyr::mutate(STATION_NAME_FULL=STATION_NAME)%>%
      dplyr::mutate(STATION_NAME=gsub(' Met_60$','',STATION_NAME_FULL,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_60$','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub(' Met_15$','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_15$','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub(' Met_1$','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_1$','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_OLD$','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub(' Met$','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_Met$','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_Amb$','',STATION_NAME,ignore.case=TRUE))%>%
      COLUMN_REORDER(columns=c("SERIAL_CODE","EMS_ID","STATION_NAME",'STATION_NAME_FULL'))



  }

  station.details <- data.frame(lapply(station.details, as.character), stringsAsFactors=FALSE)

  # file.remove(file.temp)   #delete the temporary file
  station.details <- tibble::as_tibble(station.details)

  #retrieve air zone details

  #remove airzone column
  try(
    station.details <- station.details %>%
      dplyr::select(-AIRZONE),
    silent = TRUE
  )

  if (0) {
    #debug airzone null
    # readr::write_csv(station.details,'temp.csv')
    require(dplyr)
    source('./r/listBC_stations.R')
    station.details <- readr::read_csv('temp.csv')
  }

  airzone_details <-
    station.details %>%
    dplyr::mutate(lat = as.numeric(LAT), lon = as.numeric(LONG), ems_id = EMS_ID) %>%
    dplyr::filter(!is.na(lat),!is.na(lon),abs(lat)<=90,abs(lat)>0) %>%
    rcaaqs::assign_airzone(bcmaps::airzones()) %>%
    dplyr::rename(AIRZONE = airzone) %>%
    dplyr::select(LAT,LONG,AIRZONE) %>%
    distinct()

  station.details <- station.details %>%
    dplyr::left_join(airzone_details)

  station.details <- clean_stationnames(station.details)
  return(station.details)

}

#' List available parameters
#'
#' This function lists the parameters that are available for retrieval
#'
#' @examples
#' list_parameters()
#'
#' @export
list_parameters <- function()
{
  # initial parameter list
  # list will still be populated from year_to_date csv
  lst_params <- c('aqhi','co','h2s','hf','humidity','no','no2','nox',
                  'o3','pm10','pm25','precip','pressure','snow','so2','temp_mean','trs',
                  'vapour_pressure','wdir_uvec','wdir_vect','wspd_sclr','wspd_vect')
  # RUN_PACKAGE(c('RCurl','dplyr','stringi'))
  ftpsource_ <- 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/binary/'
  temp_<-as.character(unlist(stringi::stri_split_lines(RCurl::getURL(ftpsource_,dirlistonly=TRUE))))
  temp_ <- temp_[!grepl('station',temp_,ignore.case=TRUE)]
  temp_ <- tolower(gsub('.csv|.parquet','',temp_,ignore.case=TRUE))
  temp_ <- sort(temp_)
  temp_ <- temp_[temp_ !=""]
  temp_ <- sort(unique(c(lst_params,temp_)))

  return(temp_)
}

#' This function clean up station names
#' Removes extra spaces, and non-alphanumeric characters
clean_stationnames <- function(df,cols = c('STATION_NAME','STATION_NAME_FULL')) {
  # -function cleans up stations names by removing spaces, non-alphanumeric characters
  if (0) {
    df <- importBC_data('pm25',2020)
    cols = c('station_name','station_name_full')
  }

  # -ensure specified column is there
  cols_ <- colnames(df)
  cols <- paste(cols,collapse ='|')
  cols <- cols_[grepl(cols,cols_,ignore.case = TRUE)]

  for (cols_ in cols) {
    df <- df %>%
      mutate(!!cols_ := gsub('[^[:alnum:]]',' ',!!sym(cols_)))
    df <- df %>%
      mutate(!!cols_ := gsub('  ',' ',!!sym(cols_)))
  }

  return(df)

}
