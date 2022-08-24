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
#'
#' @examples
#' listBC_stations()
#' listBC_stations(2015)
#'
#' @export
listBC_stations<-function(year=NULL)
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

  print('Retrieving station details from FTP...')
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
    )

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
  # RUN_PACKAGE(c('RCurl','dplyr','stringi'))
  ftpsource_ <- 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/'
  temp_<-as.character(unlist(stringi::stri_split_lines(RCurl::getURL(ftpsource_,dirlistonly=TRUE))))
  temp_ <- temp_[!grepl('station',temp_,ignore.case=TRUE)]
  temp_ <- tolower(gsub('.csv','',temp_,ignore.case=TRUE))
  temp_ <- sort(temp_)
  temp_ <- temp_[temp_ !=""]
  return(temp_)
}