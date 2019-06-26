#this R script retrieves data from ECCC

#' GET ECCC FORECAST function
#'
#' This function retrieves latest forecast from ECCC models
#' @param parameter is a vector string of AQHI, PM25, NO2, O3, PM10. It will include all forecase if NULL
#' @export
GET_ECCC_FORECAST<-function(parameter=NULL)
{
  #debug
  # parameter<-NULL
  # parameter<-c('pm25','pm10')
  #end debug

  RUN_PACKAGE(c('dplyr','tidyr','XML'))
  source.url<-'http://dd.weatheroffice.ec.gc.ca/air_quality/aqhi/pyr/forecast/model/csv/'
  description.url<-'http://dd.weatheroffice.ec.gc.ca/air_quality/doc/AQHI_XML_File_List.xml' #contains details of AQHI sites
  parameter<-tolower(parameter)
  source.files<-GET_URL_FOLDERS(source.url)%>%
    dplyr::filter(TYPE=='[TXT]')%>%
    tidyr::separate(FOLDER,into=c('DATE','PARAMETER','REGION','MODEL'),sep='_',remove=FALSE)%>%
    dplyr::mutate(PARAMETER=gsub('PM2.5','PM25',PARAMETER,ignore.case=TRUE))%>%
    dplyr::mutate(MODEL=gsub('.csv','',MODEL,ignore.case=TRUE))%>%
    dplyr::group_by(PARAMETER,MODEL)%>%
    dplyr::mutate(DATE_MAX=max(DATE))%>%
    ungroup()%>%
    dplyr::filter(DATE==DATE_MAX)

  if (!length(parameter)==0)
  {
    source.files<-dplyr::filter(source.files,tolower(PARAMETER) %in% tolower(parameter))
  }

  result<-NULL
  for (i in 1:nrow(source.files))
  {
    temp.file<-source.files[i,]
    print(paste('Analysing the file:',temp.file$FOLDER))
    temp<-read.table(file=paste(source.url,temp.file$FOLDER,sep=''),
                     sep=',',header=TRUE)%>%
      RENAME_COLUMN('stationId','NAPS_ID')%>%
      RENAME_COLUMN('cgndb','CGNDB')
    #add station and cgndb columns
    if (!('NAPS_ID' %in% colnames(temp)))
    {
      temp<-mutate(temp,NAPS_ID='')
    }
    if (!('CGNDB' %in% colnames(temp)))
    {
      temp<-mutate(temp,CGNDB='')
    }
    #note that some entries have cgndb, others stationId (NAPSId)
    temp.result<-NULL
    #creating a flat table
    for (column in colnames(temp)[!grepl('NAPS_ID',colnames(temp)) &
                                  !grepl('CGNDB',colnames(temp))
                                  ])
    {
      print(paste('Retrieving from',column))

      temp.result.date<-temp%>%
        RENAME_COLUMN(column,'FORECAST_VALUE')%>%
        dplyr::mutate(DATE=gsub('X','',column,ignore.case=TRUE))%>%
        dplyr::select(NAPS_ID,CGNDB,DATE,FORECAST_VALUE)
      temp.result<-rbind(temp.result,temp.result.date)
    }
    temp.result<-temp.result%>%
      dplyr::mutate(DATE_NEW=as.POSIXct(DATE,format='%Y%m%d%H',tz='utc'),
        PARAMETER=temp.file$PARAMETER,
                    DATE_CREATED=as.POSIXct(temp.file$DATE_MAX,format='%Y%m%d%H',tz='utc'),
                    MODEL=temp.file$MODEL)%>%
      dplyr::mutate(DATE=DATE_NEW)%>%
      dplyr::select(NAPS_ID,CGNDB,PARAMETER,DATE,DATE_CREATED,MODEL,FORECAST_VALUE)
    result<-rbind(result,temp.result)
  }

  #get details of stations and AQHI sites
  #note, temporarile retrieving station details from 2018

  #future, to grab from xml data list
  #aqhi.list<-XML::xmlParse(description.url)

  #added min() to remove duplication station with same NAPS_ID
  list.station<-GET_STATION_DETAILS_FTP(2018)%>%
    dplyr::group_by(NAPS_ID)%>%
    dplyr::mutate(TEMP=min(STATION_NAME_FULL))%>%
    dplyr::ungroup()%>%
    dplyr::filter(STATION_NAME_FULL==TEMP)%>%
    dplyr::select(STATION_NAME,NAPS_ID,LAT,LONG)%>%
    RENAME_COLUMN(c('LAT','LONG'),c('LATITUDE','LONGITUDE'))%>%
    unique()

  #the min(),max() used to remove duplicate station with same NAPS ID and station name
  list.aqhi<-GET_STATION_DETAILS_FTP()%>%
    dplyr::group_by(CGNDB,LATITUDE,LONGITUDE)%>%
    dplyr::mutate(TEMP=max(DATE_ESTABLISHED),TEMP2=min(STATION_NAME_FULL))%>%
    ungroup()%>%
    dplyr::filter(DATE_ESTABLISHED==TEMP)%>%
    dplyr::filter(STATION_NAME_FULL==TEMP2)%>%
    dplyr::select(STATION_NAME,CGNDB,LATITUDE,LONGITUDE)%>%
    dplyr::mutate(CGNDB=toupper(CGNDB))%>%
    dplyr::filter(!CGNDB %in% c('N/A','NA',''))%>%
    unique()

  #non-aqhi forecasts
  #summarise added to remove duplicate stations
  result.nonaqhi<-result%>%
  dplyr::filter(!PARAMETER=='AQHI')%>%
  dplyr::mutate(CGNDB=as.character(CGNDB))%>%
  merge(list.station,all.x=TRUE)%>%
  dplyr::filter(!is.na(STATION_NAME))
  #aqhi forecasts
  result.aqhi<-result%>%
    dplyr::filter(PARAMETER=='AQHI')%>%
    dplyr::mutate(CGNDB=as.character(CGNDB))%>%
    merge(list.aqhi,all.x=TRUE)%>%
    dplyr::filter(!is.na(STATION_NAME))


#combine non-aqhi and aqhi forecasts
rbind(result.aqhi,result.nonaqhi)%>%
  dplyr::arrange(PARAMETER,NAPS_ID,CGNDB,MODEL,DATE)%>%
  dplyr::mutate(DATE_PST=DATE)%>%
  dplyr::select(DATE,STATION_NAME,NAPS_ID,CGNDB,PARAMETER,MODEL,LATITUDE,LONGITUDE,DATE_CREATED,FORECAST_VALUE)%>%
    return()
}
