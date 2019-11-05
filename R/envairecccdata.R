#this R script retrieves data from ECCC

#' GET_FILE_CSV
#'
#' This function retrieves csv file from ftp, save local copy. It only retrieves updated files
#' @param list.files is a vector string containing filesnames with extension
#' @param ftp.path is the ftp path where data will be saved
#' @param path.local is the local directory where the file will be saved. Default is NULL where temp folder is in the workpath
#' @export
GET_FILE_CSV<-function(list.files,ftp.path,path.local=NULL,clean=TRUE)
{
  #debug
  if (0)
  {
    #list.files<-c('E270963.csv','E289309.csv','E223756.csv','e2225.csv','E22322')
    # ftp.path<-'ftp.env.gov.bc.ca/pub/outgoing/AIR_TEST/Hourly_Raw_Air_Data/Station/'
    # path.local<-NULL
    list.files="co.csv"
    ftp.path="ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2017/"
    path.local="A:/Air/Operations ORCS/Technology/Software Codes ScriptsExecutables/R_SCRIPTS/03_BCGovR/envair/R/temp_valid/2017"
    "A:/Air/Operations ORCS/Technology/Software Codes ScriptsExecutables/R_SCRIPTS/03_BCGovR/envair/R/temp_valid"
    list.files<-data.filedetails$FILENAME
  }
  #end debug

  #make sure ftp.path has a "/" at the end
  if (!substr(ftp.path,nchar(ftp.path),nchar(ftp.path))=='/')
  {
    ftp.path=paste(ftp.path,'/',sep='')
  }

  files.result<-NULL
  #prepare the file name, only .csv files allowed, if not file extension, add .csv
  list.files.csv<-list.files[grepl('.csv',list.files,ignore.case=TRUE)]  #has .csv
  list.files.csv<-sub('.csv',"",list.files.csv)
  list.files.nocsv<-list.files[!grepl('\\.',list.files,ignore.case=TRUE)]  #no period
  list.files<-c(list.files.csv,list.files.nocsv)
  list.files<-paste(toupper(list.files),'.csv',sep='')
  #specifiy the location where temp files will be saved
  if (is.null(path.local))
  {
    path.local<-paste(getwd(),'/temp',sep='')
  }

  #delete temp folder if clean==TRUE
  if (clean)
  {
    print(paste('Deleting temp path:',path.local))
    #try(unlink(path.local,recursive=TRUE))
    file.remove(file.path(path.local,
                          list.files(path.local)[grepl('.csv',list.files(path.local))]))
    file.remove(file.path(path.local,
                          list.files(path.local)[grepl('.csv_',list.files(path.local))]))
  }

  #attempt 3 times to make sure directory is created
  try(dir.create(path.local,showWarnings = FALSE))
  try(dir.create(path.local,showWarnings = FALSE))
  try(dir.create(path.local,showWarnings = FALSE))
  list.write<-NULL #this is vector that contains list of files to write at end of function
  RUN_PACKAGE(c('curl','stringr','dplyr','tibble'))

  print(paste('Retrieving details of ftp:',ftp.path))
  #get details of files in FTP
  data.filedetails<-
    data.frame(FILEALL=unlist(strsplit(x=
                                         getURL(url=ftp.path,verbose=FALSE,
                                                ftp.use.epsv=TRUE
                                         ),
                                       split='\r\n')))%>%
    tidyr::separate(FILEALL,c('DATE','TIME','INDEX','FILENAME'),sep=' +',extra='drop')%>%
    dplyr::mutate(URL=paste(ftp.path,FILENAME,sep=''))%>%
    dplyr::filter(toupper(FILENAME) %in% toupper(list.files))%>%
    dplyr::mutate(CREATION_TIME=paste(DATE,TIME))%>%
    dplyr::mutate(CREATION_TIME=as.POSIXct(strptime(CREATION_TIME,"%m-%d-%y %I:%M%p"),tz='utc'))%>%
    dplyr::mutate(DATE=format(strptime(DATE,"%m-%d-%y"),'%Y-%m-%d'))%>%
    dplyr::mutate(TIME=format(strptime(TIME,"%I:%M%p"),'%H:%M'))

  data.filedetails.local<-list.files(path.local)
  data.filedetails.local<-data.filedetails.local[toupper(data.filedetails.local) %in% toupper(list.files)]

  if (length(data.filedetails.local)==0)
  {
    print(paste('There were no files in temporary folder:',path.local))
    #download all the listed files
    list.write<-c(list.write,data.filedetails$FILENAME)
  } else
  {
    #have to evaluate files first before downloading, see if download is necessary
    temp<-NULL
    print(paste('Retrieving local File details from:',nrow(data.filedetails.local),'files'))
    for (data.file in data.filedetails.local)
    {
      temp<-rbind(temp,data.frame(FILENAME=data.file,file.info(paste(path.local,data.file,sep='/'))))
    }
    data.filedetails.local<-temp%>%
      dplyr::select(FILENAME,ctime,mtime)%>%
      tibble::rownames_to_column("FULL_PATH")%>%
      dplyr::mutate(CREATION_TIME=as.POSIXct(as.character(ctime),tz='utc'))

    for (file_ in list.files)
    {
      #check if file is already saved
      temp.local<-data.filedetails.local%>%
        dplyr::filter(toupper(FILENAME)==toupper(file_))
      temp.ftp<-data.filedetails%>%
        dplyr::filter(toupper(FILENAME)==toupper(file_))

      if (nrow(temp.local)==0)
      {
        #this means there is no local copy of file,so inlcude the file
        list.write<-c(list.write,file_)
      } else
      {
        #add in write list if the file in ftp is newer version
        if (temp.ftp[1,]$CREATION_TIME>temp.local[1,]$CREATION_TIME)
        {
          list.write<-c(list.write,file_)
        }

      }
    }

  }


  #this will wrte the file listed in list.write
  list.write<-unique(list.write)
  list.write<-list.write[toupper(list.write) %in% toupper(data.filedetails$FILENAME)]
  if (!is.null(list.write) && length(list.write)>0)
  {
    print(paste('Copying:',length(list.write),'files...'))
    for (file_ in list.write)
    {

      file.temp<-data.filedetails%>%
        dplyr::filter(toupper(FILENAME)==toupper(file_))

      destfile.temp<-paste(path.local,'/',file_,'___',sep='')
      destfile<-paste(path.local,'/',file_,sep='')
      #download temporary file, read, then delete it
      download.file(file.temp$URL[1],verbose=TRUE,
                    destfile=destfile.temp,mode='wb')
      if (file.rename(from=destfile.temp,destfile))
      {
        files.result<-files.result%>%
          rbind(file.temp%>%
                  dplyr::mutate(FULL_PATH=destfile))
      }


    }

  }
  print(colnames(files.result))
  if (!is.null(files.result))
  {
    files.result<-files.result%>%
      dplyr::mutate(CREATION_TIME=paste(DATE,TIME))%>%
      RENAME_COLUMN(c('DATE','TIME','INDEX','URL'))

    if (!length(data.filedetails.local)==0)
    {

      files.result<-files.result%>%
        plyr::rbind.fill(data.filedetails.local%>%
                           RENAME_COLUMN(c('ctime','mtime'))%>%
                           dplyr::filter(!as.character(FILENAME) %in% as.character(files.result$FILENAME))
        )
    }
  } else
  {
    files.result<-data.filedetails.local%>%
      RENAME_COLUMN(c('ctime','mtime'))
  }
  # files.result<-files.result%>%
  #   RENAME_COLUMN('URL','FULL_PATH')%>%
  #   dplyr::mutate(CREATION_TIME=paste(DATE,TIME))%>%
  #                   RENAME_COLUMN(c('DATE','TIME','INDEX','FILENAME'))#%>%
  # plyr::rbind.fill(data.filedetails.local%>%
  #                    RENAME_COLUMN(c('ctime','mtime'))

  if (length(files.result)==0)
  {
    return(NULL)
  } else
  {
    return(unique(files.result))
  }
  # )

}

#' GET ECCC FORECAST function
#'
#' This function retrieves latest forecast from ECCC models
#' @param parameter is a vector string of AQHI, PM25, NO2, O3, PM10. It will include all forecase if NULL
#' @export
GET_ECCC_FORECAST_deprecated<-function(parameter=NULL)
{
  #debug
if (0)
{
  parameter<-'FORECASTSUMMARY'
}
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
    dplyr::ungroup()%>%
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
    dplyr::mutate(DATE_PST=as.POSIXct(as.character(DATE),tz='utc')-3600*8)%>% #UTC to PST conversion
    dplyr::select(DATE_PST,STATION_NAME,NAPS_ID,CGNDB,PARAMETER,MODEL,LATITUDE,LONGITUDE,DATE_CREATED,FORECAST_VALUE)%>%
    return()
}


#' INCOMPLETE: Get the CGNDB of a specified city
#'
#' This function retrieves CGNDB details
#' @param list.cities is a string vector of cities
#' @export
GET_CGNDB<-function(list.cities)
{

  #debug
  list.cities<-c('kamloops','kelowna')
  #end debug

  RUN_PACKAGE(c('dplyr','RCurl'))
  url<-'http://geogratis.gc.ca/services/geoname/en/geonames?q='
  for (cities in list.cities)
  {
    test<-RCurl::getURL(paste(url,cities,sep=''))
  }
}

#' Get Folders in URL
#'
#' This function retrieves the list of folders that are in the specified URL
#' @param source.url is the URL containing the data folders, default is ECCC datamart

GET_URL_FOLDERS<-function(source.url='http://dd.weatheroffice.ec.gc.ca/bulletins/alphanumeric/' )
{
  if (0)
  {
    source.url='https://dd.weatheroffice.ec.gc.ca/bulletins/alphanumeric/20191018/FL/CWVR/'
  }
  #retrieves list of files from the URL
  RUN_PACKAGE(c('dplyr','tidyr','httr','curl'))

  #note: Do not use the RCurl version of reading https, there is an SSL bug

  #we'll try http and https
  temp_<-curl(gsub('https://','http://',source.url))
  try(result<-data.frame(LINES=unlist(strsplit(readLines(temp_),split='/n'))))
  temp_<-curl(gsub('http://','https://',source.url))
  try(result<-data.frame(LINES=unlist(strsplit(readLines(temp_),split='/n'))))

  result<-result%>%
    dplyr::filter(grepl('alt="\\[',LINES))%>%
    dplyr::mutate(LINES=as.character(LINES))%>%
    tidyr::separate(col='LINES',into=c("LINE1","LINE2","LINE3","TYPE",
                                       "LINE5","FOLDER","LINE7"),sep='"',remove=FALSE)%>%
    tidyr::separate(col="LINE7",into=c("","DATE"),sep="  +")

  list.columns<-colnames(result)
  result<-result%>%
    RENAME_COLUMN(list.columns[!list.columns %in% c('TYPE','FOLDER','DATE')])%>%
    dplyr::filter(grepl('\\[',TYPE))
  #dplyr::filter(!is.null(TYPE))%>%
  return(result)
}




#' Get AQHI from ECCC
#'
#' Retrieves hourly AQHI from the ECCC datamart website
#' @param type is either REALTIME or FORECAST, or FORECASTSUMMARY
GET_ECCC_AQHI<-function(type='REALTIME')
{
  #debug
  #type<-'REALTIME'
  #end debug

  RUN_PACKAGE(c('dplyr','readr'))
  type<-toupper(type)
  if (type=='REALTIME')
  {
    aqhi.source<-'http://dd.weatheroffice.ec.gc.ca/air_quality/aqhi/pyr/observation/realtime/csv/'
    list.files<-GET_URL_FOLDERS(aqhi.source)%>%
      dplyr::mutate(SOURCE=paste(aqhi.source,FOLDER,sep=''))%>%
      dplyr::arrange(desc(FOLDER))
    result<-NULL
    for (i in unique(c(seq(from=1, to=nrow(list.files),by=48),nrow(list.files))))
    {
      print(paste('Extracting ECCC AQHI data',list.files$FOLDER[i]))
      data.temp<-read.csv(list.files$SOURCE[i],sep=',')%>%
        dplyr::mutate(DATE_UTC_=paste(Date,' ',Hour..UTC.,':00',sep=''))%>%
        dplyr::mutate(DATE_UTC_=as.POSIXct(DATE_UTC_,tz='utc'))%>%
        dplyr::mutate(DATE_PST=as.character(DATE_UTC_-(8*3600),format='%Y-%m-%d %H:%M'))%>%
        RENAME_COLUMN(c('Date','Hour..UTC.','DATE_UTC_'))
      list.columns<-colnames(data.temp)
      list.columns<-list.columns[!list.columns %in% c('DATE_PST')]

      for (column__ in list.columns)
      {

        result<-result%>%
          rbind(
            data.temp%>%
              RENAME_COLUMN(column__,'TEMP_COLUMN__')%>%
              dplyr::mutate(CGNDB=column__,
                            AQHI_VALUE=as.numeric(TEMP_COLUMN__))%>%
              select(DATE_PST,CGNDB,AQHI_VALUE)

          )%>%
          unique()

      }
    }
  }

  if (type=='FORECAST')
  {
    result<-GET_ECCC_FORECAST()
  }

  if (type=='FORECASTSUMMARY')
  {
    #retrieve BC and MVRD aqhi details that we want listed
    aqhi.details<-readr::read_csv('https://envistaweb.env.gov.bc.ca/aqo/csv/BC_AQHI_SITES_AQHIPlusSO2.csv')
    aqhi.source<-'http://dd.weatheroffice.ec.gc.ca/air_quality/aqhi/pyr/forecast/realtime/xml/'
    #retrieve list of files from EC datamart
    list.files<-GET_URL_FOLDERS(aqhi.source)%>%
      dplyr::mutate(ECCC_URL=paste(aqhi.source,FOLDER,sep=''))%>%
      dplyr::arrange(desc(FOLDER))%>%
      dplyr::filter(grepl('CURRENT',FOLDER))%>%
      dplyr::mutate(CGNDB=gsub('AQ_FCST_','',FOLDER,ignore.case=TRUE))%>%
      dplyr::mutate(CGNDB=gsub('_CURRENT.xml','',CGNDB,ignore.case=TRUE))%>%
      merge(aqhi.details)

    data.aqhi<-NULL
    for (i in 1:nrow(list.files))
    {
      temp_<-list.files[i,]
      print(paste('Retrieving from ECCC:',temp_$FOLDER))

      #scan for airQualityHealthIndex
      data.aqhi_<-readr::read_csv(temp_$ECCC_URL,col_names = FALSE)

      #retrieve time stamp
      data.aqhi1_<-data.aqhi_%>%
        dplyr::mutate(DATE_UTC=as.character(X1))%>%
        dplyr::filter(grepl('<UTCStamp>',DATE_UTC,ignore.case=TRUE))%>%
        dplyr::mutate(DATE_UTC=gsub('<UTCStamp>','',DATE_UTC))%>%
        dplyr::mutate(DATE_UTC=gsub('</UTCStamp>','',DATE_UTC))
      #retrieve only the latest value
      data.aqhi1_<-  data.aqhi1_[1,]%>%
      dplyr::mutate(DATE_ISSUED_PST=as.POSIXlt((DATE_UTC),tz='utc',
                                                 tryFormats=c('%Y%m%d%H00','%Y%m%d%H0000','%Y%m%d%H%M%S'))-8*3600)
      #retrinve AQHI Forecast values
      data.aqhi_<-data.aqhi_%>%
        dplyr::mutate(AQHI=as.character(X1))%>%
        dplyr::filter(grepl('<airQualityHealthIndex>',AQHI,ignore.case=TRUE))%>%
        dplyr::mutate(AQHI=gsub('<airQualityHealthIndex>','',AQHI))%>%
        dplyr::mutate(AQHI=gsub('</airQualityHealthIndex>','',AQHI))
      data.aqhi<-data.aqhi%>%
        plyr::rbind.fill(temp_%>%
                           dplyr::mutate(DATE_ISSUED=data.aqhi1_$DATE_ISSUED_PST[1],
                                         TODAY=data.aqhi_$AQHI[1],
                                         TONIGHT=data.aqhi_$AQHI[2],
                                         TOMORROW=data.aqhi_$AQHI[3])
        )

    }

    #display only specific columns by deleting all the rest
    list.columns<-colnames(data.aqhi)

    result<-data.aqhi%>%
      select(DATE_ISSUED,CGNDB,TODAY,TONIGHT,TOMORROW)%>%
      unique()%>%
      #add char values for 10+
      dplyr::mutate(TODAY=as.numeric(TODAY),
                    TONIGHT=as.numeric(TONIGHT),
                    TOMORROW=as.numeric(TOMORROW)
      )



    #check if there is no TOMORROW value, that means night time forecast where today==NULL
    #have to fix this later on to get actual TODAY forecast
    if (all(is.na(as.numeric(result$TOMORROW))))
    {
      result$TOMORROW<-result$TONIGHT
      result$TONIGHT<-result$TODAY
      print("TODAY forecast missing")
    }

    #adding CHAR columns
    result<-result%>%
      dplyr::mutate(TODAY_CHAR=ifelse(round2(TODAY)>10,'11+',
                                      ifelse(is.na(TODAY),'',
                                             as.character(TODAY)))
      )%>%
      dplyr::mutate(TONIGHT_CHAR=ifelse(round2(TONIGHT)>10,'11+',
                                        ifelse(is.na(TONIGHT),'',
                                               as.character(TONIGHT)))
      )%>%
      dplyr::mutate(TOMORROW_CHAR=ifelse(round2(TOMORROW)>10,'11+',
                                         ifelse(is.na(TOMORROW),'',
                                                as.character(TOMORROW)))
      )
  }
  return(unique(result))

}
