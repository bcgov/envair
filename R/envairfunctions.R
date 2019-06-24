
#' RENAME_COLUMN function
#'
#' This function renames or deletes the column of a dataframe based on its name
#' @param data.station dataframe input
#' @param colname.orig string vector containing the column to be deleted or renamed
#' @param colname.new string vector containing the new name for the column. if NULL, it deletes the specified column
#' @keywords rename dataframe
#' RENAME_COLUMN()
RENAME_COLUMN<-function(data.station,colname.orig,colname.new=NULL)

{

  if (is.null(colname.new)==TRUE)
  {
    #delete the column

    result<-data.station
    for (columns in colname.orig)
    {
      print(paste("Deleting column",columns))
      data.column.number<-which(colnames(result)==as.character(columns)) #column number
      result[data.column.number]<-NULL

    }
  } else
  {
    #rename the column
    result<-data.station
    counter<-1 #start a counter

    for (columns in colname.orig)
    {
      print(paste("Renaming column",columns))
      data.column.number<-which(colnames(result)==as.character(columns)) #column number


      colnames(result)[data.column.number]<-colname.new[counter]
      counter<-counter+1

    }
  }
  return(result)
}

#' rounding off function
#' #
#' This function makes a simple rounding off
#' @param x the as.numeric input
#' @param n the number of decimal places for resulting output
#' round2()
round2 = function(x,n)
{
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


#' GET STATION DETAILS FUNCTION
#'
#' This function retrieves latest station details or deteails during specific year from the ftp feed
#' @param data.year the year where station details are retrieved from. Defaults to current year
#' GET_STATION_DETAILS_FTP()
GET_STATION_DETAILS_FTP<-function(data.year=as.character(format(Sys.Date(),'%Y')))
{
  #2019-05-13
  #retrieves the station details from the BC ftp link
  #if useLATEST, it will retrieve from the web BC station details
  #contains CGNDB information

  #temp files are saved here
  RUN_PACKAGE(c('dplyr','RCurl'))
  dir.temp<-paste(getwd(),'/TEMP',sep="")
  file.temp<-paste(dir.temp,'/stationdetails.csv',sep="")
  dir.create(dir.temp,showWarnings = FALSE)

  if (data.year==as.character(format(Sys.Date(),'%Y'))) {
    useLATEST<-TRUE
  } else
  {
    useLATEST<-FALSE
  }

  if (useLATEST)
  {

    ftp.station<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Air_Monitoring_Stations/bc_air_monitoring_stations.csv'
  } else
  {
    ftp.station<-paste("ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/",
                       data.year,"/bc_air_monitoring_stations.csv",sep="")
  }

  print('Retrieving station details from FTP...')
  dir.temp<-paste(getwd(),'/TEMP',sep="")
  file.temp<-paste(dir.temp,'/stationdetails.csv',sep="")
  #temp<-RCurl::getURL(ftp.station,ftp.use.epsv=FALSE,header=TRUE)

  download.file(ftp.station,destfile=file.temp,quiet=TRUE)
  station.details<-read.csv(file.temp)%>%
    dplyr::mutate(STATUS=ifelse(STATUS==1,'ACTIVE','INACTIVE'))

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
        rbind(temp)
    }
  }

  #Create detailed list of AQHI stations with CGNDB
  station.details.aqhi<-station.details%>%
    dplyr::filter(SERIAL_CODE %in% station.details.aqhi.CGNDB$SERIAL_CODE)%>%
    merge(station.details.aqhi.CGNDB,all.y=TRUE)

  station.details<-station.details%>%
    dplyr::filter(!SERIAL_CODE %in% station.details.aqhi.CGNDB$SERIAL_CODE)%>%
    dplyr::mutate(CGNDB="N/A")%>%
    rbind(station.details.aqhi)

  #add station_name_full is not there yet
  if (!any('STATION_NAME_FULL' %in% colnames(station.details)))
  {
    #this makes column called STATION_NAME that has no suffix
    #and also creates a column called STATION_NAME_FULL
    station.details<-station.details%>%
      dplyr::mutate(STATION_NAME_FULL=STATION_NAME)%>%
      dplyr::mutate(STATION_NAME=gsub(' Met_60','',STATION_NAME_FULL,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_60','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub(' Met_15','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_15','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub(' Met_1','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_1','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_OLD','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub(' Met','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_Met','',STATION_NAME,ignore.case=TRUE))%>%
      dplyr::mutate(STATION_NAME=gsub('_Amb','',STATION_NAME,ignore.case=TRUE))%>%
      COLUMN_REORDER(columns=c("SERIAL_CODE","EMS_ID","STATION_NAME",'STATION_NAME_FULL'))



  }

  station.details <- data.frame(lapply(station.details, as.character), stringsAsFactors=FALSE)
  file.remove(file.temp)   #delete the temporary file
  return(station.details)

}


#' GET RECENT STATION DATA FUNCTION
#'
#' This function retrieves the recent 1 month of data from the ftp feed
#' @param STATION vector string of the air quality monitoring station, note this excludes
#' the underscore naming system, e.g., Prince George Plaza 400 NOT Prince George Plaza 400 Met_60
#' GET_RECENT_DATA_STATION_FTP()
GET_RECENT_DATA_STATION_FTP<-function(STATION='ALL')

{
  #debug option
  # STATION<-'smithers muheim memorial'

  RUN_PACKAGE(c('dplyr','RCurl'))
  #identify the latest validation cycle data
  file.save<-paste(getwd(),'temp',sep='/')
  dir.create(file.save,showWarnings = FALSE)
  data.ftpsource<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Station/'
  station.details.all<-GET_STATION_DETAILS_FTP()
  station.list<-station.details.all%>%
    dplyr::select(STATION_NAME)%>%
    unique()
  if (!STATION=='ALL')
  {
    station.list<-station.list%>%
      dplyr::filter(tolower(STATION_NAME)==tolower(STATION))
  }

  data.output<-NULL


  for (STATION in station.list$STATION_NAME)
  {
    print(paste('Retriving data from:',STATION))
    station.details<-station.details.all%>%
      dplyr::filter(tolower(STATION_NAME)==tolower(STATION))%>%
      dplyr::mutate(FILENAME=paste(EMS_ID,'.csv',sep=''))%>%
      dplyr::select(FILENAME)%>%
      unique()

    if (nrow(station.details)>1)
    {
      print ('There are more than one EMS ID for this station')
    }
    station.details<-station.details[1] #only acquires the first entry
    data.filedetails<-data.frame(FILEALL=unlist(strsplit(x=
                                                           getURL(data.ftpsource,verbose=FALSE,
                                                                  ftp.use.epsv=TRUE
                                                           ),
                                                         split='\r\n')))%>%
      tidyr::separate(FILEALL,c('DATE','TIME','INDEX','FILENAME'),sep=' +',extra='drop')%>%
      dplyr::filter(!grepl('AQHI-',FILENAME))%>%
      dplyr::filter(as.Date(Sys.Date()) - as.Date(DATE,tryFormats='%m-%d-%y')< 10)%>%
      dplyr::mutate(URL=paste(data.ftpsource,FILENAME,sep=''))%>%
      dplyr::filter(FILENAME %in% station.details$FILENAME)

    for (i  in 1:nrow(data.filedetails))
    {
      file.temp<-data.filedetails[i,]

      destfile<-paste(file.save,file.temp$FILENAME,sep='/')
      #download temporary file, read, then delete it
      if (nrow(file.temp)>0 && !is.na(file.temp$URL))
      {
        download.file(file.temp$URL,verbose=FALSE,
                      destfile=destfile)

        data.output.temp<-read.table(destfile,sep=',',header=TRUE,comment.char='')
        file.remove(destfile)
        if (!is.null(data.output))
        {
          data.output<-merge(data.output,data.output.temp,all=TRUE)
        } else
        {data.output<-data.output.temp}

      }
    }

  }

  data.output <- data.frame(lapply(data.output, as.character), stringsAsFactors=FALSE)
  return(data.output)
}


#' Install and Load specific libraries that are useful for the envair package
#'
#' This function isntalls and loads packages if needed
#' @param packages vector string listing the packages
#' @param lib.pack path of library, defaults to current lib paths
#' RUN_PACKAGE()
RUN_PACKAGE<-function(packages=c('dplyr','ggplot2','reshape',
                                 'lazyeval','zoo','DataCombine','data.table',
                                 'fasttime','readr','RCurl','tidyr','lubridate'),
                      lib.path=NULL)

{
  #debug

  # packages<-c('dplyr','ggplot2','RODBC','reshape',
  # 'lazyeval','zoo','DataCombine','data.table',
  # 'fasttime','readr','RCurl','tidyr','lubridate',
  # 'raster','rgdal','shapefiles','xml2')

  #end of debug lines
  if (is.null(lib.path)){
    lib.path<-.libPaths()[1]
  }



  .libPaths(lib.path)
  package_to_load<-packages


  # packages[!packages %in% loadedNamespaces()]
  package_to_install<-packages[(!packages %in% installed.packages(lib.loc = lib.path))]


  for (package in package_to_install)
  {
    print(paste('Installing package:',package))
    install.packages(package,dependencies = TRUE,lib=lib.path,repos="https://cran.r-project.org/")

  }


  for (package in package_to_load)
  {

    require(package,character.only=TRUE,lib.loc=lib.path)
  }

  return(TRUE)
}



#' GET VALID PARAMETER DATA FUNCTION
#'
#' This function retrieves parameter data from the open data portal by saving in temporary folder.
#' The temporary folder is in  the working directory ~/TEMP
#' file is only downloaded if MD5 checksum of file is different than specified in FTP
#' @param data.parameter the air quality parameter. can be PM25, PM10, SO2, TRS, H2S, etc.
#' @param year.start the beginning of query, year only
#' @param year.end the end of query, year only. Latest valid year if unspecified
#' GET_VALID_DATA_PARAMETER()
GET_VALID_DATA_PARAMETER<-function(data.parameter,
                                   year.start,
                                   year.end=NULL)

{
  #debug
  # data.parameter<-'co'
  # year.start<-2019
  # year.end=NULL

  #end debug lines

  #load packages
  RUN_PACKAGE(c('dplyr','RCurl'))  #,'feather'
  if (is.null(year.end))
  {
    year.end<-year.start
  }

  #primary data location
  data.source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/'
  data.unvalidated_source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/'

  #this is where temporary files will be saved
  path.data.temp<-paste(getwd(),'/temp/',sep='')
  dir.create(path.data.temp,showWarnings = FALSE)
  #identify the latest validation cycle data
  temp<-as.character(unlist(strsplit(getURL(data.source,dirlistonly=TRUE),split='\r\n')))
  temp<-temp[nchar(temp)==4] #get only 4-digit folders
  validation.lastvalidationcycle<-max(as.numeric(temp))


  #file.data.temp<-paste(path.data.temp,data.parameter,'.feather',sep='')
  #file.remove(file.data.temp)

  data.result<-NULL
  #scan one year at a time, create a combined feather file
  for (data.year in year.start:year.end)
  {

    #get the file source

    if (data.year<=validation.lastvalidationcycle)
    {
      data.url<-paste(data.source,data.year,"/",data.parameter,".csv",sep="")
    } else
    {
      data.url<-paste(data.unvalidated_source,data.parameter,".csv",sep="")
    }
    print(paste('Retrieving data from:',data.url))

    #get address to retrieve and write the md5
    md5.file.source<-gsub('.csv','.md5',paste(data.url),ignore.case=TRUE)
    if (RCurl::url.exists(md5.file.source) & file.exists(paste(path.data.temp,data.parameter,'_',data.year,'.csv',sep='')))
    {
      md5.source<-readLines(md5.file.source)
      md5.target<-as.character(tools::md5sum(paste(path.data.temp,data.parameter,'_',data.year,'.csv',sep='')))
      if (!identical(md5.source,md5.target))
      {
        #this means file has to be downloaded
        print(paste('Downloading into',path.data.temp,'...'))
        file.remove(paste(path.data.temp,data.parameter,'_',data.year,'.csv',sep=''))
        download.file(url=data.url,
                      destfile=paste(path.data.temp,
                                     data.parameter,'_',data.year,'.csv',sep=''),
                      quiet = TRUE)

      }
    } else
    {
      md5.source<-NULL

      print(paste('Downloading into',path.data.temp,'...'))
      download.file(url=data.url,
                    destfile=paste(path.data.temp,
                                   data.parameter,'_',data.year,'.csv',sep=''),
                    quiet = TRUE)
      md5.target<-as.character(tools::md5sum(paste(path.data.temp,data.parameter,'_',data.year,'.csv',sep='')))

    }
    #get target md5 value, whether downloaded recently or not
    #check if new md5 has to be updated in source
    md5.target<-as.character(tools::md5sum(paste(path.data.temp,data.parameter,'_',data.year,'.csv',sep='')))
    if (!identical(md5.source,md5.target))
    {
      #save new md5 only if user has access rights
      md5.file.source<-gsub('ftp://ftp.env.gov.bc.ca/','//ftpenv.nrs.bcgov/ftpenv/',
                            md5.file.source,ignore.case=TRUE)
      try(writeLines(md5.target,md5.file.source),silent=TRUE)
    }

    data.temp<-read.table(paste(path.data.temp,data.parameter,'_',data.year,'.csv',sep=''),
                          header=TRUE,sep=',')%>%
      dplyr::mutate(VALIDATION_STATUS=ifelse(data.year<= validation.lastvalidationcycle,
                                             'VALID','UNVERIFIED'))
    #pad mising data
    temp.date.start<-min(as.POSIXct(data.temp$DATE_PST,tz='utc'))
    temp.date.end<-max(as.POSIXct(data.temp$DATE_PST,tz='utc'))

    date.padding<-data.temp%>%
      RENAME_COLUMN(c('DATE_PST','DATE','TIME','RAW_VALUE','ROUNDED_VALUE','UNIT'))%>%
      unique()%>%
      merge(
        data.frame(DATE_PST=seq(from=temp.date.start,to=temp.date.end,by='hours'))
      )
    data.temp<-data.temp%>%
      dplyr::mutate(DATE_PST=as.POSIXct(DATE_PST,tz='utc'))%>%
      merge(date.padding,all.y=TRUE)%>%
      dplyr::mutate(DATE_TEMP=as.POSIXct(DATE_PST,tz='utc')-3600)%>%
      dplyr::mutate(DATE=as.character(DATE_TEMP,format='%Y-%m-%d'))%>%
      dplyr::mutate(TIME=as.character(DATE_PST,format='%H:00'))%>%
      dplyr::mutate(TIME=ifelse(TIME=='00:00','24:00',TIME))%>%
      RENAME_COLUMN('DATE_TEMP')  #remove temporary column
    #dplyr::select(DATE_PST,DATE_TEMP,DATE,TIME)

    data.result<-data.result%>%
      plyr::rbind.fill(data.temp)%>%
      dplyr::arrange(STATION_NAME,DATE_PST)%>%
      COLUMN_REORDER(columns=c('DATE_PST','DATE','TIME'))

    data.result <- data.frame(lapply(data.result, as.character), stringsAsFactors=FALSE)
    #file.remove(paste(path.data.temp,data.parameter,'_',data.year,'.csv',sep=''))

  }


  return(data.result)
}

#' REORDER COLUMNS
#'
#' This function reorders the columns of a dataframe based on the order it is specified in columns
#' @param data.input the input data frame
#' @param columns the vector of strings listing the column names in desired order. Columns not listed are added at the end
COLUMN_REORDER<-function(data.input,columns=c(''))
{
  #reorders column based on the defined vlaue in columns
  #note that unlisted columns will be added as is, in the end
  columns<-columns[columns %in% colnames(data.input)]   #removes those columns not in data
  column.nonsort<-colnames(data.input)[!colnames(data.input) %in% columns]
  data.input%>%
    dplyr::select(c(columns,column.nonsort))%>%
    return()
}

IsDate <- function(mydate) {
  tryCatch(!is.na(as.Date(mydate,tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y",
                                                "%d-%B-%Y","%d/%B/%Y","%A %B %d, %Y"))),
           error = function(err) {FALSE})
}
#Returns all items in a list that ARE contained in toMatch
#toMatch can be a single item or a list of items
include <- function (theList, toMatch){
  matches <- unique (grep(paste(toMatch,collapse="|"),
                          theList, value=TRUE))
  return(matches)
}
readKML <- function(file,keep_name_description=FALSE,layer,...) {
  # Set keep_name_description = TRUE to keep "Name" and "Description" columns
  #   in the resulting SpatialPolygonsDataFrame. Only works when there is
  #   ExtendedData in the kml file.

  sp_obj<-readOGR(file,layer,...)
  xml1<-read_xml(file)
  if (!missing(layer)) {
    different_layers <- xml_find_all(xml1, ".//d1:Folder")
    layer_names <- different_layers %>%
      xml_find_first(".//d1:name") %>%
      xml_contents() %>%
      xml_text()

    selected_layer <- layer_names==layer
    if (!any(selected_layer)) stop("Layer does not exist.")
    xml2 <- different_layers[selected_layer]
  } else {
    xml2 <- xml1
  }

  # extract name and type of variables

  variable_names1 <-
    xml_find_first(xml2, ".//d1:ExtendedData") %>%
    xml_children()

  while(variable_names1 %>%
        xml_attr("name") %>%
        is.na() %>%
        any()&variable_names1 %>%
        xml_children() %>%
        length>0) variable_names1 <- variable_names1 %>%
    xml_children()

  variable_names <- variable_names1 %>%
    xml_attr("name") %>%
    unique()

  # return sp_obj if no ExtendedData is present
  if (is.null(variable_names)) return(sp_obj)

  data1 <- xml_find_all(xml2, ".//d1:ExtendedData") %>%
    xml_children()

  while(data1 %>%
        xml_children() %>%
        length>0) data1 <- data1 %>%
    xml_children()

  data <- data1 %>%
    xml_text() %>%
    matrix(.,ncol=length(variable_names),byrow = TRUE) %>%
    as.data.frame()

  colnames(data) <- variable_names

  if (keep_name_description) {
    sp_obj@data <- data
  } else {
    try(sp_obj@data <- cbind(sp_obj@data,data),silent=TRUE)
  }
  sp_obj
}

