
#' GET_FILE_CSV
#'
#' This function retrieves csv file from ftp, save local copy. It only retrieves updated files
#' @param list.files is a vector string containing filesnames with extension
#' @param ftp.path is the ftp path where data will be saved
#' @param path.local is the local directory where the file will be saved. Default is NULL where temp folder is in the workpath
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
                                         RCurl::getURL(url=ftp.path,verbose=FALSE,
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
                    destfile=destfile.temp)
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

#' RENAME_COLUMN function
#'
#' This function renames or deletes the column of a dataframe based on its name
#' this was created to prevent any error when the column name does not exist
#' It is, at the same time, able to delete column
#' @param data.station dataframe input
#' @param colname.orig string vector containing the column to be deleted or renamed
#' @param colname.new string vector containing the new name for the column. if NULL, it deletes the specified column
#' @keywords rename dataframe
#' RENAME_COLUMN()
RENAME_COLUMN_untidy<-function(data.station,colname.orig,colname.new=NULL,quiet=TRUE)

{



  if (is.null(colname.new)==TRUE)
  {
    #delete the column

    result<-data.station
    for (columns in colname.orig)
    {
      if (!quiet){print(paste("Deleting column",columns))}
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
      if (!quiet){print(paste("Renaming column",columns,"to",colname.new[counter]))}
      data.column.number<-which(colnames(result)==as.character(columns)) #column number


      colnames(result)[data.column.number]<-colname.new[counter]
      counter<-counter+1

    }
  }
  return(result)
}

#' rounding off function
#'
#' This function makes a simple rounding off based on ISO17025
#'
#' @param x the as.numeric input
#' @param n the number of decimal places for resulting output
#' round2()
#'
round2 = function(x,n=0)
{
  x=as.numeric(x)
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
#'
#' @param data.year the year where station details are retrieved from. Defaults to current year
#' GET_STATION_DETAILS_FTP()
#'
GET_STATION_DETAILS_FTP<-function(data.year=as.character(format(Sys.Date(),'%Y')))
{
  #2019-05-13
  #retrieves the station details from the BC ftp link
  #if useLATEST, it will retrieve from the web BC station details
  #contains CGNDB information

  #temp files are saved here
  RUN_PACKAGE(c('dplyr','RCurl','readr'))
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
#  dir.temp<-paste(getwd(),'/TEMP',sep="")
 # file.temp<-paste(dir.temp,'/stationdetails.csv',sep="")
  #temp<-RCurl::getURL(ftp.station,ftp.use.epsv=FALSE,header=TRUE)

  # download.file(ftp.station,destfile=file.temp,quiet=FALSE)
  station.details<-readr::read_csv(ftp.station)%>%
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
  # file.remove(file.temp)   #delete the temporary file
  return(station.details)

}





#' Install and Load specific libraries that are useful for the envair package
#'
#' DEPRECATED
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

  #commented all these out, deprecated

  if (0)
  {

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
  }
  return(TRUE)
}


#' REORDER COLUMNS
#'
#' This function reorders the columns of a dataframe based on the order it is specified in columns
#' @param data.input the input data frame
#' @param columns the vector of strings listing the column names in desired order. Columns not listed are added at the end
#' @param align specifies where the specified columns are inserted at
#' beginning ("left") or end ("right")
COLUMN_REORDER<-function(data.input,columns=c(''),align = 'left')
{
  #reorders column based on the defined vlaue in columns
  #note that unlisted columns will be added as is, in the end
  columns<-columns[columns %in% colnames(data.input)]   #removes those columns not in data
  column.nonsort<-colnames(data.input)[!colnames(data.input) %in% columns]

  if (tolower(align) == 'right') {
    #align at right (insert at end)
    data.input%>%
      dplyr::select(c(column.nonsort,columns))%>%
      return()
  } else {
    #align left (insert at beginning)
    data.input%>%
      dplyr::select(c(columns,column.nonsort))%>%
      return()
  }
}

IsDate <- function(mydate) {
  tryCatch(!is.na(as.Date(mydate,tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y",
                                                "%d-%B-%Y","%d/%B/%Y","%A %B %d, %Y","%Y%m%d"))),
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


#' Rounds the numbers based on DAS numeric format
round3_old<-function(x,num_format=5.2)
{
  #debug
  #x<-c(1.23236556,4,'43qw4',435.57622)
  #end debug

  num_format=as.character(format(as.numeric(num_format),nsmall=1))
  x<-as.numeric(x)


  y<-NULL
  for (i in x)
  {

    if (!is.na(i))
    {
      y<-c(y,round2(i,n=as.numeric(unlist(strsplit(num_format,'\\.'))[2])))
    } else
    {
      y<-c(y,-999)
    }
  }

  return(y)
}

#' GET FTP DETAILS
#'
#' grabs the file and directory details of an ftp
#' @param path.ftp the ftp path
GET_FTP_DETAILS<-function(path.ftp)
{
  data.filedetails<-
    data.frame(FILEALL=unlist(strsplit(x=
                                         RCurl::getURL(url=path.ftp,verbose=FALSE,
                                                ftp.use.epsv=TRUE
                                         ),
                                       split='\r\n')))%>%
    tidyr::separate(FILEALL,c('DATE','TIME','INDEX','FILENAME'),sep=' +',extra='drop')%>%
    dplyr::mutate(URL=paste(path.ftp,FILENAME,sep=''))%>%
    dplyr::mutate(CREATION_TIME=paste(DATE,TIME))%>%
    dplyr::mutate(CREATION_TIME=as.POSIXct(strptime(CREATION_TIME,"%m-%d-%y %I:%M%p"),tz='etc/gmt+8'))%>%
    dplyr::mutate(DATE=format(strptime(DATE,"%m-%d-%y"),'%Y-%m-%d'))%>%
    dplyr::mutate(TIME=format(strptime(TIME,"%I:%M%p"),'%H:%M'))
}




#' GET PARAMETER DATA FUNCTION
#'
#' This function retrieves parameter data from the FTP open data portal
#' Data includes verified and unverified data
#'
#' Updated: 2020-02-13 with updates for reading the data, parsing fix
#'
#' @param data.parameter the air quality parameter. can be PM25, PM10, SO2, TRS, H2S, etc.
#' @param year.start the beginning of query, year only
#' @param year.end the end of query, year only. the same year if unspecified
#' GET_VALID_DATA_PARAMETER()
#'
GET_PARAMETER_DATA<-function(data.parameter,
                             year.start,
                             year.end=NULL)

{
  #debug

  if (0)
  {
    data.parameter <- 'co'
    year.start <- 2020
    year.end=NULL
  }

  RUN_PACKAGE(c('dplyr','RCurl','readr','lubridate'))  #,'feather'
  if (is.null(year.end))
  {
    year.end<-year.start
  }

  #primary data location
  data.source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/'
  data.unvalidated_source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/'


  #identify the latest validation cycle data
  temp<-as.character(unlist(strsplit(RCurl::getURL(data.source,dirlistonly=TRUE),split='\r\n')))
  temp<-temp[nchar(temp)==4] #get only 4-digit folders
  validation.lastvalidationcycle<-max(as.numeric(temp),na.rm = TRUE)


  #file.data.temp<-paste(path.data.temp,data.parameter,'.feather',sep='')
  #file.remove(file.data.temp)
  path.temp <- tempdir()
  data.result<-NULL
  #scan one year at a time, create a combined feather file
  for (data.year in year.start:year.end)
  {

    #get the file source

    if (data.year<= validation.lastvalidationcycle)
    {
      data.path<-paste(data.source,data.year,'/',sep='')
      path.data.temp<-paste(path.temp,'/temp_valid/',data.year,sep='')
      dir.create(path.data.temp,showWarnings = FALSE)
    } else
    {
      data.path<-data.unvalidated_source
      path.data.temp<-paste(path.temp,'/temp_valid_recent',sep='')
      dir.create(path.data.temp,showWarnings = FALSE)
    }
    print(paste('Retrieving data from:',data.path))

    # list.data<-GET_FILE_CSV(list.files=paste(data.parameter,"csv",sep='.'),
    #                         ftp.path =data.path,path.local =path.data.temp )

    list.data <- read_csv(paste(data.path,data.parameter,".csv",sep=''))

    if (length(list.data)>0)
    {
      list.data <- list.data %>%
        dplyr::mutate(VALIDATION_STATUS = ifelse(data.year <= validation.lastvalidationcycle,
                                                 'VALID','UNVERIFIED'))

      #pad mising data
      temp.date.start<-min(as.POSIXct(as.character(list.data$DATE_PST),tz='etc/GMT+8'),na.rm = TRUE)
      temp.date.end<-max(as.POSIXct(as.character(list.data$DATE_PST),tz='etc/GMT+8'),na.rm = TRUE)

      date.padding<-list.data%>%
        RENAME_COLUMN(c('DATE_PST','DATE','TIME','RAW_VALUE','ROUNDED_VALUE','UNIT'))%>%
        unique()%>%
        merge(
          data.frame(DATE_PST=seq(from=temp.date.start,to=temp.date.end,by='hours'))
        )


      list.data<-list.data%>%
        dplyr::mutate(DATE_PST=as.POSIXct(as.character(DATE_PST),tz='etc/GMT+8'))%>%
        merge(date.padding,all.y=TRUE)%>%
        dplyr::mutate(DATE_TEMP=as.POSIXct(as.character(DATE_PST),tz='etc/GMT+8')-3600)%>%
        dplyr::mutate(DATE=as.character(DATE_TEMP,format='%Y-%m-%d'))%>%
        dplyr::mutate(TIME=as.character(DATE_PST,format='%H:00'))%>%
        dplyr::mutate(TIME=ifelse(TIME=='00:00','24:00',TIME))%>%
        dplyr::select(-`DATE_TEMP`)  #remove temporary column

      #dplyr::select(DATE_PST,DATE_TEMP,DATE,TIME)

      data.result<-data.result%>%
        plyr::rbind.fill(list.data)%>%
        dplyr::arrange(STATION_NAME,DATE_PST)%>%
        COLUMN_REORDER(columns=c('DATE_PST','DATE','TIME'))
    }

  }


  #filter to the specified year
  data.result <-  data.result%>%
    dplyr::mutate(YEAR = year(as.Date(DATE))) %>%
    dplyr::filter(YEAR %in% year.start:year.end) %>%
    dplyr::select(-YEAR)


  if (length(data.result>0))
  {
    data.result <- data.frame(lapply(data.result, as.character), stringsAsFactors=FALSE)
  } else
  {
    data.result<-NULL
  }
  #file.remove(paste(path.data.temp,data.parameter,'_',data.year,'.csv',sep=''))

  return(data.result)
}


#' GET MINUTE DATA by PARAMETER/STATION FUNCTION
#'
#' This function retrieves the minute data for a specific parameter. Contains only the recent 1-2 months
#
#' @param data.request parameter or station name for the data that will be retrieved.
#' Function automatically identify if you entered station name or parameter
#'
GET_RECENT_MINUTE_DATA<-function(data.request)

{
  #debug
  if (0)
  {
    data.request<-c('prince george plaza 400','kAmloops Federal Building')
    data.request<-c('pm25','so2','test')
  }

  #end debug lines

  #load packages
  RUN_PACKAGE(c('dplyr','RCurl','readr'))

  #primary data location
  data.station.details<-GET_STATION_DETAILS_FTP()
  data.source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Minute_Raw_Air_Data/Parameter/'
  data.source.details<-GET_FTP_DETAILS(data.source)%>%
    dplyr::mutate(PARAMETER=gsub('.csv','',FILENAME,ignore.case=TRUE))

  #determine if paramer or station
  if (any(toupper(data.request) %in% toupper(data.source.details$PARAMETER)))
  {

    print('Locating parameter minute data...')
    data.source.details<-data.source.details%>%
      dplyr::filter(toupper(PARAMETER) %in% toupper(data.request))
    #path.data.temp<-paste(getwd(),'/temp_minute_parameter/',sep='')
  }  else
  {

    print('Locating station minute data...')
    data.source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Minute_Raw_Air_Data/Station/'
    data.source.details<-GET_FTP_DETAILS(data.source)%>%
      dplyr::mutate(EMS_ID=gsub('.csv','',FILENAME))%>%
      merge(
        data.station.details%>%
          dplyr::select(STATION_NAME,EMS_ID)
      )%>%
      dplyr::filter(toupper(STATION_NAME) %in% toupper(data.request))%>%
      unique()
    #path.data.temp<-paste(getwd(),'/temp_minute_station/',sep='')
  }
  #this is where temporary files will be saved

  #save data


  data.result<-NULL
  #scan one file at a time
  for (filename in data.source.details$URL)
  {
    print(paste('Reading the content of file:',filename))
    data.result<-data.result%>%
      plyr::rbind.fill(readr::read_csv(filename))
  }

  data.result<-data.result%>%
    COLUMN_REORDER(c('DATE_PST','DATE','TIME','STATION_NAME','STATION_NAME_FULL'))
  return(data.result)
}


#' GET RECENT HOURLY DATA by PARAMETER/STATION FUNCTION
#'
#' This function retrieves the hourly  data for a specific parameter. Contains only the recent 1-2 months
#
#' @param data.request vector of either parameters or station names for the data that will be retrieved.
#' Function automatically identify if you entered station name or parameter
#'
GET_RECENT_HOURLY_DATA<-function(data.request)

{
  #debug
  if (0)
  {
    data.request<-c('prince george plaza 400','kAmloops Federal Building')
    data.request<-c('pm25','so2','test')
  }
  #end debug lines

  #load packages
  RUN_PACKAGE(c('dplyr','RCurl','readr'))  #,'feather'


  #primary data location
  data.station.details<-GET_STATION_DETAILS_FTP()
  data.source.air<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Air_Quality/'
  data.source.met<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Meteorological/'
  data.source.details<-GET_FTP_DETAILS(data.source.air)%>%
    dplyr::mutate(FTP=data.source.air)%>%
    rbind(GET_FTP_DETAILS(data.source.met)%>%
            dplyr::mutate(FTP=data.source.met)
    )%>%
    dplyr::mutate(PARAMETER=gsub('.csv','',FILENAME,ignore.case=TRUE))

  #special consideration for h2s, trs request because file might be combined
  if (any(toupper(data.request) %in% c('H2S','TRS')))
  {
    if (any(toupper(data.source.details$PARAMETER) %in% 'H2STRS'))
    {
      data.request<-'H2STRS'
    }
  }
  #determine if paramer or station
  if (any(toupper(data.request) %in% toupper(data.source.details$PARAMETER)))
  {

    print('Locating parameter Hour data...')
    data.source.details<-data.source.details%>%
      dplyr::filter(toupper(PARAMETER) %in% toupper(data.request))
    # path.data.temp<-paste(getwd(),'/temp_hour_parameter/',sep='')
  }  else
  {

    print('Locating station Hour data...')
    data.source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Station/'
    data.source.details<-GET_FTP_DETAILS(data.source)%>%
      dplyr::mutate(EMS_ID=gsub('.csv','',FILENAME))%>%
      merge(
        data.station.details%>%
          dplyr::select(STATION_NAME,EMS_ID)
      )%>%
      dplyr::filter(toupper(STATION_NAME) %in% toupper(data.request))%>%
      dplyr::mutate(FTP=data.source)%>%
      unique()
    #path.data.temp<-paste(getwd(),'/temp_hour_station/',sep='')
  }

  # list.files<-NULL
  # for (data.source in unique(data.source.details$FTP))
  # {
  #   list.files<-list.files%>%
  #     rbind(GET_FILE_CSV(data.source.details$FILENAME[data.source.details$FTP==data.source],
  #                        path.local = path.data.temp,data.source)
  #     )
  # }
  data.result<-NULL
  #scan one file at a time
  for (filename in data.source.details$URL)
  {
    print(paste('Reading the content of file:',filename))
    data.result<-data.result%>%
      plyr::rbind.fill(readr::read_csv(filename))
  }
  data.result<-data.result%>%
    COLUMN_REORDER(c('DATE_PST','DATE','TIME','STATION_NAME','STATION_NAME_FULL','STATION'))%>%
    RENAME_COLUMN('STATION','STATION_NAME')
  return(data.result)
}


#' GET RECENT STATION DATA FUNCTION
#'
#' This function retrieves the recent 1 month  of data from the ftp feed
#' @param STATION vector string of the air quality monitoring station, note this excludes
#' @param timebase default to 60, is either 1 or 60. If 1, function is same as GET_MINUTE_DATA_FTP
#' the underscore naming system, e.g., Prince George Plaza 400 NOT Prince George Plaza 400 Met_60
#' GET_RECENT_DATA_STATION_FTP()
#'
GET_RECENT_STATION_DATA<-function(STATION='ALL',timebase=60)

{


  if (0)
  {
    STATION<-c('Prince George Plaza 400','Vanderhoof Courthouse')
    timebase=1
  }
  RUN_PACKAGE(c('dplyr','RCurl','readr','plyr'))


  if (timebase==1)
  {
    data.ftpsource<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Minute_Raw_Air_Data/Station/'
  } else
  {
    data.ftpsource<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Station/'
  }

  station.details.all<-GET_STATION_DETAILS_FTP()
  if (all(!(STATION=='ALL')))
  {
    station.list<-station.details.all%>%
      dplyr::select(STATION_NAME)%>%
      dplyr::filter(tolower(STATION_NAME) %in% tolower(STATION))%>%
      unique()
  } else
  {
    station.list<-station.details.all%>%
      dplyr::select(STATION_NAME)%>%
      unique()
  }


  station.details<-station.details.all%>%
    dplyr::filter(toupper(STATION_NAME) %in% toupper(station.list$STATION_NAME))%>%
    dplyr::mutate(FILENAME=paste(EMS_ID,'.csv',sep=''))%>%
    dplyr::select(FILENAME)%>%
    unique()

  #retrieve data from ftp
  files_<-data.frame(FILENAME=as.character(unlist(strsplit(RCurl::getURL(
    data.ftpsource, dirlistonly=TRUE),split='\r\n'))),
    ATTRIBUTES=as.character(unlist(strsplit(RCurl::getURL(
      data.ftpsource, dirlistonly=FALSE),split='\r\n')))
  )%>%
    dplyr::mutate(FULL_PATH=paste(data.ftpsource,FILENAME,sep=''))%>%
    separate(ATTRIBUTES,into=c('DATE','EXTRA_ATTRIB'),sep='  ',extra='merge')%>%
    dplyr::mutate(DATE=as.POSIXct(DATE,format='%m-%d-%y'))%>%
    dplyr::mutate(AGE=as.Date(Sys.Date())-as.Date(DATE))

  list.files<-files_$FULL_PATH[toupper(files_$FILENAME) %in% toupper(station.details$FILENAME)]



  data.output<-NULL
  for (file_ in list.files)
  {

    print(paste('Retrieving data from:',file_))
    data.output <- data.output%>%
      plyr::rbind.fill(readr::read_csv(file_))

  }
  data.output <- data.output%>%
    COLUMN_REORDER(c('STATION','DATE_PST','DATE','DATE_LOCAL','LATITUDE','LONGITUDE'))
  return(data.output)
}


#' Get Rolling Mean function
#'
#' This function creates a rolling mean based on specified hours
#' NOTE: use GET_RUNNING_AVG() for tidy version
#'
#' @param data.input is dataframe containing data
#' @param data.input.previous is dataframe of earlier data, added to have complete rolling mean
#' @param data.averaginghour is the rolling mean time
#' @param column.value is vector that will be averaged
#' @param column.date is string defining the date time column
#' @param column.static is vector sting containing all columns where rolling average is calculated
#' @param threshold is the data capture requirement in fraction. If the valid sample
#' for average is less, it will not have a rolling mean value
#' @param precision is the number of decimal places for the result
GET_ROLLING_MEAN<-function(data.input,data.input.previous=NULL,
                           data.averaginghour=8,column.date='DATE_PST',
                           column.value='ROUNDED_VALUE',
                           column.static=c('STATION_NAME','STATION_NAME_FULL','SERIAL','EMS_ID','INSTRUMENT'),
                           threshold=0.75,precision=1)
  #gets the rolling mean
  #should include previous year's value here



{
  # debug
  # data.input<-station.data
  # # #
  # # data.input.previous<-NULL
  # data.averaginghour=3
  # column.date='DATE_PST'
  # # column.static=c('STATION_NAME','STATION_NAME_FULL','SERIAL','EMS_ID','INSTRUMENT')
  # # column.value=c('NO2_ROUNDED','O3_ROUNDED','pm25_rounded')
  # threshold=0.75
  # precision=1
  # data.input.previous<-GET_DATA_FTP(2017,'o3')
  # data.averaginghour=8
  # column.date='DATE_PST'
  # column.name='ROUNDED_VALUE'
  # precision=1
  # threshold=0.75
  # end debug


  print('getting rolling mean')

  #prepare column.value convert to readable format
  column.value<-colnames(data.input)[toupper(colnames(data.input)) %in% toupper(column.value)]
  if (length(column.value)>0)
  {
    data.output<-data.input%>%
      plyr::rbind.fill(data.input.previous)%>% #combine with the previous year's date, if available
      #dplyr::filter(STATION_NAME=='Prince George Plaza 400')%>%   #for debugging purposes
      GET_DATEPADDED_DATA(column.datefield = column.date,
                          column.static = column.static,keep_REF_ = TRUE)%>%#pad missing dates, to be sure
      RENAME_COLUMN(column.date,'TEMP_DATE_PST')

    for (column.name in column.value)
    {
      print(paste('Processing data column:',column.name))
      data.output<-data.output%>%
        RENAME_COLUMN(column.name,'TEMP_COLUMN_VALUE')%>%
        dplyr::mutate(TEMP_COLUMN_VALUE=as.numeric(TEMP_COLUMN_VALUE))%>%
        dplyr::mutate(TEMP_VALID_COUNTER=ifelse(is.na(TEMP_COLUMN_VALUE),0,1))%>% #just adding a counter
        dplyr::mutate(TEMP_COLUMN_VALUE2=ifelse(is.na(TEMP_COLUMN_VALUE),0,as.numeric(TEMP_COLUMN_VALUE)))%>%
        dplyr::mutate(TEMP_COLUMN_VALUE2=ifelse(is.na(TEMP_COLUMN_VALUE2),0,as.numeric(TEMP_COLUMN_VALUE2)))%>%  #secondary check
        dplyr::arrange(REF_KEY,TEMP_DATE_PST)%>%   #makes sure it is sorted
        dplyr::group_by(REF_KEY)%>%
        #introduces rolling sums, value just accumulates, and calculations from this

        dplyr::mutate(TEMP_ROLL=cumsum(TEMP_VALID_COUNTER),TEMP_ROLL_VALUE=cumsum(TEMP_COLUMN_VALUE2))%>%
        dplyr::mutate(TEMP_PREV_ROLL=dplyr::lag(TEMP_ROLL,n=data.averaginghour),
                      TEMP_PREV_VALUE=dplyr::lag(TEMP_ROLL_VALUE,n=data.averaginghour))%>%
        #make sure there are no NA in rolling sums
        dplyr::mutate(TEMP_ROLL_VALUE=ifelse(is.na(TEMP_ROLL_VALUE),0,TEMP_ROLL_VALUE),
                      TEMP_PREV_VALUE=ifelse(is.na(TEMP_PREV_VALUE),0,TEMP_PREV_VALUE),
                      TEMP_ROLL=ifelse(is.na(TEMP_ROLL),0,TEMP_ROLL),
                      TEMP_PREV_ROLL=ifelse(is.na(TEMP_PREV_ROLL),0,TEMP_PREV_ROLL))%>%
        dplyr::mutate(TEMP_ROLL_AVE=round2((TEMP_ROLL_VALUE-TEMP_PREV_VALUE)/(TEMP_ROLL-TEMP_PREV_ROLL),precision),
                      TEMP_VALID_COUNT=(TEMP_ROLL-TEMP_PREV_ROLL))%>%

        dplyr::mutate(TEMP_ROLL_AVE=ifelse(TEMP_VALID_COUNT>=
                                             as.integer(threshold*data.averaginghour),
                                           as.character(TEMP_ROLL_AVE),''))%>%
        #removes average that does not have enough number of samples
        RENAME_COLUMN('TEMP_ROLL_AVE',paste(column.name,'_',data.averaginghour,'HR',sep=''))%>% #rename to match the original column name
        RENAME_COLUMN('TEMP_COLUMN_VALUE',column.name)%>%  #naming the column back
        RENAME_COLUMN(c('TEMP_VALID_COUNTER','TEMP_ROLL','TEMP_ROLL_VALUE','TEMP_PREV_ROLL',
                        'TEMP_PREV_VALUE','TEMP_VALID_COUNT','TEMP_COLUMN_VALUE2'))%>% #remove the temporary and unused column
        dplyr::ungroup()

      #reorder the dataframe columns, to insert the rolling average right after
      data.colnames<-colnames(data.output)
      data.colnames.value<-match(column.name,data.colnames)
      data.colnames.left<-c(data.colnames[1:data.colnames.value],
                            paste(column.name,'_',data.averaginghour,'HR',sep=''))
      data.output<-COLUMN_REORDER(data.output,data.colnames.left)
    }
    data.output<-data.output%>%
      dplyr::mutate(TEMP_DATE_PST2=as.POSIXct(as.character(TEMP_DATE_PST),tz='utc'))%>%
      dplyr::filter(TEMP_DATE_PST2>=as.POSIXct(as.character(min(data.input$DATE_PST,na.rm = TRUE)),tz='utc'))%>%   #show only the needed dates
      dplyr::filter(TEMP_DATE_PST2<=as.POSIXct(as.character(max(data.input$DATE_PST,na.rm = TRUE)),tz='utc'))%>%
      RENAME_COLUMN('TEMP_DATE_PST2')%>% #remove temporary date column
      RENAME_COLUMN('TEMP_DATE_PST',column.date)%>%
      RENAME_COLUMN(c('REF_KEY','TEMP_COLUMN_NAME','TEMP_DATE_FIELD'))
    return(data.output)
  } else
  {return(data.input)}
}


#' Pad missing dates function
#'
#' This function identifies and inserts missing dats
#' @param data.unpadded is dataframe of data
#' @param column.datefield is a string defining the date and time
#' @param column.static is a vector listing the column names that data is grouped by
#' @param timebase is either 60 or 1
#' @param keep_REF_ is internal option to keep a reference or index column
GET_DATEPADDED_DATA<-function(data.unpadded,column.datefield='DATE_PST',
                              column.static=c('STATION_NAME','STATION_NAME_FULL','SERIAL','EMS_ID','INSTRUMENT'),
                              timebase=60,keep_REF_=FALSE)

{
  #This will pad the data with missing dates
  #column.static is a vector of strings that describes the column names whose value will be retained throughout the padded entries
  #it will serve as the reference key to create the padded dates

  #----debug purposes------------
  # #these are function inputs
  # data.unpadded<-station.data
  # # column.static<- c('STATION_NAME','EMS_ID','AQHI_AREA')
  # column.datefield<-'DATE_PST'
  # column.static=c('STATION_NAME','STATION_NAME_FULL','SERIAL','EMS_ID','INSTRUMENT')
  #data.unpadded<-data
  #-------------------------



  #convert all fields that are not part of date or static fields
  column.allnames<-colnames(data.unpadded)
  column.static<-column.static[column.static %in% column.allnames]
  print('Converting data to string')
  for (temp in column.allnames[!column.allnames %in% c(column.static,column.datefield)])
  {
    data.unpadded<-data.unpadded%>%
      RENAME_COLUMN(temp,'TEMP_COLUMN_')%>%
      dplyr::mutate(TEMP_COLUMN_NAME=as.character(TEMP_COLUMN_)) %>%
      RENAME_COLUMN('TEMP_COLUMN_',temp)
  }
  #-------------------------------------------------------------

  data.unpadded<-data.unpadded%>%
    RENAME_COLUMN(column.datefield,'TEMP_DATE_FIELD_00')%>%   #temporarily rename the column
    dplyr::mutate(TEMP_DATE_FIELD=as.POSIXct(TEMP_DATE_FIELD_00,tz='utc'))%>%
    dplyr::mutate(TEMP_DATE_FIELD_00=as.character(format(TEMP_DATE_FIELD,'%Y-%m-%d %H:%M')))%>%   #this is to ensure format of date is standard
    RENAME_COLUMN('TEMP_DATE_FIELD_00',column.datefield)%>%
    dplyr::mutate(REF_KEY='KEY')  #rename the column back


  #create a key column based on multiple columns defined in column.key
  #column key is just concatenation of other columns
  for (column.temp in column.static)
  {
    print(paste('Creating a key from:',column.temp,'column'))
    data.unpadded<-data.unpadded%>%
      RENAME_COLUMN(column.temp,'TEMP_REF_')%>%
      dplyr::mutate(REF_KEY=paste(REF_KEY,TEMP_REF_,sep='-'))%>%
      RENAME_COLUMN('TEMP_REF_',column.temp)  #rename back
  }


  #identify the start date and end dates
  data.date<-data.unpadded%>%
    dplyr::group_by(REF_KEY)%>%
    dplyr::summarise(DATE_START=min(TEMP_DATE_FIELD,na.rm=TRUE),
                     DATE_END=max(TEMP_DATE_FIELD,na.rm = TRUE))


  for (key in unique(data.unpadded$REF_KEY))
  {
    print(paste('Padding dates for the following:',key))
    temp.data<-data.unpadded%>%
      dplyr::filter(REF_KEY==key)

    temp.date<-data.date%>%
      dplyr::filter(REF_KEY==key)
    if (as.numeric(timebase)==1)
    {
      temp.filler<-data.frame(TEMP_DATE_FIELD=seq.POSIXt(from=temp.date$DATE_START,to=temp.date$DATE_END[1],by='min'))%>%
        dplyr::mutate(TEMP_DATE_FIELD_00=as.character(format(TEMP_DATE_FIELD,'%Y-%m-%d %H:%M')))%>%   #insert the original column
        RENAME_COLUMN('TEMP_DATE_FIELD_00',column.datefield)
    } else
    {
      temp.filler<-data.frame(TEMP_DATE_FIELD=seq.POSIXt(from=temp.date$DATE_START,to=temp.date$DATE_END[1],by='hour'))%>%
        dplyr::mutate(TEMP_DATE_FIELD_00=as.character(format(TEMP_DATE_FIELD,'%Y-%m-%d %H:%M')))%>%   #insert the original column
        RENAME_COLUMN('TEMP_DATE_FIELD_00',column.datefield)
    }

    #insert columns with static values
    #add the reference key as a static field
    column.static<-unique(c(column.static,'REF_KEY')) #this will make sure the value gets carried over
    for (column.temp in column.static)
    {
      temp.data<-temp.data%>% #grab the static values for specific columns
        RENAME_COLUMN(column.temp,'TEMP_COLUMN_')

      temp.filler<-temp.filler%>%
        dplyr::mutate(TEMP_COLUMN_=temp.data$TEMP_COLUMN_[1])%>%
        RENAME_COLUMN('TEMP_COLUMN_',column.temp)

      temp.data<-temp.data%>%
        RENAME_COLUMN('TEMP_COLUMN_')   #remove this column once processed
    }
    #insert other columns, leave these blank
    column.allnames<-colnames(data.unpadded)
    column.allnames<-column.allnames[!column.allnames %in% c(column.static,column.datefield,'REF_KEY','TEMP_DATE_FIELD')]

    for (column.temp in column.allnames)
    {
      if (!column.temp %in% colnames(temp.filler))
      {
        #this adds columns that do not exist in temp.filler
        print(paste('Insert a column with blank value:',column.temp))
        temp.filler<-temp.filler%>%
          dplyr::mutate(TEMP_COLUMN_='')%>%
          RENAME_COLUMN('TEMP_COLUMN_',column.temp)
      }
    }



    temp.filler<-temp.filler %>%
      dplyr::filter(!TEMP_DATE_FIELD %in% temp.data$TEMP_DATE_FIELD)
    data.unpadded<-rbind(data.unpadded,temp.filler)
    print(paste('Inserted',nrow(temp.filler),'missing dates'))
  }

  if (keep_REF_)
  {
    data.result<-RENAME_COLUMN(data.unpadded,c('TEMP_COLUMN_NAME','TEMP_DATE_FIELD'))

  } else
  {
    data.result<-RENAME_COLUMN(data.unpadded,c('REF_KEY','TEMP_COLUMN_NAME','TEMP_DATE_FIELD'))  #deletes these two columns

  }
  return(data.result)
}


#' Get Folders in URL
#'
#' This function retrieves the list of folders that are in the specified URL
#' @param source.url is the URL containing the data folders, default is ECCC datamart
GET_URL_FOLDERS<-function(source.url='https://dd.weather.gc.ca/bulletins/alphanumeric/' )
{
  if (0)
  {
    source.url='https://dd.weather.gc.ca/bulletins/alphanumeric/20191107/FL/CWVR'
    source.url = 'https://dd.weather.gc.ca/bulletins/alphanumeric/20191108/FL/CWVR/14/'
    source.url <- 'http://dd.weather.gc.ca/air_quality/aqhi/pyr/observation/realtime/csv/'
  }
  #retrieves list of files from the URL
  RUN_PACKAGE(c('dplyr','tidyr','httr','curl'))

  #note: Do not use the RCurl version of reading https, there is an SSL
  httr::set_config(config( ssl_verifypeer = 0L ) )

  result <- NULL

  for (j in 1:20)
  {
    print(paste('Attempt',j,'in GETURLFolder for',source.url))
    #we'll try http and https
    source.url <- gsub('https://','http://',source.url)
    temp_<-curl::curl(source.url)
    try(result<-unlist(strsplit(readLines(temp_),split='/n')),silent = TRUE)

    source.url <- gsub('http://','https://',source.url)
    temp_<-curl::curl(source.url)
    try(result<-unlist(strsplit(readLines(temp_),split='/n')),silent = TRUE)

    if (!is.null(result)) {break}

  }

  if (!is.null(result))
  {
    result<-data.frame(LINES = result)

    result<-result%>%
      dplyr::filter(grepl('alt="\\[',LINES))%>%
      dplyr::mutate(LINES=as.character(LINES))%>%
      tidyr::separate(col='LINES',into=c("LINE1","LINE2","LINE3","TYPE",
                                         "LINE5","FOLDER","LINE7"),sep='"',remove=FALSE)%>%
      tidyr::separate(col="LINE7",into=c("LINE8","DATE"),sep="  +")

    list.columns<-colnames(result)
    result<-result%>%
      RENAME_COLUMN(list.columns[!list.columns %in% c('TYPE','FOLDER','DATE')])%>%
      dplyr::filter(grepl('\\[',TYPE))
    #dplyr::filter(!is.null(TYPE))%>%
  }

#print(paste('File retrieval complete:',source.url))
  return(result)
}




#TIDY Functions Beyond here-------


#' RENAME_COLUMN function
#'
#' This function renames or deletes the column of a dataframe based on its name
#' this was created to prevent any error when the column name does not exist
#' It is, at the same time, able to delete column
#' @param data.station dataframe input
#' @param colname.orig string vector containing the column to be deleted or renamed
#' @param colname.new string vector containing the new name for the column. if NULL, it deletes the specified column
#' @keywords rename dataframe
#' RENAME_COLUMN()
RENAME_COLUMN<-function(data.station,colname.orig,colname.new=NULL,quiet=TRUE)

{



  if (is.null(colname.new)==TRUE)
  {
    #delete the column

    result<-data.station
    for (columns in colname.orig)
    {
      if (!quiet){print(paste("Deleting column",columns))}
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
      if (!quiet){print(paste("Renaming column",columns,"to",colname.new[counter]))}
      data.column.number<-which(colnames(result)==as.character(columns)) #column number


      colnames(result)[data.column.number]<-colname.new[counter]
      counter<-counter+1

    }
  }
  return(result)
}


#' Rounds the numbers based on DAS numeric format
round3<-function(x,num_format)
{
  #debug\
  if (0)
  {
  x<-c(1.23236556,4,'43qw4',435.57622)
  num_format <- c(5.2,5.1,5.4,5)
  }
  #end debug
  #print(paste(length(x),'-',length(num_format)))
  num_format=as.character(format(as.numeric(num_format),nsmall=1))

  df_x <- tibble(num = as.numeric(x), format = as.character(num_format))%>%
    tidyr::separate(col=`format`,into=c("whole","decimal"),sep='\\.')%>%
    dplyr::mutate(decimal = ifelse(is.na(decimal),0,as.numeric(decimal)))%>%
    dplyr::mutate(rounded = round2(num,decimal))%>%
    dplyr::mutate(rounded = ifelse(is.na(rounded),-9999,rounded))


  return(df_x$rounded)
}


#' Get the compass needle value of an azimuth measurement
#'
#' The result is OVER or UNDER if it is not inside 0-360 deg value
#' Note: must be efficient if this function revised to use Mod and div instead
#' @param angle is a vector of numbers, corresponds to azimuth readings
direction <- function(angle)
{
  if (0)
  {
    angle <- c(50,45,36,360,500,-155)
  }

  #all invalid angles are changed to OVER or UNDER Error
  print(paste('Direction check',length(angle),'values'))
  df_result <- tibble(`angle` = angle, result= 'WAITING',id = 1:length(angle))
  df_result$result[df_result$angle>360] <- 'OVER'
  df_result$result[df_result$angle<0] <- 'UNDER'


  #function to convert value to DEG string
  df_direction <- tribble(
    ~Direction,~Angle,
    'N',0,
    'NE',45,
    'E',90,
    'SE',135,
    'S',180,
    'SW',225,
    'W',270,
    'NW',315
  )
  increment <- 360/nrow(df_direction) #increment per needle point
  df_direction <- df_direction %>%
    mutate(startangle = Angle - (increment/2),
           endangle = Angle + (increment)/2) %>%
    #adjust for  values exceeding 360 or under zero
    mutate(startangle = ifelse(startangle <0,360 + startangle,startangle),
           endangle = ifelse(endangle > 360, endangle -360,endangle)) %>%
    select(-Angle)


  #create input for data
  df_data <- df_result%>%
    dplyr::filter(result == 'WAITING') %>%
    merge(df_direction) %>%
    #check how far the angle is from the pre-determinet values
    dplyr::mutate(startDev = angle - startangle,
                  endDev = endangle - angle)%>%
    #fix negatives, excess values
    dplyr::mutate(startDev = ifelse(startDev <0, startDev+360, startDev),
                  endDev= ifelse(endDev <0, endDev+360, endDev))%>%
    #get the minumum startDev from each
    dplyr::group_by(id)%>%
    dplyr::mutate(minStartDev = min(startDev, na.rm = TRUE)) %>%
    ungroup()%>%
    dplyr::filter(startDev == minStartDev)%>%
    dplyr::mutate(result=Direction) %>%
    dplyr::select(angle,result,id)

  #combine to result
  df_result <- df_result %>%
    dplyr::filter(result != 'WAITING')%>%
    plyr::rbind.fill(df_data) %>%
    dplyr::arrange(id)
  return(df_result$result)
}

#' Fix tooltip in rmarkdown kable
#'
#' This fixes tooltip issue by creating new lines
#' by inserting spaces
#'
#' @param txt is a string containing the tooltip
#'             lines are separated by sep_string
#' @param sep_string is the string separator for the lines
#'            default is br
FIX_tooltip <- function(txt,sep_string='<br>')
{
  if (0)
  {
    txt <- df_alerts$tooltips1
    sep_string <- '<br>'

  }
  library(dplyr)
  library(stringr)

  #print('Fix_tooltip running...')
  df_txt <- tibble(Text = txt) %>%
    dplyr::mutate(num_breaks = str_count(sep_string),
                  index=1:n())


  total_breaks <- max(df_txt$num_breaks,na.rm=TRUE)

  #escape if there are no breaks
  if (total_breaks==0)
  {
    return(txt)
  }

  #separate one at a time
  lst_ <- NULL
  for (i in 1:(total_breaks+1))
  {
    lst_ <- c(lst_,paste('txt_',i,sep=''))
  }

  df_txt %>%
    tidyr::separate(Text,sep=sep_string,into=lst_,extra = 'merge',remove = TRUE)%>%
    pivot_longer(cols=lst_,names_to='line',values_to = 'content')%>%
    dplyr::filter(!is.na(content),
           content!='')%>%
    dplyr::mutate(char_length = nchar(content))%>%
    group_by(index)%>%
    dplyr::mutate(max_char = max(char_length,na.rm = TRUE))%>%
    ungroup()%>%
    mutate(content=str_pad(content,max_char+2,side='right'))%>%
    group_by(index) %>%
    arrange(line)%>%
    dplyr::summarise(Text = paste(content,collapse='\r\n'))%>%
    pull(Text)

}


#' Calculate the running average
#'
#' deprecated, use importBC_data_avg()
#' Gets the running average of specific parameters
#' requires a pivot_longer format
#'
#' @param airdata is the dataframe containing the air quality
#'                data. Requires columns listing the parameter and the value
#' @param parameter is vector contaling parameters listed in the parameter column
#'                where the running average is calculated
#'                if NULL, it will include each parameters in averaging
#' @param date_column is the column contains the date-time is ymd_hm format
#' @param parameter_column is the column listing all the paremetsr
#' @param value_column is the column containing the data value
#' @param averaging_time is the time to perform running average
#' @param grouping_column is the column where result will be grouped by
#' @param threshold_percent is the percent of data that are valid, otherwise
#'                          the averaging result is NA
GET_RUNNING_AVG <- function(airdata,parameter=NULL,
                            date_column = 'DATE_PST',
                            parameter_column='name',
                            value_column='value',
                            averaging_time = 24,
                            grouping_column = 'STATION_NAME',
                            threshold_percent=75)
{

  #debug----
  if (0)
  {
    airdata <- df_raw_all
    parameter <- 'pm25'
    date_column = 'DATE_PST'
    parameter_column='name'
    value_column='value'
    averaging_time = 24
    grouping_column = 'STATION_NAME'
    threshold_percent=75
  }

  #setup-----
  RUN_PACKAGE(c('dplyr','lubridate','plyr'))

  #rename columns
  df_airdata <- airdata%>%
    RENAME_COLUMN(c(parameter_column,
                    date_column,
                    value_column,
                    grouping_column),
                  c('parameter_column',
                    'date_column',
                    'value_column',
                    'grouping_column')) %>%
    mutate(`date_column` = ymd_hm(`date_column`))

  df_airdata_orig <- df_airdata

  if (is.null(parameter))
  {
    #add all parameters if parameter is nULL
    parameter <- unique(df_airdata$parameter_column)
  }

  if (!any(tolower(parameter) %in% tolower(unique(df_airdata$parameter_column))))
  {
    #that means the parameter is not on the data
    return(airdata)
  }
  #identify the min and max dates
  #fill with the missing dates and time
  min_date <- min(df_airdata$date_column,na.rm=TRUE)
  max_date <- max(df_airdata$date_column,na.rm=TRUE)

  df_fill <- df_airdata%>%
    dplyr::select(`parameter_column`,`grouping_column`) %>%
    unique()%>%
    merge(tibble(`date_column` =
                   seq.POSIXt(min_date,max_date,by='hour')))

  df_airdata <- df_airdata %>%
    left_join(df_fill) %>%
    arrange(`date_column`)

  #retrieve the past n_hours of value-----
  df_airdata_ <- NULL
  for (i in 1:averaging_time)
  {
    if (0)
    {
      i=1
    }
    df_ <- df_airdata %>%
      group_by(`grouping_column`,`parameter_column`) %>%
      dplyr::mutate(prev_value = lag(`value_column`,n=i-1)) %>%
      dplyr::mutate(n_ = ifelse(is.na(as.numeric(prev_value)),0,1),
                    lag = i)

    df_airdata_ <- df_airdata_ %>%
      plyr::rbind.fill(df_)

  }

  #calculate the averages, process output-----
  df_airdata <- df_airdata_ %>%
    group_by(`grouping_column`,`parameter_column`,`date_column`,`value_column`) %>%
    dplyr::summarise(total_value = sum(as.numeric(`prev_value`),na.rm = TRUE),
                     total_n = sum(n_,na.rm=TRUE)) %>%
    ungroup() %>%
    dplyr::filter(tolower(`parameter_column`) %in% parameter)%>%
    mutate(ave_value = total_value/total_n,
           valid_perc = (total_n/averaging_time)*100) %>%
    mutate(ave_value = ifelse(valid_perc>=threshold_percent,
                              ave_value,
                              ''),
           `parameter_column` = paste(`parameter_column`,averaging_time,sep='_'))%>%
    dplyr::select(`grouping_column`,`parameter_column`,`date_column`,ave_value) %>%
    dplyr::rename(`value_column` = ave_value)

  #combine the result with original input----
  result <- df_airdata_orig %>%
    plyr::rbind.fill(df_airdata) %>%
    mutate(`date_column` = as.character(format(`date_column`,'%Y-%m-%d %H:%M'))) %>%
    arrange(`grouping_column`,`parameter_column`,`date_column`) %>%
    RENAME_COLUMN(c("date_column","parameter_column","grouping_column","value_column"),
                  c(date_column,parameter_column,grouping_column,value_column))

  return(result)
}


#' Determine if date is DST in BC or not
#'
#' @param datetime is the date and time
isit_DST <- function(datetime = Sys.time())
{
  if (0)
  {
    datetime <- Sys.time()
    datetime <- '2020-01-01 01:00'
  }

  str_time_pst <- as.character(ymd_hm(datetime,tz='etc/gmt+8'),'%Y-%m-%d %H:%M')
  str_time_local <- as.character(format(ymd_hm(str_time_pst,tz='etc/gmt+8'),
                                        tz='America/Vancouver','%Y-%m-%d %H:%M'))
  time_diff <- difftime(ymd_hm(str_time_local),ymd_hm(str_time_pst),units ='hours')
  if (as.integer(time_diff) ==1)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

#' Forward fills data with orevious value
#'
#' This addresses the issue where a station has multiple dataloggers
#' and some dataloggers have not updated recent data
#' Works only for 1-station entry, no STATION_NAME_FULL
#'
#' @param data_all is the dataframe containing all data in original wide format
#' @param threshold is the number of hours of maximum delay
FILL_Recent_Data <- function(data_all,threshold=2)
{

  #package load----
  RUN_PACKAGE(c('dplyr','tidyr','lubridate'))

  #debug-----
  if (0)
  {
    data_all <- GET_DATA_STATION_DAS('Prince George Plaza 400',
                                     startdate = '2020-05-01',enddate = '2020-05-20',
                                     webviewonly = TRUE)
    threshold <- 2
  }

  #setup-----
  #quit if more than one station name
  if (length(unique(data_all$STATION_NAME))>1)
  {
    print('ERROR: More than one station on the list. Please send one')
    print('Stations found:',unique(data_all$STATION_NAME))
    return(data_all)
  }


  #define the parameter suffixed
  suffix <- c('UNITS','ROUNDED','UNIT','INSTRUMENT')


  print(paste('Processing',nrow(data_all),'rows of data for forward-filling'))
  #get column names, retrieve list of parameters----
  cols_ <- colnames(data_all)
  maincols <- c('STATION_NAME','DATE_PST')
  cols_sensor <- cols_[grepl('_ROUNDED$',cols_)]
  cols_sensor <- gsub('_ROUNDED$','',cols_sensor)

  #switch to longer format
  data_all <- data_all %>%
    arrange(ymd_hm(DATE_PST)) %>%
    dplyr::mutate(index =1:n())

  df_data <- data_all %>%
    select(c(maincols,'index',cols_sensor)) %>%
    pivot_longer(cols = -c(maincols,'index'))

  #identify the latest data
  df_latest <- df_data %>%
    dplyr::filter(!(is.na(value))) %>%
    group_by(STATION_NAME,name) %>%
    dplyr::mutate(max_index =max(index,na.rm=TRUE)) %>%
    ungroup() %>%
    dplyr::filter(index==max_index) %>%
    #do not substitute as latest value if >threshold hrs
    #substitute the DATE_PST as the latest DATE_PST
    group_by(STATION_NAME) %>%
    dplyr::mutate(dev_maxindex = max(index,na.rm=TRUE) - index) %>%
    dplyr::filter(dev_maxindex<=threshold) %>%
    ungroup()

  #split the original wide table
  df_result <- NULL
  for (dev in sort(unique(df_latest$dev_maxindex)))
  {
    df_latest_ <- df_latest[df_latest$dev_maxindex==dev,]
    #define all related parameters, this includes adding the suffix
    df_cols <- tibble(source_parameter = df_latest$name[df_latest$dev_maxindex == dev]) %>%
      merge(tibble(param_suffix = c(toupper(suffix),'source_param')))%>%
      mutate(parameter = paste(source_parameter,param_suffix,sep='_'))%>%
      mutate(parameter=gsub('_source_param','',parameter))%>%
      dplyr::filter(parameter %in% cols_)

    df_ <- data_all %>%
      select(c(maincols,df_cols$parameter,'index'))
    if (dev == 0)
    {
      df_result <- df_
    } else
    {
      print(paste('Forward filling:',dev,'rows'))
      df_filler <- df_%>%
        dplyr::filter(index==df_latest_$max_index[1]) %>%
        select(-maincols)

      df_remove<- df_%>%
        dplyr::filter(index>df_latest_$max_index[1])%>%
        select(maincols)

      df_add <- df_remove %>%
        merge(df_filler)

      df_ <- df_ %>%
        dplyr::filter(index<=df_latest_$max_index[1])%>%
        plyr::rbind.fill(df_add) %>%
        select(-index)

      df_result <- merge(df_result,df_)
    }

  }

  df_result <- df_result %>%
    COLUMN_REORDER(cols_)

  return(df_result)
}

#' Convert to Local Pacific Timezone
#'
#' @param DATE_PST is vector of PST date-time in ymd_hm format
#' @param format is the format of the output
GET_LOCAL_Time <- function(DATE_PST, format = '%Y-%m-%d %H:%M')
{
  #load package
  RUN_PACKAGE(c('dplyr','lubridate'))

  if (0)
  {
    DATE_PST <- df_data_orig_$DATE_PST
    format = '%B %Y-%m-%d %H:%M'
  }

  #setup----
  #makes sure it is character
  DATE_PST <- as.character(DATE_PST,'%Y-%m-%d %H:%M')

  result <- format(ymd_hm(DATE_PST,tz='etc/gmt+8'),tz='America/Vancouver',format = format)
  return(result)
}

#' Retrieves the excel files Containing the Statistical Summary
#' This fixes the header where in the header is made of two rows
#'
#' @param filename is the full path of the file
GET_SAS_EXCEL_FILE <- function(filename)
{
  if (0)
  {
    filename <- "A:/Air/Operations ORCS/Data/05_CAAQS/R Summaries/2019_UNSUPPRESSED/Submitted_200520/2019_NO2_UNSUPPRESSED.xlsx"
  }

  #header of the excel file, extracted separately
  df_stat_header <- read_excel(filename,n_max=2,col_names = FALSE)
  df_stat_header_trans  <- df_stat_header%>%
    data.table::transpose() %>%
    dplyr::mutate(V1 = gsub('HOURLY PERCENTILES','%(hr)',V1,ignore.case = TRUE)) %>%
    dplyr::mutate(V1 = gsub('DAILY PERCENTILES','%(day)',V1,ignore.case = TRUE)) %>%
    dplyr::mutate(V1 = gsub('^.*MONITORING MONTHS.*$','(days)',V1,ignore.case = TRUE)) %>%
    dplyr::mutate(V1 = gsub('^.*QUARTERLY DATA.*$','(%days)',V1,ignore.case = TRUE))

  #remove NA of column category values
  repeat
  {
    # df_nonNAs <- df_stat_header_trans[!is.na(df_stat_header_trans$V1),]
    # df_NAs<- df_stat_header_trans[is.na(df_stat_header_trans$V1),]

    df_stat_header_trans <- df_stat_header_trans %>%
      dplyr::mutate(V1=ifelse(is.na(V1),lag(V1),V1))

    if ((!any(is.na(df_stat_header_trans$V1)))){break}
  }

  df_stat_header_trans <- df_stat_header_trans %>%
    dplyr::mutate(columnname = ifelse(is.na(V2),
                                      V1,
                                      paste(V2,V1,sep=''))
    )

  df_stat_official <- read_excel(filename,skip=2,col_names = FALSE)


  colnames(df_stat_official) <- df_stat_header_trans$columnname
  try(
    df_stat_official <- df_stat_official %>%
      dplyr::mutate(
        `Q1(%days)` = as.integer(`Q1(%days)`),
        `Q2(%days)` = as.integer(`Q2(%days)`),
        `Q3(%days)` = as.integer(`Q3(%days)`),
        `Q4(%days)` = as.integer(`Q4(%days)`)
      ))
  return(df_stat_official)
}
