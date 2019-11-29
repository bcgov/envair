
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
#'@param use_openairformat is boolean,if TRUE, output is compatible with openair
#'#'@param use_ws_vector default is FALSE, if TRUE and use_openairformat is TRUE, ws is the vector wind speed
#'@param pad, default is FALSE. if true, it will pad missing dates. This requires greater memory
#'
#'@examples
#' importBC_data('Prince George Plaza 400')
#' importBC_data('pm25',2015:2016,use_openairformat = FALSE)
#' importBC_data(c('Prince George','Kamloops'),c(2010,2015))
#'
#' @export
importBC_data<-function(parameter_or_station,
                        years=NULL,use_openairformat=TRUE,
                        use_ws_vector = FALSE,pad = FALSE)

{
  #debug
  if (0)
  {
    parameters<-c('no','no2')
    years<-2017:2018
    parameters=NULL
    stations=NULL
    parameter_or_station<-c('Trail Columbia Gardens Airport')
  }

  #load packages
  RUN_PACKAGE(c('dplyr','RCurl','plyr','readr','lubridate','tidyr'))  #,'feather'
  if (is.null(years))
  {
    years=as.numeric(format(Sys.Date(),'%Y'))
  }



  #primary data location
  data.source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/'
  data.unvalidated_source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/'


  #identify if parameter or station based on the list of parameters that are in unvalidated source
  temp_<-as.character(unlist(strsplit(getURL(data.unvalidated_source,dirlistonly=TRUE),split='\r\n')))
  temp_<-gsub('.csv','',temp_,ignore.case=TRUE)
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
  temp_<-as.character(unlist(strsplit(getURL(data.source,dirlistonly=TRUE),split='\r\n')))
  temp_<-temp_[nchar(temp_)==4] #get only 4-digit folders
  valcycle<-max(as.numeric(temp_))

  data.result<-NULL

  if (!is.null(parameters))
  {
    #user wanted to look for parameters, not stations
    # scan one parameter at a time
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
        list.data<-paste(source_,'/',parameter,".csv",sep='')

        print(paste('Retrieving data from:',list.data))




        data.result<-data.result%>%
          plyr::rbind.fill(
            readr::read_csv(list.data)%>%
              dplyr::mutate(VALIDATION_STATUS=ifelse(data.year<= valcycle,
                                                     'VALID','UNVERIFIED'))
          )
      }

      #Pad dates, recalculate DATE and TIME
    }


  } else
  {
    #retrieve station data, not parameters
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

    if (nrow(list.stations>0))
    {
      #that means there were stations on the list
      #retrieve data from each station
      for (ems_ in unique(list.stations$EMS_ID))
      {
        #retrieve data from each year for that specified station
        for (year_ in years)
        {
          print(paste('Retrieving data from: EMS=',ems_,'Year=',year_))
          #determine the source of data
          if (year_> valcycle)
          {
            source_<-paste(data.unvalidated_source,'STATION_DATA/',ems_,'.csv',sep='')
          } else
          {
            source_<-paste(data.source,year_,'/STATION_DATA/',ems_,'.csv',sep='')
          }

          temp_<-NULL
          temp_<-try(as.character(unlist(strsplit(getURL(source_,dirlistonly=TRUE),split='\r\n'))))

          sourcefile_<-unlist(strsplit(source_,split='/'))
          sourcefile_<-sourcefile_[length(sourcefile_)]
          if (sourcefile_ %in% temp_)
          {

            print(paste('Downloading data from:',source_))
            data.result<-data.result%>%
              plyr::rbind.fill(readr::read_csv(source_))
          }
        }
      }
    }

  }


  #stop if there are no result
  if (is.null(data.result))
  {
    return(NULL)
  }

  #remove DATE,TIME columns, just utilize the DATE_PST
data.result <- data.result%>%
  RENAME_COLUMN(c('DATE','TIME'))  #remove these columns
  #covert DATE_PST to POSIXct date
  tz(data.result$DATE_PST) <- 'Etc/GMT+8'


  #check if there are duplicate entries
  #merge their
  duplicate <- data.result%>%
    dplyr::select(EMS_ID,STATION_NAME)%>%
    unique()%>%
    dplyr::group_by(STATION_NAME)%>%
    dplyr::mutate(number =n(),newems = max(EMS_ID,na.rm = TRUE))%>%
    dplyr::ungroup()%>%
    RENAME_COLUMN('EMS_ID')%>% #delete
    RENAME_COLUMN('newems','EMS_ID')%>% #rename with the latest
    unique()

  if (max(duplicate$number,na.rm = TRUE)>1)
  {
    #fix for duplicate station entries
    print('Station has duplicate entries, might take longer than usual')

    #list instrument and data columns
    cols_ <- colnames(data.result)
    cols_ <- cols_[!cols_ %in% c('STATION_NAME','DATE','TIME','EMS_ID','DATE_PST')]
    cols_instrument_ <- cols_[grepl('instrument',cols_,ignore.case = TRUE)]
    cols_ <- cols_[!cols_ %in% cols_instrument_]

    #dates only to pad removed entries
    df_dates <- data.result%>%
      dplyr::select(DATE_PST,STATION_NAME)%>%
      unique()


    #process duplicate station data, remove EMS ID, assign with a common one
    data.duplicate <- data.result%>%  #
      dplyr::filter(STATION_NAME %in% duplicate$STATION_NAME)%>%
      RENAME_COLUMN(cols_instrument_)%>%
      tidyr::pivot_longer(cols = cols_,names_to = 'PARAMETER',values_to = 'VALUES')%>%
      RENAME_COLUMN('EMS_ID')%>%
      merge(duplicate%>%
              dplyr::select(STATION_NAME,EMS_ID))%>%
      dplyr::filter(!is.na(as.numeric(VALUES)))%>%
      tidyr::pivot_wider(names_from = PARAMETER, values_from = VALUES)

    #process instrument names, combine for all entries on each station, get only one value for each instrument
    data.duplicate.instruments <- data.result%>%
      dplyr::filter(STATION_NAME %in% duplicate$STATION_NAME)%>%
      dplyr::select(STATION_NAME,cols_instrument_)%>%
      unique()%>%
      tidyr::pivot_longer(-STATION_NAME, names_to='parameter',values_to='instrument')%>%
      unique()
    #use counter to remove duplicates
    data.duplicate.instruments <- data.duplicate.instruments%>%
      dplyr::arrange(desc(instrument))%>%
      dplyr::mutate(counter = 1:nrow(data.duplicate.instruments))%>%
      dplyr::group_by(STATION_NAME,parameter)%>%
      dplyr::mutate(use_counter = min(counter,na.rm = TRUE))%>%
      dplyr::ungroup()%>%
      dplyr::filter(counter == use_counter)%>%
      dplyr::select(-counter,-use_counter)%>%
      tidyr::pivot_wider(names_from = parameter, values_from = instrument)

    #insert instrument details to duplicate data

    data.duplicate <- data.duplicate%>%
      left_join(data.duplicate.instruments)%>%
      COLUMN_REORDER(c('DATE_PST','DATE','TIME','STATION_NAME','EMS_ID',
                       sort(c(cols_,cols_instrument_))))%>%
      right_join(df_dates)

    #bind again with data.result
    data.result <- data.result%>%
      dplyr::filter(!STATION_NAME %in% duplicate$STATION_NAME)%>%
      plyr::rbind.fill(data.duplicate)


  }


if (pad)
{
  #pad data, add missing data entries as NA
  #note that this does not group station name anymore
  col_ <- colnames(data.result)
  col_instrument <- col_[grepl('instrument',col_,ignore.case = TRUE)]

  df_padding <- data.result%>%
    select(STATION_NAME,EMS_ID)%>%
    unique()%>%
    group_by(STATION_NAME)%>%
    merge(tidyr::tibble(DATE_PST= seq.POSIXt(from = as.POSIXct(min(data.result$DATE_PST)),
                                      to = as.POSIXct(max(data.result$DATE_PST)),
                                      by='hour')))

  print(paste(nrow(df_padding) - nrow(data.result),'rows padded' ))

  # print('Padding',as.character(nrow(df_padding) - nrow(data.result)),
  #       'rows')
  data.result <- data.result%>%
    right_join(df_padding)
}



  if (use_openairformat & !is.null(data.result))
  {
    #rename all columns, change them to lower case to match openair requirements
    column_<-colnames(data.result)
    if (use_ws_vector)
    {
      #ws is vector wind speed (not default)
      data.result<-data.result%>%
        RENAME_COLUMN(column_,tolower(column_))%>%
        RENAME_COLUMN('date_pst','date')%>%
        RENAME_COLUMN(c('wspd_vect','wdir_vect'),
                      c('ws','wd'))

      #use wspd_scalar as ws if not available
      #use wdir_uvec as wd if not available

      column_<-colnames(data.result)
      if (!('ws' %in% column_))
      {
        data.result<-data.result%>%
          RENAME_COLUMN('wspd_vect','ws')
      }
      if (!('wd' %in% column_))
      {
        data.result<-data.result%>%
          RENAME_COLUMN('wdir_uvec','wd')
      }

    } else
    {
      #ws is scalar wind speed #default
      data.result<-data.result%>%
        RENAME_COLUMN(column_,tolower(column_))%>%
        RENAME_COLUMN(c('date','time'))%>%
        RENAME_COLUMN('date_pst','date')%>%
        RENAME_COLUMN(c('wspd_sclr','wdir_vect'),
                      c('ws','wd'))

      #use wspd_scalar as ws if not available
      #use wdir_uvec as wd if not available

      column_<-colnames(data.result)
      if (!('ws' %in% column_))
      {
        data.result<-data.result%>%
          RENAME_COLUMN('wspd_sclr','ws')
      }
      if (!('wd' %in% column_))
      {
        data.result<-data.result%>%
          RENAME_COLUMN('wdir_uvec','wd')
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


  } else
  {
    #add DATE and TIME columns
    data.result <- data.result%>%

      #GET_DATEPADDED_DATA()%>%
      dplyr::mutate(date_=as.POSIXct(DATE_PST,tz='Etc/GMT+8')-3600)%>%
      dplyr::mutate(DATE=as.character(format(date_,'%Y-%m-%d')),
                    TIME=as.character(format(as.POSIXct(DATE_PST,tz='Etc/GMT+8'),'%H:%M')))%>%
      dplyr::mutate(TIME=ifelse(TIME=='00:00','24:00',TIME))%>%
      COLUMN_REORDER(columns=c('DATE_PST','DATE','TIME'))%>%
      dplyr::select(-`date_`)

  }


  print(paste('Done.',nrow(data.result),'rows'))
  return(data.result)
}



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

  if (is.null(year))
  {year<- as.numeric(format(Sys.Date(),'%Y'))}

  RUN_PACKAGE(c('dplyr','RCurl','readr','tibble'))

  # dir.temp<-paste(getwd(),'/TEMP',sep="")
  # file.temp<-paste(dir.temp,'/stationdetails.csv',sep="")
  # dir.create(dir.temp,showWarnings = FALSE)

  #identify the latest validation cycle
  temp_<-as.character(unlist(strsplit(getURL("ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/",
                                             dirlistonly=TRUE),split='\r\n')))
  temp_<-temp_[nchar(temp_)==4] #get only 4-digit folders
  valcycle<-max(as.numeric(temp_))


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
  station.details <- tibble::as.tibble(station.details)
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
  RUN_PACKAGE(c('RCurl','dplyr'))
  ftpsource_ <- 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/'
  temp_<-as.character(unlist(strsplit(getURL(ftpsource_,dirlistonly=TRUE),split='\r\n')))
  temp_ <- temp_[!grepl('station',temp_,ignore.case=TRUE)]
  temp_ <- tolower(gsub('.csv','',temp_,ignore.case=TRUE))
  temp_ <- sort(temp_)
return(temp_)
}
