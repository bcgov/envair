
#R scripts to Update Venting Index Bulletin


#' generate the venting index bulletin for BC env
#'
#' This function retrieves venting data from ECCC datamart
#' to generate the text bulletin
#' function here was developed to run in the DAS server (PARTICULATE.dmz)
#' which saves the resulting venting.html file into E:/WebSites/wwwroot/Web2016/aqo/files/bulletin/
#' and will show up in
#'
#' @param date.start is the date of the venting data. if NULL, it retrieves the latest available
#' @param savefile is the location where the file will be saved. If null it will default to locations
#' defined in default_html, and default_text locations based on output
#' If an ftp link is defined, envair.key with write access details to FTP server is needed
#' If savefile is terminated in '/', another folder for the data year (~./2019/EC_) will be generated and
#' the
#' @param output is either 'html' or 'ftp'. If 'html', it will create html file into envistaweb
#'              if 'ftp' then it will save into the Open Data Portal FTP page
#'              if 'csv' it creates the csv file that populates the http://www.env.gov.bc.ca/epd/bcairquality/aqo/csv/VentingIndex.csv
#'
#' ventingBC_bulletin()
ventingBC_bulletin<-function(date.start=NULL,
                             savefile=NULL,
                             output='html')
{
  #debug
  if (0)
  {
    date.start<-NULL
    savefile=NULL
    output='csv'
    source('../../envair/R/envairfunctions.R')
  }
  # date.start<-NULL
  #end debug
  RUN_PACKAGE('lubridate')
  if (is.null(date.start)) {date.start <- as.character(now(),'%Y-%m-%d')}
  #These are pre-defined based on intended output locations if running on DAS server
  #results are based on whether it is saving into ftp or file
  default_html <- 'E:/WebSites/wwwroot/Web2016/aqo/files/bulletin/venting.html'
  default_text <- 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/VentingBulletins/'
  default_csv <- 'E:/apps_data/open_data_portal/VentingIndex.csv'

  source('envairfunctions.R')

  if (is.null(savefile))
  {
    savefile<-ifelse(tolower(output)=='html',default_html,
                     ifelse(tolower(output)=='csv',default_csv,
                            default_text))
  }

  #PURPOSE: Retrieves venting data from the specified URL
  #venting.metadata='ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Air_Quality/VentingMetaData.csv'
  venting.url='https://dd.weatheroffice.ec.gc.ca/bulletins/alphanumeric/'    #ECCC venting index data
  #venting.metadata='ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Air_Quality/VentingMetaData.csv'
  venting.template='https://envistaweb.env.gov.bc.ca/aqo/files/bulletin/venting_template.html'
  RUN_PACKAGE(c('dplyr','curl'))
  if (is.null(date.start))
  {
    #get the latest venting files
    temp.list<-GET_URL_FOLDERS()%>%
      dplyr::arrange(desc(DATE))%>%
      dplyr::mutate(URL=paste(venting.url,FOLDER,'FL/CWVR/',sep=''))

  } else
  {
    #get the one that matches specified date
    date.start<-gsub('-','',date.start)
    temp.list<-GET_URL_FOLDERS()%>%
      dplyr::arrange(desc(DATE))%>%
      dplyr::mutate(DATE_TEMP=gsub('/','',FOLDER))%>%
      dplyr::filter(DATE_TEMP==date.start)%>%
      RENAME_COLUMN('DATE_TEMP')%>%
      dplyr::mutate(URL=paste(venting.url,FOLDER,'FL/CWVR/',sep=''))

  }

  venting.content<-NULL #contains the ECCC web data for venting (FLCN39)
  for (i in 1:nrow(temp.list))
  {

    # temp.download<-rbind(temp.download,
    #                      GET_URL_FOLDERS(temp.list[i,]$URL)%>%
    #                        dplyr::mutate(URL=paste(temp.list[i,]$URL,FOLDER,sep=''))%>%
    #                        dplyr::arrange(desc(DATE))
    #                      )
    if (is.null(venting.content))
    {
      temp.download <- NULL
      try(
        temp.download<-GET_URL_FOLDERS(temp.list[i,]$URL)%>%
          dplyr::mutate(URL=paste(temp.list[i,]$URL,FOLDER,sep=''))%>%
          dplyr::arrange(desc(DATE)) %>%
          dplyr::filter(grepl(date.start,URL))    #this will give error if nothing retrieved
      )


      if (!is.null(temp.download))
      {
        print(temp.download)
        for (temp.download.url in temp.download$URL)
        {
          if (is.null(venting.content))
          {
            #check the files in the url folder
            print(paste('scanning content of',temp.download.url))
            temp<-GET_URL_FOLDERS(temp.download.url)%>%
              filter(grepl('FLCN39',FOLDER,ignore.case=TRUE))
            if (nrow(temp)>0)
            {
              temp.result <- NULL
              for (j in 1:20)
              {
                print(paste('Attempt:',j,'on',temp.download.url))
                #we'll just try both http and https, whichever works
                try(temp.result<-unlist(readLines(paste(gsub('https://','http://',temp.download.url,ignore.case=TRUE)
                                                        ,temp$FOLDER,sep=''))))
                try(temp.result<-unlist(readLines(paste(gsub('http://','https://',temp.download.url,ignore.case=TRUE)
                                                        ,temp$FOLDER,sep=''))))
                if (!is.null(temp.result))
                {
                  break
                }

              }

              if (length(temp.result)>0)
              {
                print (paste('found Venting file in',temp$FOLDER))
                venting.content<-temp.result
                #scan date from path
                venting.date_ <- unlist(strsplit(temp.download.url,split='/'))
                for (j in 1:length(venting.date_))
                {
                  if (IsDate(venting.date_[j]))
                  {
                    venting.date <- as.character(as.Date(venting.date_[j],'%Y%m%d'),format='%Y-%m-%d')
                  }
                }
                if (!is.null(venting.content)){break} #exit point
              }


            }
          }
        }
      }
    }
  }

  if (venting.date == as.character(ymd(date.start),'%Y-%m-%d'))
  {

    print(paste('Found Venting of date',venting.date))
    #create a string of the venting content
    ventECCC_<-venting.content[1]
    for (i in 2:length(venting.content))
    {
      ventECCC_<-paste(ventECCC_,venting.content[i],sep='\n')
    }
    #find the date from the ECCC file
    #result is in venting.date
    # for (i in 1:length(venting.content))
    #
    # {
    #   #print(paste('Parsing venting file:',i,'of',length(venting.content)))
    #
    #   temp<-venting.content[i]
    #   #identify if there is date in this line
    #   if (IsDate(temp))
    #   {
    #     venting.date<-as.character(as.Date(temp,'%d-%B-%Y'),format='%Y-%m-%d')
    #   }
    # }

    #if output is html, it will save file directly into the defined path
    #if output is ftp, it will save in temporary folder, and then upload file into ftp
    if (tolower(output)=='ftp')
    {
      #save ventECCC_ into a temporary file
      if (nchar(ventECCC_)>100)
      {
        #create a text version, normally as archive in the ftp server
        filename_<-data.frame(FILENAME=paste('EC_BBS_',
                                             as.character(as.Date(venting.date),format='%y%m%d'),'_',
                                             as.character(as.Date(venting.date),format='%a_%B_%d_%y'),'.txt',
                                             sep=''))%>%
          dplyr::mutate(TEMP_FILE=paste(tempdir(),FILENAME,sep='/'),
                        FINAL_FILE=paste(savefile,as.character(as.Date(venting.date),format='%Y'),
                                         '/',FILENAME,sep='')
          )


        unlink(tempdir(),recursive=TRUE)
        dir.create(tempdir())
        write(ventECCC_,filename_$TEMP_FILE[1])

        print(paste('Saving to ',filename_$FINAL_FILE[1]))
        key<-ENVAIR_CONNECTION_CHECK()
        key.ftpuser<-as.character(key$VALUE[key$ITEM=='FTP_USER'])
        key.ftppwd<-as.character(key$VALUE[key$ITEM=='FTP_PASSWORD'])
        # print(paste('from:',paste(path.temp,'/',filename,sep=''),
        #             'to:',filename.final))
        try(RCurl::ftpUpload(filename_$TEMP_FILE[1],
                             filename_$FINAL_FILE[1],
                             userpwd=paste(safer::decrypt_string(key.ftpuser,key=Sys.info()['nodename']),
                                           safer::decrypt_string(key.ftppwd,key=Sys.info()['nodename'])
                                           ,sep=':'),.opts=list(ftp.create.missing.dirs=TRUE)
        ))

      }




    }
    if (tolower(output)=='html')
    {
      #converting venting.content into an html file similar to the original venting
      #retrieve template
      temp_<-curl(venting.template)
      template_<-data.frame(LINES=unlist(strsplit(readLines(temp_),split='/n')))%>%
        dplyr::mutate(LINES=as.character(LINES))

      #create html of string
      html_<-template_$LINES[1]
      for (i in 2:nrow(template_))
      {

        html_<-paste(html_,template_$LINES[i],sep='\n')
      }
      #insert date
      find_<-'<!--INSERT POINT FOR DATE-->'
      insert_<-as.character(as.Date(venting.date),format='%A %B %d, %Y')
      html_<-gsub(find_,insert_,html_,ignore.case=TRUE)
      #insert the venting bulleting
      find_<-'<!--INSERT POINT FOR VENTING-->'
      insert_<-ventECCC_
      html_<-gsub(find_,insert_,html_,ignore.case=TRUE)

      #writes to the file only if the html string is large~>100 strings
      if (nchar(html_)>100) {write(html_,savefile)}
    }
    if (tolower(output)=='csv')
    {
      vent_<-GET_VENTING_ECCC(as.character(now(),'%Y-%m-%d'))%>%
        COLUMN_REORDER(c('NAME','REGION','VENTING_INDEX_ABBREV','DATE_ISSUED','LAT','LONG'))

      if (!is.null(vent_))
      {
        data.table::fwrite(vent_,file=savefile)
      } else
      {
        print('There were no venting data retrieved from ECCC')

      }
    }
  } else
  {
    print(paste('Venting of the date not found, only found',venting.date, 'and today is', date.start))
  }
}


#' Creates kml files or shapefiles
#'
#' This function creates kml files using the new OBSCR sensitivity zones and venting regions
#' @param path.output where kml files will be saved. If null, function returns shape file
#' @param HD is boolean. If true, it will use high quality map >50 MB. This exceeds Web ARcMap requirements
#'                       If false, this will use simplified map (0.0002 degrees) of <10MB file size
#' @param isCOVID is boolean; if TRUE, it applies HSSZ fireban due to COVID 19
#' @param fireban is vector containg the sensitivity zones where a fireban is applied
#'
#'  @examples
#' ventingBC_kml(HD=FALSE)
#'
#' @export
ventingBC_kml<-function(path.output=NULL,HD=TRUE,isCOVID = TRUE,fireban=NULL)
{
  #debug
  if (0)
  {
    setwd("A:/Air/Operations ORCS/Technology/Software Codes ScriptsExecutables/R_SCRIPTS/03_BCGovR/envair/R")
    source('ventingBC_bulletin.R')
    source('envairfunctions.R')
    path.output<-'C:/TEMP/'
    HD<-FALSE
    path.temp <- 'C:/TEMP/'
    isCOVID = TRUE
    fireban = c('MEDIUM','LOW')
  }
  #end debug lines

  #setup------
  #special message for COVID19
  msg_COVID <- '*Open burning restricted for all High Smoke Sensitivity  Zones until Monday, 15th June 2020.
  No new fires may be initiated and no material added to existing fires.'

  RUN_PACKAGE(c('rlang','R6','Rcpp','tidyr','namespace','dplyr','ggplot2','RODBC','reshape',
                'lazyeval','zoo','DataCombine','data.table',
                'fasttime','readr','RCurl','tidyr','lubridate',
                'raster','rgdal','shapefiles','xml2','zip'))
  source('envairfunctions.R')
  date_today <- as.character(now(),'%Y-%m-%d')
  path.temp<-tempdir()
  unlink(path.temp)
  dir.create(path.temp,showWarnings = FALSE)
  file.temp<-paste(path.temp,'/venting.kml',sep='')
  if (HD)
  {
    URL.shapefile<-'https://envistaweb.env.gov.bc.ca/aqo/files/Venting_Blank_HD.kml'
  } else
  {
    URL.shapefile<-'https://envistaweb.env.gov.bc.ca/aqo/files/Venting_Blank.kml'
  }

  URL.inserfile<-'https://envistaweb.env.gov.bc.ca/aqo/files/Venting_format.xml'


  print(paste('Downloading blank shapefile to:',file.temp))
  download.file(URL.shapefile,file.temp)

  if (0)
  {
    file.temp<-'A:/Data Systems/IT 6000-6999/Systems development 6450/IT Projects 6450-20/Ventilation Index Map/CovidOBSCRBan/Venting_Blank_COVID.kml'
  }
  #read KML file
  file.shapefile<-readKML(file.temp,keep_name_description = FALSE)
  file.remove(file.temp)
  #read the insert file for kml file
  venting.insert<-readLines(URL.inserfile)
  venting.data <- NULL
  try(
    venting.data<-GET_VENTING_ECCC(date_today)%>%
      dplyr::mutate(TODAY_VI_NUMERIC=TODAY_VI_DESC)%>%
      dplyr::mutate(TODAY_VI_NUMERIC=gsub('good','2',TODAY_VI_NUMERIC,ignore.case=TRUE))%>%
      dplyr::mutate(TODAY_VI_NUMERIC=gsub('fair','1',TODAY_VI_NUMERIC,ignore.case=TRUE))%>%
      dplyr::mutate(TODAY_VI_NUMERIC=gsub('poor','0',TODAY_VI_NUMERIC,ignore.case=TRUE))%>%
      dplyr::mutate(TODAY_VI_NUMERIC=ifelse(TODAY_VI_NUMERIC %in% c('0','1','2'),as.numeric(TODAY_VI_NUMERIC),NA))%>%
      dplyr::mutate(TOMORROW_VI_NUMERIC=TOMORROW_VI_DESC)%>%
      dplyr::mutate(TOMORROW_VI_NUMERIC=gsub('good','2',TOMORROW_VI_NUMERIC,ignore.case=TRUE))%>%
      dplyr::mutate(TOMORROW_VI_NUMERIC=gsub('fair','1',TOMORROW_VI_NUMERIC,ignore.case=TRUE))%>%
      dplyr::mutate(TOMORROW_VI_NUMERIC=gsub('poor','0',TOMORROW_VI_NUMERIC,ignore.case=TRUE))%>%
      dplyr::mutate(TOMORROW_VI_NUMERIC=ifelse(TOMORROW_VI_NUMERIC %in% c('0','1','2'),as.numeric(TOMORROW_VI_NUMERIC),NA))%>%
      dplyr::mutate(VI_NUMERIC=as.numeric(TODAY_VI_NUMERIC)*10+as.numeric(TOMORROW_VI_NUMERIC))%>%
      #apply rules for old sensitivity zones
      #removed during the 2019 OBSCR update
      # dplyr::mutate(LOW_SENSITIVITY_BURN=ifelse(TODAY_VI_NUMERIC>0 & TOMORROW_VI_NUMERIC>0,'YES','NO'))%>%
      # dplyr::mutate(MEDIUM_SENSITIVITY_BURN=ifelse(TODAY_VI_NUMERIC>1 & TOMORROW_VI_NUMERIC>0,'YES','NO'))%>%
      # dplyr::mutate(HIGH_SENSITIVITY_BURN=ifelse(TODAY_VI_NUMERIC>1 & TOMORROW_VI_NUMERIC>0,'YES','NO'))%>%

      #
      RENAME_COLUMN(c('TODAY_VI_NUMERIC','TOMORROW_VI_NUMERIC'))#delete the numeric description columns
  )

  if (is.null(venting.data))
  {
    print(paste('No venting map data for:',date_today))
    return()
  }

  if (as.character(venting.data$DATE_ISSUED) != date_today)
  {
    print(paste('Date needed and available did not match, exiting',
                'Date in ECCC:',as.character(venting.data$DATE_ISSUED),
                'Date Requested:',date_today))
    return()

  }

  #create 3 unique venting details for each venting zone
  venting.data.final<-venting.data %>%
    merge(tribble(
      ~SENSI,
      'LOW',
      'MEDIUM',
      'HIGH'
    ))

  #define the rules for venring
  df_vent_rule <- tribble(
    ~SENSI,~TODAY_VI_DESC,~TOMORROW_VI_DESC,~BURN,~BURN_DURATION,~NOTE,
    'LOW','GOOD','GOOD','YES (>1 DAY)','\u2264 6 days*','*Up to 6 days burn, subject to start and end times specified in regulation',
    'LOW','GOOD','FAIR','YES (>1 DAY)','\u2264 6 days*','*Up to 6 days burn, subject to start and end times specified in regulation',
    'LOW','GOOD','POOR','NO','0','',
    'LOW','FAIR','GOOD','YES (>1 DAY)','\u2264 6 days*','*Up to 6 days burn, subject to start and end times specified in regulation',
    'LOW','FAIR','FAIR','YES (>1 DAY)','\u2264 6 days*','*Up to 6 days burn, subject to start and end times specified in regulation',
    'LOW','FAIR','POOR','NO','0','',
    'LOW','POOR','GOOD','NO','0','',
    'LOW','POOR','FAIR','NO','0','',
    'LOW','POOR','POOR','NO','0','',
    'MEDIUM','GOOD','GOOD','YES (>1 DAY)','\u2264 4 days*','*Up to 4 days burn, subject to start and end times specified in regulation',
    'MEDIUM','GOOD','FAIR','YES (>1 DAY)','\u2264 4 days*','*Up to 4 days burn, subject to start and end times specified in regulation',
    'MEDIUM','GOOD','POOR','YES (1-DAY)','1-DAY*','*Open burning ends by 4 p.m. or two hours before sunset, whichever is later, on the same day the open burning starts.',
    'MEDIUM','FAIR','GOOD','NO','0','',
    'MEDIUM','FAIR','FAIR','NO','0','',
    'MEDIUM','FAIR','POOR','NO','0','',
    'MEDIUM','POOR','GOOD','NO','0','',
    'MEDIUM','POOR','FAIR','NO','0','',
    'MEDIUM','POOR','POOR','NO','0','',
    'HIGH','GOOD','GOOD','YES (>1 DAY)','\u2264 2 days*','*Up to 2 days burn, subject to start and end times specified in regulation',
    'HIGH','GOOD','FAIR','YES (>1 DAY)','\u2264 2 days*','*Up to 2 days burn, subject to start and end times specified in regulation',
    'HIGH','GOOD','POOR','YES (1-DAY)','1-DAY*','*Open burning ends by 4 p.m. or two hours before sunset, whichever is later, on the same day the open burning starts.',
    'HIGH','FAIR','GOOD','NO','0','',
    'HIGH','FAIR','FAIR','NO','0','',
    'HIGH','FAIR','POOR','NO','0','',
    'HIGH','POOR','GOOD','NO','0','',
    'HIGH','POOR','FAIR','NO','0','',
    'HIGH','POOR','POOR','NO','0','',
    'HIGH','N/A','N/A','NO','0',''
  )

  if (isCOVID)
  {
    df_vent_rule <- df_vent_rule %>%
      dplyr::mutate(BURN = ifelse(SENSI == 'HIGH',
                                  'NO*',
                                  BURN)) %>%
      dplyr::mutate(BURN_DURATION = ifelse(SENSI == 'HIGH',
                                           '0',
                                           BURN_DURATION)) %>%
      dplyr::mutate(TODAY_VI_DESC = ifelse(SENSI == 'HIGH',
                                           'N/A',
                                           TODAY_VI_DESC)) %>%
      dplyr::mutate(TOMORROW_VI_DESC = ifelse(SENSI == 'HIGH',
                                              'N/A',
                                              TOMORROW_VI_DESC)) %>%
      dplyr::mutate(NOTE = ifelse(SENSI == 'HIGH',
                                  msg_COVID,
                                  NOTE))

    venting.data.final <- venting.data.final %>%
      dplyr::mutate(TODAY_VI_DESC = ifelse(SENSI == 'HIGH',
                                           'N/A',
                                           TODAY_VI_DESC)) %>%
      dplyr::mutate(TOMORROW_VI_DESC = ifelse(SENSI == 'HIGH',
                                              'N/A',
                                              TOMORROW_VI_DESC)) %>%
      dplyr::mutate(TODAY_VI = ifelse(SENSI == 'HIGH',
                                           'N/A',
                                           TODAY_VI)) %>%
      dplyr::mutate(TOMORROW_VI = ifelse(SENSI == 'HIGH',
                                              'N/A',
                                              TOMORROW_VI))
  }


  #combine message with venting.data.final
  venting.data.final <- venting.data.final %>%
    left_join(df_vent_rule) %>%
    unique()


  #change shapefile file types to string
  file.shapefile$Name<-as.character(file.shapefile$Name)
  file.shapefile$SENSI<-as.character(file.shapefile$SENSI)



  venting.data.final<-venting.data.final %>%
    dplyr::rename(Name = NAME)


  venting.shapefile <- merge(file.shapefile,venting.data.final,by=c('Name','SENSI'))

  if (is.null(path.output))
  {
    return(venting.shapefile)
  } else
  {
    #make sure path.output has '/' at the end of name
    if (substr(path.output,nchar(path.output),nchar(path.output)) != '/')
    {path.output <- paste(path.output,'/',sep='')}
    print(paste('saving file to:',path.output))
    #configure the description field
    venting.shapefile$Description<-paste('<p><b>Sensitivity Zone:</b> ',venting.shapefile$SENSI,
                                         '<br/><b>Issued:</b>',format(as.Date(venting.shapefile$DATE_ISSUED),'%a %d %b %Y'),
                                         '<br/><b>Burn Allowed:</b> ',venting.shapefile$BURN,
                                         '<br/><b>Burn Duration:</b> ',venting.shapefile$BURN_DURATION,
                                         "<br/><b>Today's Index:</b> ",venting.shapefile$TODAY_VI_DESC,
                                         ' (',venting.shapefile$TODAY_VI,')',
                                         "<br/><b>Today's Wind Speed:</b> ",venting.shapefile$TODAY_WSPD,' km/h',
                                         "<br/><b>Today's Mix Height:</b> ",
                                         format(as.numeric(venting.shapefile$TODAY_MIX_HEIGHT),big.mark = ','),' m',
                                         "<br/><br/><b>Tomorrow's Index:</b> ",venting.shapefile$TOMORROW_VI_DESC,' (',venting.shapefile$TOMORROW_VI,')',
                                         "<br/><br/>",venting.shapefile$NOTE,
                                         '</p>',sep=''
    )

    #clean up special text like HIGH_CVD
    venting.shapefile$Description <- gsub('HIGH_CVD','HIGH',
                                          venting.shapefile$Description)

    #make changes in description for metrovancouver stations
    temp<-venting.insert[grep('<Metro_description>',x=venting.insert,ignore.case=TRUE)]
    temp<-gsub(pattern="<Metro_description>",replacement="",x=temp,ignore.case=TRUE)
    temp<-gsub(pattern="</Metro_description>",replacement="",x=temp,ignore.case=TRUE)
    venting.shapefile$Description[toupper(venting.shapefile$Name)=='METRO VANCOUVER']<-temp


    #write temporarilty into a KML file
    print(paste('Writing temporary kml file (unmodded) into:',path.temp))
    writeOGR(obj=venting.shapefile,dsn=paste(path.temp,'/vent_temp.kml',sep=''),
             driver='KML',layer="Venting",
             dataset_options = c("NameField=name",
                                 "DescriptionField=Description"
             ),
             overwrite_layer = TRUE)
    #edit the kml file, replace the actual > and <
    #insert the styleURL definition that defines polygon styles
    #this will be inserted at the <Document> line
    venting.text<-readLines(paste(path.temp,'/vent_temp.kml',sep=''))
    print(paste('Created temp kml and extracting content',path.temp))

    #insert styleURL linespath.temp
    #remove default style lines, replace with actual style
    #in each scan, it will identify that
    temp<-NULL
    temp.found<-FALSE
    polygon.name<-NULL  #the venting index region
    polygon.zone<-NULL  #the sensitivity zone of polygon
    temp.exclude<-FALSE

    if (0)
    {
      venting.text_<-venting.text
    }

    #scan the kml file one line at a time
    #to try and insert the style
    for (i in 1:length(venting.text))
    {

      temp.line<-venting.text[i]

      #scan for the <name> which is the venting index region
      if (grepl('<name>',temp.line,ignore.case=TRUE))
      {
        polygon.temp<-unlist(strsplit(tolower(temp.line),split='<name>'))
        #look where the </name> is
        for (j in 1:length(polygon.temp))
        {
          if (grepl('</name>',polygon.temp[j],ignore.case=TRUE))
          {

            polygon.name<-unlist(strsplit(tolower(polygon.temp[j]),split='</name>'))[1]
            print(paste('scanning kml entry for:',toupper(polygon.name)))
          }
        }
      }

      #scan for the Sensitivity zone in description
      if (grepl('<description>',temp.line,ignore.case=TRUE))
      {
        polygon.temp<-unlist(strsplit(tolower(temp.line),split=':'))
        #look where the </name> is
        for (j in 1:length(polygon.temp))
        {
          if (grepl(';sensitivity zone',polygon.temp[j],ignore.case=TRUE))
          {
            #
            #print(paste('sensitivity',polygon.temp[j+1]))
            #define polygon.zone's value
            polygon.zone<-unlist(strsplit(polygon.temp[j+1],split="&lt;/b&gt; "))[2]
            polygon.zone<-toupper(unlist(strsplit(polygon.zone,split='&'))[1])
            #print(polygon.zone)
            #   print(paste('scanning kml entry for:',toupper(polygon.name)))
            # if (polygon.zone != 'NA')
            # {
            #   crash
            # }
          }
        }
      }
      #


      # #scan for the <style> line and replace with the style
      if (!is.null(polygon.name) & (grepl('<style>',temp.line,ignore.case=TRUE)))
      {
        #replace the style line with the template styles
        #determine if this area and zone is a burn or no burn zone

        venting.temp<-venting.data.final%>%
          dplyr::filter(tolower(Name)==tolower(polygon.name))%>%
          dplyr::filter(tolower(SENSI)==tolower(polygon.zone))

        if (nrow(venting.temp)>0)
        {
          #define the style for the polygon based on BURN field
          #and also force to use NA if metrovancouver
          temp.style<-'NA'
          temp.style<-ifelse(venting.temp[1,]$BURN=='YES (1-DAY)','YES-1DAY',temp.style)
          temp.style<-ifelse(venting.temp[1,]$BURN=='YES (>1 DAY)','YES-MULTIDAY',temp.style)
          temp.style<-ifelse(venting.temp[1,]$BURN=='NO','NO',temp.style)
          temp.style<-ifelse(venting.temp[1,]$BURN=='NO*','NO*',temp.style)
          temp.style<-ifelse(grepl('vancouver',venting.temp[1,]$Name,ignore.case = TRUE),
                             'NA',temp.style)
          temp.line<-paste('<styleUrl>#',temp.style,'</styleUrl>',sep='')
        } else
        {
          temp.line<-'<styleUrl>#NA</styleUrl>'
        }
        print(paste('inserting the style:',temp.line,'for',venting.temp[1,]$SENSI))
        if (0)
        {
          #this crashes
          # crash
        }
      }

      if (grepl('<Folder>',temp.line,ignore.case=TRUE))
      {
        temp.line<-''
      }
      if (grepl('</Folder>',temp.line,ignore.case=TRUE))
      {
        temp.line<-''
      }
      if (temp.exclude){temp.line<-''}
      if (!temp.line=='')
      {temp<-append(temp,temp.line)} #add temp.line only if !temp.exclude

      #insert the style template
      if (!temp.found & grepl('<Document',temp.line,ignore.case=TRUE))
      {
        print(paste('Inserting the style files:',
                    length(venting.insert),'rows in row number',
                    i))
        temp.found<-TRUE
        temp<-append(temp,venting.insert)
      }
    }

    #save the resulting data


    if (HD)
    {
      filename_<-'Venting_Index_HD.kml'
    } else
    {
      filename_<-'Venting_Index.kml'
    }

    print(paste('Writing the index file in temp location:',
                path.temp))
    #delete the N/A VI value
    temp <- gsub('N/A (N/A)','N/A',temp,ignore.case = TRUE)

    write(x=temp,file=paste(path.temp,'/Venting_Index.kml',sep=''))

    #move to final location in temp file name, then rename to final name
    print(paste('Copying the file to final location',path.output,filename_))


    file.copy(from=paste(path.temp,'/Venting_Index.kml',sep=''),
              to=paste(path.output,'Venting_Index.kml_',sep=''),
              overwrite=TRUE)
    unlink(paste(path.temp,'/Venting_Index.kml',sep=''),force= TRUE)

    file.rename(from=paste(path.output,'Venting_Index.kml_',sep=''),
                to=paste(path.output,filename_,sep=''))

    return(TRUE)
  }

}





#' Retrieves the venting index FLCN 39 from ECCC data mart
#'
#' This function connects to ECCC datamart and retrieves venting data
#' @param date.start string in YYYY-mm-dd
#'        if left undefined, it uses the current date
#'
#' @examples
#' GET_VENTING_ECCC()
#'
#' @export
GET_VENTING_ECCC<-function(date.start=NULL)
{
  #debug
  if (0)
  {
    date.start<-date_today
    date.start <- '2019-12-25'
    date.start <- '2016-09-01'
  }
  #date.start<-NULL
  #end debug

  #setup---
  RUN_PACKAGE(c('dplyr','data.table','readr','RCurl','tibble','lubridate','tidyr'))
  #PURPOSE: Retrieves venting data from the specified URL
  #venting.metadata='ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Air_Quality/VentingMetaData.csv'
  venting.url='http://dd.weatheroffice.ec.gc.ca/bulletins/alphanumeric/'    #ECCC venting index data
  #venting.metadata='ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Air_Quality/VentingMetaData.csv'
  venting.metadata='https://envistaweb.env.gov.bc.ca/aqo/files/VentingMetaData.csv'
  venting.url2 <- 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/VentingBulletins/'


  if (is.null(date.start))
  {
    #get the latest venting files
    temp.list<-GET_URL_FOLDERS()%>%
      dplyr::arrange(desc(DATE))%>%
      dplyr::mutate(URL=paste(venting.url,FOLDER,'FL/CWVR/',sep=''))

  } else
  {
    #get the one that matches specified date
    date.start <- as.character(date.start,'%Y-%m-%d')
    date.start<-gsub('-','',date.start)
    temp.list<-GET_URL_FOLDERS()%>%
      dplyr::arrange(desc(DATE))%>%
      dplyr::mutate(DATE_TEMP=gsub('/','',FOLDER))%>%
      RENAME_COLUMN('DATE_TEMP')%>%
      dplyr::mutate(URL=paste(venting.url,FOLDER,'FL/CWVR/',sep=''))%>%
      dplyr::filter(grepl(date.start,URL))

  }
  venting.date<-NULL
  venting.content<- NULL #this will have FLCN text

  #scan for date,
  #    !=0 means ECCC (priority)
  #    ==  sca from ENV ftp
  #updated: 2020-05-01
  #sites older than 2 days will use ENV FTP site

  date_now <- as.character(Sys.Date(),'%Y-%m-%d')
  date_diff <- difftime(date_now,date.start,units='days')
  if (nrow(temp.list) != 0 & date_diff<3)
  {

    for (i in 1:nrow(temp.list))
    {

      # temp.download<-rbind(temp.download,
      #                      GET_URL_FOLDERS(temp.list[i,]$URL)%>%
      #                        dplyr::mutate(URL=paste(temp.list[i,]$URL,FOLDER,sep=''))%>%
      #                        dplyr::arrange(desc(DATE))
      #                      )
      if (is.null(venting.content))
      {
        temp.download <- NULL

        try(
          temp.download<-GET_URL_FOLDERS(temp.list[i,]$URL)%>%
            dplyr::mutate(URL=paste(temp.list[i,]$URL,FOLDER,sep=''))%>%
            dplyr::arrange(desc(DATE))
        )


        if (!is.null(temp.download))
        {
          for (temp.download.url in temp.download$URL)
          {
            if (is.null(venting.content))
            {
              #check the files in the url folder
              print(paste('scanning content of',temp.download.url))
              temp<-GET_URL_FOLDERS(temp.download.url)%>%
                filter(grepl('FLCN39',FOLDER,ignore.case=TRUE))


              if (nrow(temp)>0)
              {
                print(paste('processing content:',paste(temp.download.url,temp$FOLDER,sep='')))
                temp.result <- NULL
                temp.download.url <- gsub('http://','https://',temp.download.url)
                try(temp.result <- unlist(readr::read_lines(paste(temp.download.url,temp$FOLDER,sep=''))),silent = TRUE)
                temp.download.url <- gsub('https://','http://',temp.download.url)
                try(temp.result <- unlist(readr::read_lines(paste(temp.download.url,temp$FOLDER,sep=''))),silent = TRUE)
                venting.content <- temp.result
                download.url <- temp.download.url
              }
            }
          }
        }
      }
    }


  } else
  {
    #if ECCC does not have data, located from ENV ftp, venting.url2

    ftp.url <- paste(venting.url2,year(ymd(date.start)),'/',sep='')
    print(paste('Retrieving data from:',ftp.url))
    lst_ventfiles <- getURL(ftp.url,verbose=TRUE,
                            ftp.use.epsv=TRUE, dirlistonly = TRUE
    )

    #create dataframe with 0 row
    df_ventfiles <- tibble(Files='BLANK') %>%
      filter(FALSE)
    try(
      df_ventfiles <- tibble(Files = unlist(strsplit(lst_ventfiles,'\r\n'))) %>%
        tidyr::separate(col=Files,
                        into=c('TXT1','TXT2','TXT3'),
                        sep='_',
                        remove = FALSE)%>%
        dplyr::mutate(Created = ymd(TXT3),
                      Fullpath = paste(ftp.url,Files,sep='')) %>%
        dplyr::filter(Created == ymd(date.start))
    )


    try(
      venting.content <- unlist(readr::read_lines(df_ventfiles$Fullpath[1])),
      silent = TRUE
    )
    download.url <- ftp.url
    #terminate if no data found
    #that means data not in ENV, ECCC sites
    if (is.null(venting.content))
    {

      print(paste('There are no folders from ECCC for this date:',date.start))
      return(NULL)

    }
  }



  venting.guidelines<-c('poor','fair','good') #the guideline list ,used to identify if line is data

  #retrieve venting.date
  if (length(venting.content)>0 & !is.null(venting.content))
  {

    #scan date from path

    venting.date_ <- unlist(strsplit(download.url,split='/'))
    for (j in 1:length(venting.date_))
    {
      if (IsDate(venting.date_[j]))
      {
        venting.date <- as.character(as.Date(venting.date_[j],'%Y%m%d'),format='%Y-%m-%d')
      }
    }

    #if venting date not retrieved, use the search date
    if (is.null(venting.date))
    {
      venting.date <- as.character(ymd(date.start),format='%Y-%m-%d')
    }
  } else
  {
    return(NULL)
  }

  #get venting metadata,check if file exist first
  venting.meta<-read.table(venting.metadata,sep=',',header = TRUE)%>%
    RENAME_COLUMN('')

  #scan each line and retrieve date details
  #venting.date<-NULL

  # for (i in 1:length(venting.content))
  #   #find the date
  # {
  #   #print(paste('Parsing venting file:',i,'of',length(venting.content)))
  #   temp<-venting.content[i]
  #   if (is.null(venting.date))
  #   {
  #     #identify if there is date in this line
  #     if (IsDate(temp))
  #     {
  #
  #       venting.date<-as.character(as.Date(temp,'%d-%B-%Y'),format='%Y-%m-%d')
  #
  #     }
  #
  #   }
  # }

  venting.table.temp<-include(toMatch=toupper(venting.guidelines),theList=toupper(venting.content))
  venting.table<-tibble::data_frame(DATE_ISSUED=venting.date,RAW=venting.table.temp)%>%
    tidyr::separate(col=RAW,into=c('VENTING_INDEX_ABBREV','X1'),sep='  +',extra='merge',fill="right")%>%
    tidyr::separate(col=X1,into=c('X2','CURRENT_WSPD','CURRENT_MIX_HEIGHT',
                                  'X5','TODAY_WSPD','TODAY_MIX_HEIGHT',
                                  'X8','TOMORROW_WSPD','TOMORROW_MIX_HEIGHT'),
                    sep=' +',extra='drop',fill='right')%>%
    dplyr::filter(!is.na(CURRENT_WSPD))%>% #remove those lines that are not part of table
    tidyr::separate(col=X2,into=c('CURRENT_VI','CURRENT_VI_DESC'),sep='/',extra='drop')%>%
    tidyr::separate(col=X5,into=c('TODAY_VI','TODAY_VI_DESC'),sep='/',extra='drop')%>%
    tidyr::separate(col=X8,into=c('TOMORROW_VI','TOMORROW_VI_DESC'),sep='/',extra='drop')
  if (nrow(venting.table)>0)
  {
    venting.table<-venting.table%>%
      merge(venting.meta,all.x=TRUE)
    print(paste('OK.',nrow(venting.table),'rows'))


  }


  if (nrow(venting.table)>0)
  {
    print('Success. Ignore warnings and errors')
    venting.table <- tibble::as.tibble(venting.table)
  }
  return(venting.table)


}


#' GET ECCC FORECAST function
#'
#' This function retrieves latest forecast from ECCC models
#' @param parameter is a vector string of AQHI, PM25, NO2, O3, PM10. It will include all forecase if NULL
#' @export
importECCC_forecast<-function(parameter=NULL)
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
    as_tibble()%>%
    return()
}



