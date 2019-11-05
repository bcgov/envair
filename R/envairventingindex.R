


#' Retrieves the venting index FLCN 39 from ECCC data mart
#'
#' This function connects to ECCC datamart and retrieves venting data
#' @param date.start string year in YYYY-mm-dd
#' GET_VENTING_ECCC()
#' @export
GET_VENTING_ECCC<-function(date.start=NULL)
{
  #debug
  # date.start<-NULL
  #end debug

  #PURPOSE: Retrieves venting data from the specified URL
  #venting.metadata='ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Air_Quality/VentingMetaData.csv'
  venting.url='http://dd.weatheroffice.ec.gc.ca/bulletins/alphanumeric/'    #ECCC venting index data
  #venting.metadata='ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Air_Quality/VentingMetaData.csv'
  venting.metadata='https://envistaweb.env.gov.bc.ca/aqo/files/VentingMetaData.csv'
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
      temp.download<-GET_URL_FOLDERS(temp.list[i,]$URL)%>%
        dplyr::mutate(URL=paste(temp.list[i,]$URL,FOLDER,sep=''))%>%
        dplyr::arrange(desc(DATE))
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

            temp.result<-unlist(readLines(paste(temp.download.url,temp$FOLDER,sep='')))
            if (length(temp.result)>0)
            {
              print (paste('found Venting file in',temp$FOLDER))
              venting.content<-temp.result
            }
          }
        }
      }
    }
  }



  venting.guidelines<-c('poor','fair','good') #the guideline list ,used to identify if line is data


  #get venting metadata,check if file exist first
  venting.meta<-(read.table(venting.metadata,sep=',',header=TRUE))%>%
    RENAME_COLUMN('')

  #scan each line and retrieve date details
  venting.date<-NULL

  for (i in 1:length(venting.content))
    #find the date
  {
    #print(paste('Parsing venting file:',i,'of',length(venting.content)))
    temp<-venting.content[i]
    if (is.null(venting.date))
    {
      #identify if there is date in this line
      if (IsDate(temp))
      {

        venting.date<-as.character(as.Date(temp,'%d-%B-%Y'),format='%Y-%m-%d')

      }

    }
  }

  venting.table.temp<-include(toMatch=toupper(venting.guidelines),theList=toupper(venting.content))
  venting.table<-data.frame(DATE_ISSUED=venting.date,RAW=venting.table.temp)%>%
    tidyr::separate(col=RAW,into=c('VENTING_INDEX_ABBREV','X1'),sep='  +',extra='merge',fill="right")%>%
    tidyr::separate(col=X1,into=c('X2','CURRENT_WSPD','CURRENT_MIX_HEIGHT',
                                  'X5','TODAY_WSPD','TODAY_MIX_HEIGHT',
                                  'X8','TOMORROW_WSPD','TOMORROW_MIX_HEIGHT'),
                    sep=' +',extra='drop',fill='right')%>%
    dplyr::filter(!is.na(CURRENT_WSPD))%>% #remove those lines that are not part of table
    tidyr::separate(col=X2,into=c('CURRENT_VI','CURRENT_VI_DESC'),sep='/',extra='drop')%>%
    tidyr::separate(col=X5,into=c('TODAY_VI','TODAY_VI_DESC'),sep='/',extra='drop')%>%
    tidyr::separate(col=X8,into=c('TOMORROW_VI','TOMORROW_VI_DESC'),sep='/',extra='drop')
  if (nrow(venting.table)>0){
    venting.table<-venting.table%>%
      merge(venting.meta,all.x=TRUE)
    print(paste('OK.',nrow(venting.table),'rows'))


  }
  return(venting.table)


}

#' Creates kml files or shapefiles
#'
#' This function creates kml files using the new OBSCR sensitivity zones and venting regions
#' @param path.output where kml files will be saved. If null, function returns shape file
#'
#' GET_VENTING_OBSCR()
#' @export
GET_VENTING_OBSCR<-function(path.output=NULL)
{

  #PURPOSE: This function generates shapefile with venting index attributes
  #saves the result in specified file
  #or returns as shapefile is output file is not defined
  #Created: Jerome A. Robles, Ph.D.
  #2019-05-27


  #debug
  # file.output=NULL
  # path.output<-'//particulate.dmz/apps_data/venting'
  #end debug lines

  RUN_PACKAGE(c('rlang','R6','Rcpp','tidyr','namespace','dplyr','ggplot2','RODBC','reshape',
                'lazyeval','zoo','DataCombine','data.table',
                'fasttime','readr','RCurl','tidyr','lubridate',
                'raster','rgdal','shapefiles','xml2','zip'))

  path.temp<-paste(getwd(),'/TEMP',sep='')
  dir.create(path.temp,showWarnings = FALSE)
  file.temp<-paste(path.temp,'/venting.kml',sep='')
  URL.shapefile<-'https://envistaweb.env.gov.bc.ca/aqo/files/Venting_Blank.kml'
  URL.inserfile<-'https://envistaweb.env.gov.bc.ca/aqo/files/Venting_format.xml'
  download.file(URL.shapefile,file.temp)

  #read KML file
  file.shapefile<-readKML(file.temp,keep_name_description = FALSE)
  file.remove(file.temp)
  #read the insert file for kml file
  venting.insert<-readLines(URL.inserfile)

  venting.data<-GET_VENTING_ECCC()%>%
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
    #apply rules for sensitivity zones
    #removed during the 2019 OBSCR update
    # dplyr::mutate(LOW_SENSITIVITY_BURN=ifelse(TODAY_VI_NUMERIC>0 & TOMORROW_VI_NUMERIC>0,'YES','NO'))%>%
    # dplyr::mutate(MEDIUM_SENSITIVITY_BURN=ifelse(TODAY_VI_NUMERIC>1 & TOMORROW_VI_NUMERIC>0,'YES','NO'))%>%
    # dplyr::mutate(HIGH_SENSITIVITY_BURN=ifelse(TODAY_VI_NUMERIC>1 & TOMORROW_VI_NUMERIC>0,'YES','NO'))%>%

    #
    RENAME_COLUMN(c('TODAY_VI_NUMERIC','TOMORROW_VI_NUMERIC'))#delete the numeric description columns

  #create 3 unique venting details for each zone
  venting.data.final<-NULL
  for (SENS_ZONE in c('LOW','MEDIUM','HIGH'))
  {
    print(SENS_ZONE)

    venting.data.final<-venting.data.final%>%
      rbind(
        venting.data%>%
          dplyr::mutate(SENSI=SENS_ZONE)
        #removed during 2019 OBSCR update
        # RENAME_COLUMN(paste(SENS_ZONE,'_SENSITIVITY_BURN',sep=''),'TEMP')%>%
        # dplyr::mutate(BURN=TEMP)%>%
        # RENAME_COLUMN(c('TEMP','LOW_SENSITIVITY_BURN','MEDIUM_SENSITIVITY_BURN','HIGH_SENSITIVITY_BURN'))
      )


  }



  #prepare venting data and shapr file for merging
  #rename venting data column names to match shapefile name

  #note that VI_NUMERIC=<TODAY><TOMORROW>
  #where good=2, fair=1, poor=0
  venting.data.final<-venting.data.final%>%
    dplyr::mutate(BURN_DURATION='N/A')%>% #define the initial burn duration
    dplyr::mutate(BURN_DURATION=ifelse(SENSI=='HIGH' & VI_NUMERIC %in% c(21,22),'\u2264 2 days*',BURN_DURATION))%>%
        dplyr::mutate(BURN_DURATION=ifelse(SENSI=='HIGH' & VI_NUMERIC %in% c(00,01,02,10,11,12),'0',BURN_DURATION))%>%
    dplyr::mutate(BURN_DURATION=ifelse(SENSI=='LOW' & VI_NUMERIC %in% c(11,12,21,22),'\u2264 6 days*',BURN_DURATION))%>%
    dplyr::mutate(BURN_DURATION=ifelse(SENSI=='LOW' & VI_NUMERIC %in% c(00,01,02,10,20),'0',BURN_DURATION))%>%
    dplyr::mutate(BURN_DURATION=ifelse(SENSI=='MEDIUM' & VI_NUMERIC %in% c(21,22),'\u2264 4 days*',BURN_DURATION))%>%
    dplyr::mutate(BURN_DURATION=ifelse(SENSI=='MEDIUM' & VI_NUMERIC %in% c(00,01,02,10,11,12),'0',BURN_DURATION))%>%
    #Current condition good, on medium and high sensitivity zone
    dplyr::mutate(BURN_DURATION=ifelse(SENSI=='MEDIUM' & VI_NUMERIC %in% c(20),'1-DAY*',BURN_DURATION))%>%
    dplyr::mutate(BURN_DURATION=ifelse(SENSI=='HIGH' & VI_NUMERIC==20,'1-DAY*',BURN_DURATION))%>%

    dplyr::mutate(BURN='N/A')%>%
    dplyr::mutate(BURN=ifelse(BURN_DURATION=='0','NO',BURN))%>%
    dplyr::mutate(BURN=ifelse(!BURN_DURATION %in% c('N/A','NOT ALLOWED','NO','NA','0'),
                              'YES (1-DAY)',BURN))%>%
    dplyr::mutate(BURN=ifelse(!BURN_DURATION %in% c('N/A','NOT ALLOWED','NO','NA','0','1-DAY','1-DAY*'),
                              'YES (>1 DAY)',BURN))%>%
    dplyr::mutate(NOTE='')%>%
    dplyr::mutate(NOTE=ifelse(BURN_DURATION=='\u2264 2 days*','*Up to 2 days burn, subject to start and end times specified in regulation',NOTE))%>%
    dplyr::mutate(NOTE=ifelse(BURN_DURATION=='\u2264 4 days*','*Up to 4 days burn, subject to start and end times specified in regulation',NOTE))%>%
    dplyr::mutate(NOTE=ifelse(BURN_DURATION=='\u2264 6 days*','*Up to 6 days burn, subject to start and end times specified in regulation',NOTE))%>%
    dplyr::mutate(NOTE=ifelse(BURN_DURATION=='1-DAY*','*Open burning ends by 4 p.m. or two hours before sunset, whichever is later, on the same day the open burning starts.',NOTE))%>%
    RENAME_COLUMN(c('NAME'),c('Name'))


  #change shapefile file types to string
  file.shapefile$Name<-as.character(file.shapefile$Name)
  file.shapefile$SENSI<-as.character(file.shapefile$SENSI)

  venting.shapefile<-merge(file.shapefile,venting.data.final,by=c('Name','SENSI'))

  if (is.null(path.output))
  {
    return(venting.shapefile)
  } else
  {

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

    #make changes in description for metrovancouver stations
    temp<-venting.insert[grep('<Metro_description>',x=venting.insert,ignore.case=TRUE)]
    temp<-gsub(pattern="<Metro_description>",replacement="",x=temp,ignore.case=TRUE)
    temp<-gsub(pattern="</Metro_description>",replacement="",x=temp,ignore.case=TRUE)
    venting.shapefile$Description[toupper(venting.shapefile$NAME)=='METRO VANCOUVER']<-temp

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


    #insert styleURL lines
    #remove default style lines, replace with actual style
    #in each scan, it will identify that
    temp<-NULL
    temp.found<-FALSE
    polygon.name<-NULL  #the venting index region
    polygon.zone<-NULL  #the sensitivity zone of polygon
    temp.exclude<-FALSE

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
          if (grepl('sensitivity',polygon.temp[j],ignore.case=TRUE))
          {
            #
            #print(paste('sensitivity',polygon.temp[j+1]))
            #define polygon.zone's value
            polygon.zone<-unlist(strsplit(polygon.temp[j+1],split="&lt;/b&gt; "))[2]
            polygon.zone<-toupper(unlist(strsplit(polygon.zone,split='&'))[1])
            #print(polygon.zone)
            #   print(paste('scanning kml entry for:',toupper(polygon.name)))
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
          temp.style<-'NA'
          temp.style<-ifelse(venting.temp[1,]$BURN=='YES (1-DAY)','YES-1DAY',temp.style)
          temp.style<-ifelse(venting.temp[1,]$BURN=='YES (>1 DAY)','YES-MULTIDAY',temp.style)
          temp.style<-ifelse(venting.temp[1,]$BURN=='NO','NO',temp.style)
          temp.line<-paste('<styleUrl>#',temp.style,'</styleUrl>',sep='')
        } else
        {
          temp.line<-'<styleUrl>#NA</styleUrl>'
        }
        print(paste('inserting the style:',temp.line))
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
    print(paste('Writing the index file in temp location:',
                path.temp))
    write(x=temp,file=paste(path.temp,'/Venting_Index.kml',sep=''))

    #move to final location in temp file name, then rename to final name
    print(paste('Copying the file to final location',path.output))
    file.copy(from=paste(path.temp,'/Venting_Index.kml',sep=''),
              to=paste(path.output,'/Venting_Index.kml_',sep=''),
              overwrite=TRUE)
    file.rename(from=paste(path.output,'/Venting_Index.kml_',sep=''),
                to=paste(path.output,'/Venting_Index.kml',sep=''))
    file.remove(paste(path.temp,'/Venting_Index.kml',sep=''))

    return(TRUE)
  }

}


