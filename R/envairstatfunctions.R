#' Gets the statistical details of a parameter
#'
#' This function renames or deletes the column of a dataframe based on its name
#' @param data.year numeric year. includes unverified and validated year
#' @param parameter string or vector containing the parameter that will be evaluated.
#' @param instrument.ignore if specified, ignores the instrument technology and merges dataset during statistical calculations.
#'              Default is NULL where it becomes FALSE for particulate (PM25,PM10) instruments
#' @keywords statistics, annual data, valid data, unverified data
#' GET_STATISTICS_PARAMETER()
#'
#' @export
GET_STATISTICS_PARAMETER<-function(data.year,parameter,instrument.ignore=NULL)
{
  #gets the statistics for that parameter at the particular year
  #parameters include PM2.5, PM10,O3,no2,no,so2,trs,h2s,co
  #note that this can only evaluate one parameter
  #instrument.ignore will ignore the instrument

  # # # #debug initialization
  # data.year<-2018
  # parameter<-'pm25'
  # instrument.ignore = !(parameter %in% c('pm25','pm10'))
  # data.source<-NULL
  # data.temp<-GET_PARAMETER_DATA('pm25',2018,2018)
  # # data.source<-GET_DATA_FTP_LARGE('h2s')
  # #

  #

  if (0)
  {
    data.year <- 2020
    parameter <- 'o3'
    source('envairfunctions.R')
    source('envairstatfunctions.R')
    source('importbc_data.R')
    instrument.ignore=NULL

    data.source=NULL
  }

  #setup
  parameter<-tolower(parameter)
  RUN_PACKAGE(c('dplyr'))
  if (is.null(instrument.ignore))
  {
    instrument.ignore=!(tolower(parameter) %in% c('pm25','pm10'))
  }

  #download data if not provided

  data.input <- NULL
  try(data.input <- importBC_data(parameter,data.year,pad = TRUE))

  #escape if null----
  if (is.null(data.input))
  {
    print(paste('The parameter',parameter,'came out blank'))
    return(NULL)
  }

  #rename instrument name when instrument is ignored in grouping
  if (instrument.ignore)
  {
    data.input <-  data.input %>%
      mutate(INSTRUMENT = paste(toupper(parameter),'Analyzer'))
  }


  #added to merge multiple instruments for PM2.5 from Kamloops Federal Building----
  #applies to 2017 data only
  if (data.year==2017 & parameter=='pm25')
  {
    #separate Kamloops from non-Kamlooops station
    data.input_ <- data.input %>%
      dplyr::filter(STATION_NAME != 'Kamloops Federal Building')

    data.input_x <- data.input %>%
      dplyr::filter(STATION_NAME == 'Kamloops Federal Building',
                    !is.na(RAW_VALUE))

    #locate duplicated dates and remove, prioritize SHARP
    data.input_x <- data.input_x %>%
      group_by(DATE_PST,STATION_NAME) %>%
      dplyr::arrange(desc(INSTRUMENT)) %>%
      dplyr::mutate(index = 1:n()) %>%
      ungroup() %>%
      dplyr::mutate(INSTRUMENT = 'SHARP5030/BAM1020') %>%
      filter(index ==1) %>%
      RENAME_COLUMN('index')





    #combine again
    data.input <- rbind.fill(data.input_,data.input_x) %>%
      arrange(STATION_NAME,INSTRUMENT,DATE_PST)
    rm('data.input_')
    rm('data.input_x')

  }




  #add YEAR in data input
  #to make it consistent across data sources
  data.input<-data.input%>%
    dplyr::mutate(YEAR=data.year)


  data.output<-NULL
  if (nrow(data.input)>0)
  {

    if (parameter %in% c('pm25','pm10'))
    {
      print(paste('Creating data summary for PM25 or PM10, year:',data.year))
      statistics.count<-GET_VALID_COUNT(data.input,day.threshold=0.75,precision=1)
      statistics.percentiles<-data.input%>%
        RENAME_COLUMN('RAW_VALUE')%>%   #remove RAW_VALUE column
        GET_PERCENTILES_NAPS(column.name = 'ROUNDED_VALUE')


      #---Daily Percentiles--------------------------------
      data.daily<-data.input%>%
        GET_DAILY_MEAN(column.name = 'ROUNDED_VALUE',precision=2,data.threshold = 0.75)


      statistics.percentiles.daily<-data.daily%>%
        GET_PERCENTILES_NAPS(column.name='ROUNDED_VALUE_DAILY_MEAN',precision=1)%>%
        RENAME_COLUMN(c('DATE_DAY','VALID_COUNT')) %>%  #remove unused column
        unique()
      #rename the percentiles columns, e.g., 10% becomes 10%(day)
      column.names<-colnames(statistics.percentiles.daily)[grepl('%',colnames(statistics.percentiles.daily))]
      statistics.percentiles.daily<-statistics.percentiles.daily%>%
        RENAME_COLUMN(column.names,paste(column.names,'(day)',sep=''))

      statistics.exceedance<-data.daily%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE_DAILY_MEAN',
                             data.exceedancethreshold = ifelse(parameter=='pm25',25,50))%>%
        RENAME_COLUMN('EXCEEDANCE',ifelse(parameter=='pm25',
                                          'EXCEEDANCES OF DAILY AVG >25 ug/m3',
                                          'EXCEEDANCES OF DAILY AVG >50 ug/m3'))
      statistics.percentiles.98<-data.daily%>%
        dplyr::select(STATION_NAME,EMS_ID,INSTRUMENT,ROUNDED_VALUE_DAILY_MEAN)%>%
        GET_PERCENTILES_NAPS(column.name='ROUNDED_VALUE_DAILY_MEAN',precision=1,percentiles = 98)%>%
        RENAME_COLUMN('98%','98TH PERCENTILE OF DAILY AVG')%>%
        unique()


      data.output<-statistics.percentiles%>%
        merge(statistics.percentiles.daily,all.x=TRUE)%>%
        merge(statistics.exceedance,all.x=TRUE)%>%
        merge(statistics.percentiles.98,all.x=TRUE)%>%
        merge(statistics.count)
    }

    if (parameter=='o3')
    {
      print(paste('Creating data summary for O3, year:',data.year))

      #download the previous year data

      # data.input.previous<-GET_DATA_FTP(data.year-1,parameter)%>%
      #   dplyr::mutate(INSTRUMENT=ifelse(instrument.ignore,
      #                                   paste(toupper(parameter),'Analyser'),
      #                                   as.character(INSTRUMENT)))
      #


        data.input.previous<-importBC_data(parameter,data.year-1)

        #rename instrument name when instrument is ignored in grouping
        if (instrument.ignore)
        {
          data.input.previous <-  data.input.previous %>%
            mutate(INSTRUMENT = paste(toupper(parameter),'Analyzer'))
        }


      #make input data consistent before merge
      data.input<-data.input%>%
        dplyr::mutate(YEAR=data.year)
      data.input.previous<-data.input.previous%>%
        dplyr::mutate(YEAR=data.year-1)

      statistics.count<-GET_VALID_COUNT(data.input,day.threshold=0.75,precision=1,Q2Q3=TRUE)
      statistics.percentiles<-data.input%>%
        RENAME_COLUMN('RAW_VALUE')%>%   #remove RAW_VALUE column
        GET_PERCENTILES_NAPS(column.name = 'ROUNDED_VALUE')

      data.input.8hr<-data.input%>%
        GET_ROLLING_MEAN(data.input.previous=data.input.previous,
                         data.averaginghour=8,threshold=0.75)%>%
        dplyr::mutate(ROUNDED_VALUE_8HR=as.numeric(ROUNDED_VALUE_8HR))

      data.input.8hrdm<-data.input.8hr%>%   #daily 8hr daily maximum
        GET_DAILY_MAX(column.name='ROUNDED_VALUE_8HR')

      statistics.D8HM_percentiles<-data.input.8hrdm%>%
        #dplyr::select(DATE_PST,STATION_NAME,EMS_ID,INSTRUMENT,ROUNDED_VALUE_8HR)%>%
        #GET_DAILY_MAX(column.name='ROUNDED_VALUE_8HR')%>%
        GET_PERCENTILES_NAPS(column.name='ROUNDED_VALUE_8HR_DAILY_MAX')%>%
        RENAME_COLUMN('DATE_DAY')%>%
        unique()%>%
        dplyr::mutate(YEAR=data.year)
      #rename the percentiles columns, e.g., 10% becomes 10%(D8HM)
      column.names<-colnames(statistics.D8HM_percentiles)[
        grepl('%',colnames(statistics.D8HM_percentiles))]
      statistics.D8HM_percentiles<-statistics.D8HM_percentiles%>%
        RENAME_COLUMN(column.names,paste(column.names,'(D8HM)',sep=''))
      #get the exceedance
      statistics.exceedance_hr<-data.input%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE',data.exceedancethreshold = 82)%>%
        RENAME_COLUMN('EXCEEDANCE','HOURLY EXCEEDANCE >82ppb')

      statistics.exceedance<-data.input.8hrdm%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE_8HR_DAILY_MAX',data.exceedancethreshold = 63)%>%
        RENAME_COLUMN('EXCEEDANCE','D8HM EXCEEDANCE >63ppb')
      #get the 4th highest value
      statistics.4th<-data.input.8hrdm %>%
        GET_nTH_HIGHEST(column.name='ROUNDED_VALUE_8HR_DAILY_MAX',data.order=4)%>%
        RENAME_COLUMN('ROUNDED_VALUE_8HR_DAILY_MAX','4th Highest D8HM')


      data.output<-statistics.percentiles%>%
        merge(statistics.D8HM_percentiles,all.x=TRUE)%>%
        merge(statistics.exceedance_hr,all.x=TRUE)%>%
        merge(statistics.exceedance,all.x=TRUE)%>%
        merge(statistics.4th)%>%
        merge(statistics.count)
    }

    if (parameter=='no2')
    {
      print(paste('Creating data summary for NO2, year:',data.year))
      statistics.count<-GET_VALID_COUNT(data.input,day.threshold=0.75,precision=1)
      statistics.percentiles<-data.input%>%
        RENAME_COLUMN('RAW_VALUE')%>%   #remove RAW_VALUE column
        GET_PERCENTILES_NAPS(column.name = 'ROUNDED_VALUE')
      statistics.exceedance<-data.input%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE',data.exceedancethreshold = 100)%>%
        RENAME_COLUMN('EXCEEDANCE','HOURLY EXCEEDANCES >100 ppb')

      data.D1HM<-data.input%>%   #1-HR maximum data
        GET_DAILY_MAX(column.name='ROUNDED_VALUE',data.threshold=0.75,exception_threshold = 60)%>%
        RENAME_COLUMN('COUNT')

      statistics.D1HM.percentiles<-data.D1HM%>%
        GET_PERCENTILES_NAPS(column.name='ROUNDED_VALUE_DAILY_MAX')%>%
        RENAME_COLUMN('DATE_DAY')%>%
        unique()
      #rename the percentiles columns, e.g., 10% becomes 10%(D1HM)
      column.names<-colnames(statistics.D1HM.percentiles)[grepl('%',colnames(statistics.D1HM.percentiles))]
      statistics.D1HM.percentiles<-statistics.D1HM.percentiles%>%
        RENAME_COLUMN(column.names,paste(column.names,'(D1HM)',sep=''))

      statistics.exceedance.D1HM<-data.D1HM%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE_DAILY_MAX',data.exceedancethreshold = 100)%>%
        RENAME_COLUMN('EXCEEDANCE','D1HM EXCEEDANCES >100 ppb')

      data.output<-statistics.percentiles%>%
        merge(statistics.exceedance,all.x=TRUE)%>%
        merge(statistics.D1HM.percentiles,all.x=TRUE)%>%
        merge(statistics.exceedance.D1HM,all.x=TRUE)%>%
        merge(statistics.count)

    }

    if (parameter=='so2')
    {
      print(paste('Creating data summary for SO2, year:',data.year))
      statistics.count<-GET_VALID_COUNT(data.input,day.threshold=0.75,precision=1)

      statistics.percentiles<-data.input%>%
        RENAME_COLUMN('RAW_VALUE')%>%   #remove RAW_VALUE column
        GET_PERCENTILES_NAPS(column.name = 'ROUNDED_VALUE')%>%
        dplyr::mutate(YEAR=data.year)


      statistics.exceedance<-data.input%>% #count the number of hourly exceedance
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE',data.exceedancethreshold = 70)%>%
        RENAME_COLUMN('EXCEEDANCE','HOURLY EXCEEDANCES >70 ppb')
      statistics.exceedance2<-data.input%>% #count the number of hourly exceedance
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE',data.exceedancethreshold = 75)%>%
        RENAME_COLUMN('EXCEEDANCE','HOURLY EXCEEDANCES >75 ppb')

      statistics.exceedance<-statistics.exceedance%>%
        merge(statistics.exceedance2)

      data.D1HM<-data.input%>%
        GET_DAILY_MAX(column.name='ROUNDED_VALUE',
                      data.threshold=0.75,
                      exception_threshold=70)%>%
        RENAME_COLUMN('COUNT')  #removes this column


      statistics.D1HM.percentiles<-data.D1HM%>%
        GET_PERCENTILES_NAPS(column.name='ROUNDED_VALUE_DAILY_MAX')%>%
        RENAME_COLUMN('DATE_DAY')%>%
        unique()
      #rename the percentiles columns, e.g., 10% becomes 10%(D1HM)
      column.names<-colnames(statistics.D1HM.percentiles)[grepl('%',colnames(statistics.D1HM.percentiles))]
      statistics.D1HM.percentiles<-statistics.D1HM.percentiles%>%
        RENAME_COLUMN(column.names,paste(column.names,'(D1HM)',sep=''))

      #getting the exceedance count of the daily max
      statistics.exceedance.D1HM<-data.D1HM%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE_DAILY_MAX',data.exceedancethreshold = 70)%>%
        RENAME_COLUMN('EXCEEDANCE','D1HM EXCEEDANCES >70 ppb')

      statistics.exceedance.D1HM2<-data.D1HM%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE_DAILY_MAX',data.exceedancethreshold = 75)%>%
        RENAME_COLUMN('EXCEEDANCE','D1HM EXCEEDANCES >75 ppb')

      statistics.exceedance.D1HM<-statistics.exceedance.D1HM%>%
        merge(statistics.exceedance.D1HM2)

      statistics.D1HM.99perc<-data.D1HM%>%
        GET_PERCENTILES_NAPS(column.name='ROUNDED_VALUE_DAILY_MAX',percentiles = c(99))%>%
        RENAME_COLUMN('DATE_DAY')%>%
        unique()%>%
        RENAME_COLUMN('99%','99th PERCENTILES of D1HM')

      data.output<-statistics.percentiles%>%
        merge(statistics.exceedance,all.x=TRUE)%>%
        merge(statistics.D1HM.percentiles,all.x=TRUE)%>%
        merge(statistics.exceedance.D1HM,all.x=TRUE)%>%
        merge(statistics.D1HM.99perc,all.x=TRUE)%>%
        merge(statistics.count)%>%
        RENAME_COLUMN(c('DATE','TIME'))%>%
        unique()
    }
    if (parameter %in% c('trs','h2s'))
    {
      print(paste('Creating data summary for',parameter,' year:',data.year))
      statistics.count<-GET_VALID_COUNT(data.input,day.threshold=0.75,precision=1,Q2Q3only = FALSE)

      statistics.percentiles<-data.input%>%
        RENAME_COLUMN('RAW_VALUE')%>%   #remove RAW_VALUE column
        GET_PERCENTILES_NAPS(column.name = 'ROUNDED_VALUE')
      statistics.exceedance<-data.input%>%
        GET_EXCEEDANCE_COUNT(data.exceedancethreshold = 5)%>%
        RENAME_COLUMN('EXCEEDANCE','HOURLY EXCEEDANCES >5 ppb')


      data.daily<-data.input%>%
        GET_DAILY_MEAN(column.name='ROUNDED_VALUE',precision=1,data.threshold=0.75)

      statistics.percentiles.daily<-data.daily%>%
        GET_PERCENTILES_NAPS(column.name='ROUNDED_VALUE_DAILY_MEAN')%>%
        RENAME_COLUMN(c('DATE_DAY','VALID_COUNT'))%>%
        unique()
      #rename the percentiles columns, e.g., 10% becomes 10%(D1HM)
      column.names<-colnames(statistics.percentiles.daily)[grepl('%',colnames(statistics.percentiles.daily))]
      statistics.percentiles.daily<-statistics.percentiles.daily%>%
        RENAME_COLUMN(column.names,paste(column.names,'(Day)',sep=''))


      statistics.exceedance.daily<-data.daily%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE_DAILY_MEAN',data.exceedancethreshold = 2)%>%
        RENAME_COLUMN('EXCEEDANCE','DAILY EXCEEDANCES >2 ppb')

      data.output<-statistics.percentiles%>%
        merge(statistics.exceedance,all.x=TRUE)%>%
        merge(statistics.percentiles.daily,all.x=TRUE)%>%
        merge(statistics.exceedance.daily,all.x=TRUE)%>%
        merge(statistics.count)


    }
    if (parameter =='co')
    {
      # data.input.previous<-GET_DATA_FTP(data.year,'co')%>%
      #   dplyr::mutate(INSTRUMENT=ifelse(instrument.ignore,paste(toupper(parameter),'Analyser'),as.character(INSTRUMENT)))

      data.input.previous<-importBC_data(parameter,data.year-1)

      #rename instrument name when instrument is ignored in grouping
      if (instrument.ignore)
      {
        data.input.previous <-  data.input.previous %>%
          mutate(INSTRUMENT = paste(toupper(parameter),'Analyzer'))
      }


      #make input data consistent before merge
      data.input<-data.input%>%
        dplyr::mutate(YEAR=data.year)
      data.input.previous<-data.input.previous%>%
        dplyr::mutate(YEAR=data.year-1)

      print(paste('Creating data summary for',parameter,' year:',data.year))
      statistics.count<-GET_VALID_COUNT(data.input,day.threshold=0.75,precision=4,Q2Q3only = FALSE)

      statistics.percentiles<-data.input%>%
        RENAME_COLUMN('RAW_VALUE')%>%   #remove RAW_VALUE column
        GET_PERCENTILES_NAPS(column.name = 'ROUNDED_VALUE',precision=4)
      statistics.exceedance<-data.input%>%
        GET_EXCEEDANCE_COUNT(data.exceedancethreshold = 13)%>%
        RENAME_COLUMN('EXCEEDANCE','HOURLY EXCEEDANCES >13 ppm')


      data.daily<-data.input%>%
        GET_DAILY_MEAN(column.name='ROUNDED_VALUE',precision=4,data.threshold=0.75)

      statistics.percentiles.daily<-data.daily%>%
        GET_PERCENTILES_NAPS(column.name='ROUNDED_VALUE_DAILY_MEAN',precision=4)%>%
        RENAME_COLUMN(c('DATE_DAY','VALID_COUNT'))%>%
        unique()
      #rename the percentiles columns, e.g., 10% becomes 10%(D1HM)
      column.names<-colnames(statistics.percentiles.daily)[grepl('%',colnames(statistics.percentiles.daily))]
      statistics.percentiles.daily<-statistics.percentiles.daily%>%
        RENAME_COLUMN(column.names,paste(column.names,'(Day)',sep=''))


      statistics.exceedance.8hr<-data.input%>%
        GET_ROLLING_MEAN(data.input.previous=data.input.previous,data.averaginghour = 8,precision=4)%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE_8HR',data.exceedancethreshold = 5)%>%
        RENAME_COLUMN('EXCEEDANCE','8-HR ROLLING AVG EXCEEDANCES >5 ppm')

      data.output<-statistics.percentiles%>%
        merge(statistics.exceedance,all.x=TRUE)%>%
        merge(statistics.percentiles.daily,all.x=TRUE)%>%
        merge(statistics.exceedance.8hr,all.x=TRUE)%>%
        merge(statistics.count)


    }
    if (parameter == 'aqhi')
    {

      #fix data to meet requirements for Stat check
      #prepare if data is aqhi
      #change column names to match requirements used for other parameters
      data.input<-data.input%>%
        RENAME_COLUMN('REPORTED_AQHI','ROUNDED_VALUE')%>%
        dplyr::mutate(PARAMETER='AQHI',INSTRUMENT='AQHI',
                      RAW_VALUE=ROUNDED_VALUE)



      #get AQHI details
      data.details<-data.input%>%
        dplyr::select(STATION_NAME,AQHI_AREA)%>%
        dplyr::filter(!AQHI_AREA=='')%>%
        unique()%>%
        merge(
          {data.frame(STATION_NAME=data.input$STATION_NAME)%>%
              unique()
          },all.y=TRUE
        )%>%
        dplyr::mutate(AQHI_AREA=toupper(AQHI_AREA))   #all CAPS for AQHI

      #For those with no defined AQHI Area. assume from Station_name
      data.details[is.na(data.details$AQHI_AREA),]$AQHI_AREA<-
        toupper(unlist(transpose(strsplit(data.details[is.na(data.details$AQHI_AREA),]$STATION_NAME,split=' '))[1]))

      statistics.count<-data.input%>%
        GET_VALID_COUNT(day.threshold=0.75,precision=1)%>%
        dplyr::select(c(STATION_NAME,ANNUAL_DAILY_AVG,
                        VALID_HOURS,TOTAL_HOURS,paste('%',month.abb)))%>%  #BUG, works only when all months available
        ungroup()

      data.D1HM<-data.input%>%
        dplyr::mutate(ROUNDED_VALUE=ifelse(
          round2(ROUNDED_VALUE,0)>10,
          11,ROUNDED_VALUE))%>%
        GET_DAILY_MAX(data.threshold=0)%>%
        RENAME_COLUMN('COUNT')

      statistics.D1HM.percentiles<-data.D1HM%>%
        GET_PERCENTILES_NAPS(column.name='ROUNDED_VALUE_DAILY_MAX')
      #rename the percentiles columns, e.g., 10% becomes 10%(D1HM)
      column.names<-colnames(statistics.D1HM.percentiles)[grepl('%',colnames(statistics.D1HM.percentiles))]
      statistics.D1HM.percentiles<-statistics.D1HM.percentiles%>%
        RENAME_COLUMN(column.names,paste(column.names,'(D1HM)',sep=''))%>%
        RENAME_COLUMN(c('EMS_ID','INSTRUMENT'))


      #create a blank statistics
      month.template<-data.D1HM%>%
        dplyr::select(STATION_NAME)%>%
        unique()%>%
        merge(data.frame(MONTH=month.abb))
      #get the number of AQHI>=7 for each month
      #this is the number of days in a month where AQHI>=7
      statistics.exceedance<-data.D1HM%>%
        dplyr::mutate(MONTH=month.abb[month(DATE_DAY)],YEAR=year(DATE_DAY))%>%
        dplyr::filter(YEAR==data.year)%>% #removes possibility that there may be data from wrong year
        dplyr::filter(ROUNDED_VALUE_DAILY_MAX>= 7)%>%
        dplyr::group_by(STATION_NAME,MONTH)%>%
        dplyr::summarise(COUNT=n())%>%
        merge(month.template,all.y=TRUE)%>%
        dplyr::mutate(COUNT=ifelse(is.na(COUNT),0,COUNT))%>%
        GET_PIVOT_TABLE(columnname.category='MONTH',
                        columnname.value='COUNT',
                        columnname.levels=month.abb)%>%
        ungroup()%>%
        RENAME_COLUMN(c(month.abb),c(paste(month.abb,'EXC',sep='_')))

      statistics.max<-data.D1HM%>%
        dplyr::mutate(MONTH=month.abb[month(DATE_DAY)],YEAR=year(DATE_DAY))%>%
        dplyr::filter(YEAR==data.year)%>% #removes possibility that there may be data from wrong year
        dplyr::group_by(STATION_NAME,MONTH)%>%
        dplyr::summarise(AVE=round2(max(ROUNDED_VALUE_DAILY_MAX),1))%>%
        merge(month.template,all.y=TRUE)%>%
        GET_PIVOT_TABLE(columnname.category='MONTH',
                        columnname.value='AVE',
                        columnname.levels=month.abb)%>%
        ungroup()%>%
        RENAME_COLUMN(c(month.abb),c(paste(month.abb,'MAX',sep='_')))

      statistics.median<-data.D1HM%>%
        dplyr::mutate(MONTH=month.abb[month(DATE_DAY)],YEAR=year(DATE_DAY))%>%
        dplyr::filter(YEAR==data.year)%>% #removes possibility that there may be data from wrong year
        dplyr::group_by(STATION_NAME,MONTH)%>%
        dplyr::summarise(AVE=round2(median(ROUNDED_VALUE_DAILY_MAX),1))%>%
        merge(month.template,all.y=TRUE)%>%
        GET_PIVOT_TABLE(columnname.category='MONTH',
                        columnname.value='AVE',
                        columnname.levels=month.abb)%>%
        ungroup()%>%
        RENAME_COLUMN(c(month.abb),c(paste(month.abb,'MEDIAN',sep='_')))

      statistics.mean<-data.D1HM%>%
        dplyr::mutate(MONTH=month.abb[month(DATE_DAY)],YEAR=year(DATE_DAY))%>%
        dplyr::filter(YEAR==data.year)%>% #removes possibility that there may be data from wrong year
        dplyr::group_by(STATION_NAME,MONTH)%>%
        dplyr::summarise(AVE=round2(median(ROUNDED_VALUE_DAILY_MAX),1))%>%
        merge(month.template,all.y=TRUE)%>%
        GET_PIVOT_TABLE(columnname.category='MONTH',
                        columnname.value='AVE',
                        columnname.levels=month.abb)%>%
        ungroup()%>%
        RENAME_COLUMN(c(month.abb),c(paste(month.abb,'MEAN',sep='_')))

      data.output<-data.details%>%
        merge(statistics.count)%>%
        merge(statistics.D1HM.percentiles)%>%
        merge(statistics.exceedance)%>%
        merge(statistics.max)%>%
        #merge(statistics.mean)%>%   #removed mean  because of the aqhi not nominal, caps at 11
        merge(statistics.median)


    }
    if (!parameter %in% c('no2','pm25','pm10','o3','co','so2','trs','h2s','aqhi'))
    {
      print(paste('Creating data summary for', parameter, 'year:',data.year))
      statistics.count<-GET_VALID_COUNT(data.input,day.threshold=0.75,precision=1)
      statistics.percentiles<-data.input%>%
        RENAME_COLUMN('RAW_VALUE')%>%   #remove RAW_VALUE column
        GET_PERCENTILES_NAPS(column.name = 'ROUNDED_VALUE')
      statistics.exceedance<-data.input%>%
        GET_EXCEEDANCE_COUNT(column.name='ROUNDED_VALUE',data.exceedancethreshold = 100)%>%
        RENAME_COLUMN('EXCEEDANCE','HOURLY EXCEEDANCES >100 ppb')

      data.output<-statistics.percentiles%>%
        merge(statistics.count)

    }
    rm(data.input)   #release memory

    #
    # data.stationmeta<-listBC_stations()%>%
    #   dplyr::select(STATION_NAME,EMS_ID)%>%
    #   dplyr::mutate(NAPS_ID=as.character(NAPS_ID))
    #
    # data.output<-merge(data.stationmeta,data.output,all.y=TRUE)%>%
    #   RENAME_COLUMN(c('DATE','TIME'))%>%
    #   unique()#combine with the station meta data



  }
  data.output<-data.output%>%
    dplyr::mutate(DATE_YEAR=data.year)
  return(data.output)
}

#' Counts the number of valid hours or days
#'
#' This function counts the number of valid hours
#'
#' @param data.hourly hourly data
#' @param day.threshold threshold for data captire
#' @param precision precision of output
#' @param Q2Q3 only
#' GET_VALID_COUNT()
GET_VALID_COUNT<-function(data.hourly,day.threshold=0.75,precision=1,Q2Q3only=FALSE)
  #Counts the number of valid hours and days
  #this will group the counts based on STATION_NAME,PARAMETER,INSTRUMENT
  #InstrumentDependent defines if it will take Instrument in consideration
  #Recommended for PM, where SHARP and TEOM results are separated
  #return the number of valid hours, valid days, 1 hr average, daily hourly average
  #Q2Q3 only option for those that combines Q2 and Q3

  #bug 2019-02-15
  #H2S Pine River Gas Plant 2010 %Dec is too high, 3000%!
  #if data comes from the large FTP dataset_

{
  #debug initializaion------
  # data.hourly<-data.source #GET_DATA_FTP_LARGE('h2s')%>%
  # #dplyr::filter(STATION_NAME=='Pine River Gas Plant')%>%
  # dplyr::filter(grepl('2010',DATE))
  #

  # day.threshold=0.75
  # precision=1
  # Q2Q3only=FALSE
  #end of debug

  #group.parameter=c('STATION_NAME','PARAMETER','INSTRUMENT')
  #count the total hours and days in months and quarters
  print('getting the valid counts')
  data.hourly<-dplyr::ungroup(data.hourly)
  data.totals<-data.hourly%>%
    dplyr::mutate(DATE_TEMP=as.POSIXct(as.character(DATE_PST),tz='utc'))%>%
    dplyr::summarise(DATE_START=min(DATE_TEMP,na.rm = TRUE),DATE_END=max(DATE_TEMP, na.rm=TRUE))
  # dplyr::mutate(DATE_START=as.POSIXct(paste(DATE_START,'-01-01 01:00',sep=''),tz='utc'),
  #        DATE_END=as.POSIXct(paste(DATE_END,'-01-01',sep=''),tz='utc'))

  data.totals<-data.totals %>%
    merge(data.frame(DATE=seq.POSIXt(from=data.totals$DATE_START[1],to=data.totals$DATE_END[1],by='hour')),all=TRUE)%>%
    dplyr::mutate(TOTAL_HOURS=n(),DATE_TEMP=DATE-3600)%>%
    dplyr::mutate(DATE_DAY=format(as.POSIXct(DATE_TEMP,tz='utc'),'%Y-%m-%d'),
                  DATE_YEAR=format(as.POSIXct(DATE_TEMP,tz='utc'),'%Y'),
                  DATE_MONTH=month.abb[month(as.character(DATE_DAY))],
                  QUARTER=as.integer((month(as.character(DATE_DAY))-1)/3)+1)%>%
    dplyr::group_by(DATE_START,DATE_END,DATE_YEAR,QUARTER,DATE_MONTH,DATE_DAY,TOTAL_HOURS)%>%
    dplyr::summarise()%>%
    dplyr::group_by(DATE_YEAR)%>%
    dplyr::mutate(TOTAL_DAYS=n())%>%
    dplyr::group_by(DATE_START,DATE_END,DATE_YEAR,QUARTER,DATE_MONTH,TOTAL_HOURS,TOTAL_DAYS)%>%
    dplyr::summarise(DAYS_in_MONTH=n())%>%
    dplyr::group_by(DATE_YEAR,QUARTER)%>%
    dplyr::mutate(DAYS_in_QUARTER=sum(DAYS_in_MONTH))%>%
    dplyr::ungroup()


  #retrieving the basic counts, and hourly mean, and daily mean of hourly data
  data.temp<-data.hourly%>%
    dplyr::mutate(DATE_TEMP=as.POSIXct(as.character(DATE_PST),tz='utc')-3600) %>% #shift one hour for Time ending
    dplyr::mutate(DATE_YEAR=format(as.POSIXct(DATE_TEMP,tz='utc'),'%Y'),
                  DATE_DAY=format(as.POSIXct(DATE_TEMP,tz='utc'),'%Y-%m-%d'))%>%
    RENAME_COLUMN('DATE_TEMP')%>%
    dplyr::filter(!RAW_VALUE=='')%>%
    dplyr::filter(RAW_VALUE>-999)%>%   #remove blanks and -999
    dplyr::mutate(RAW_VALUE=as.numeric(RAW_VALUE),ROUNDED_VALUE=as.numeric(ROUNDED_VALUE))%>%  #convert these to numbers
    dplyr::group_by(STATION_NAME,INSTRUMENT,DATE_YEAR,PARAMETER,EMS_ID) %>%
    dplyr::mutate(VALID_HOURS=n(),TOTAL_HOURS=data.totals$TOTAL_HOURS[1],
                  ANNUAL_AVG_1HR=round2(mean(ROUNDED_VALUE,na.rm=TRUE),precision)) %>%
    dplyr::ungroup()%>%
    dplyr::group_by(STATION_NAME,INSTRUMENT,DATE_YEAR,DATE_DAY,PARAMETER,EMS_ID,ANNUAL_AVG_1HR) %>%
    dplyr::mutate(VALID_HOURS_PER_DAY=n(),ROUNDED_VALUE_DAY=mean(ROUNDED_VALUE,na.rm=TRUE))%>%
    dplyr::filter(VALID_HOURS_PER_DAY>=day.threshold*24)%>%
    dplyr::group_by(STATION_NAME,INSTRUMENT,DATE_YEAR,DATE_DAY,PARAMETER,EMS_ID,VALID_HOURS,TOTAL_HOURS,ANNUAL_AVG_1HR,ROUNDED_VALUE_DAY) %>%
    dplyr::summarise()%>%
    dplyr::ungroup()%>%
    dplyr::group_by(STATION_NAME,INSTRUMENT,DATE_YEAR,PARAMETER,EMS_ID,VALID_HOURS,TOTAL_HOURS,ANNUAL_AVG_1HR) %>%
    dplyr::mutate(VALID_DAYS=n(),TOTAL_DAYS=data.totals$TOTAL_DAYS[1],ANNUAL_DAILY_AVG=round2(mean(ROUNDED_VALUE_DAY),precision))%>%
    dplyr::mutate(MONTH=month.abb[month(as.character(DATE_DAY))],QUARTER=as.integer((month(as.character(DATE_DAY))-1)/3)+1)%>%
    dplyr::ungroup()%>%
    dplyr::group_by(STATION_NAME,INSTRUMENT,DATE_YEAR,PARAMETER,EMS_ID,
                    VALID_HOURS,TOTAL_HOURS,ANNUAL_AVG_1HR,
                    VALID_DAYS,TOTAL_DAYS,ANNUAL_DAILY_AVG,MONTH,QUARTER) %>%
    dplyr::summarise(VALID_in_MONTH=n())%>%
    dplyr::ungroup()%>%
    dplyr::group_by(STATION_NAME,INSTRUMENT,DATE_YEAR,PARAMETER,EMS_ID,
                    VALID_HOURS,TOTAL_HOURS,ANNUAL_AVG_1HR,
                    VALID_DAYS,TOTAL_DAYS,ANNUAL_DAILY_AVG,QUARTER) %>%
    dplyr::mutate(VALID_in_QUARTER=sum(VALID_in_MONTH))%>%
    dplyr::ungroup()


  #insert percentage values
  temp.result<-NULL
  for (i in 1:nrow(data.temp))
  {
    temp<-data.temp[i,]%>%
      dplyr::mutate(VALID_in_MONTH_PERC=round2(100*VALID_in_MONTH/data.totals$DAYS_in_MONTH[data.totals$DATE_MONTH==MONTH][1],precision),
                    VALID_in_QUARTER_PERC=round2(100*VALID_in_QUARTER/data.totals$DAYS_in_QUARTER[data.totals$QUARTER==QUARTER][1],precision)
      )
    temp.result<-rbind(temp.result,temp)
  }
  data.temp<-temp.result
  #pivot the table by month
  data.temp.month<-data.temp%>%
    dplyr::ungroup()%>%
    dplyr::select(STATION_NAME,INSTRUMENT,DATE_YEAR,VALID_in_MONTH,MONTH) %>%
    GET_PIVOT_TABLE('MONTH','VALID_in_MONTH',month.abb)

  data.temp.month.perc<-data.temp%>%
    dplyr::ungroup()%>%
    dplyr::select(STATION_NAME,INSTRUMENT,DATE_YEAR,VALID_in_MONTH_PERC,MONTH) %>%
    GET_PIVOT_TABLE('MONTH','VALID_in_MONTH_PERC',month.abb)%>%
    RENAME_COLUMN(month.abb,paste('%',month.abb))


  data.temp.quarter<-data.temp%>%
    dplyr::ungroup()%>%
    dplyr::select(STATION_NAME,INSTRUMENT,DATE_YEAR,VALID_in_QUARTER,QUARTER)%>%
    GET_PIVOT_TABLE('QUARTER','VALID_in_QUARTER',seq(1:4))%>%
    RENAME_COLUMN(c('1','2','3','4'),c('Q1','Q2','Q3','Q4'))

  data.temp.quarter.perc<-data.temp%>%
    dplyr::ungroup()%>%
    dplyr::select(STATION_NAME,INSTRUMENT,DATE_YEAR,VALID_in_QUARTER_PERC,QUARTER) %>%
    GET_PIVOT_TABLE('QUARTER','VALID_in_QUARTER_PERC',seq(1:4))%>%
    RENAME_COLUMN(c('1','2','3','4'),c('% Q1','% Q2','% Q3','% Q4'))

  #for Q2+Q3 option like on ozone
  if (Q2Q3only)
  {
    data.temp.quarter<-data.temp.quarter%>%
      dplyr::mutate(Q2Q3=Q2+Q3)%>%
      RENAME_COLUMN(c('Q1','Q2','Q3','Q4'))%>%
      RENAME_COLUMN('Q2Q3','Q2+Q3')

    data.totals.Q2<-data.totals[data.totals$QUARTER==2,]$DAYS_in_QUARTER[1]
    data.totals.Q3<-data.totals[data.totals$QUARTER==3,]$DAYS_in_QUARTER[1]

    data.temp.quarter.perc<-data.temp.quarter.perc%>%
      RENAME_COLUMN(c('% Q2','% Q3'),c('Q2_PERC','Q3_PERC'))%>%
      dplyr::mutate(Q2Q3_PERC=round2((Q2_PERC*data.totals.Q2+Q3_PERC*data.totals.Q3)/
                                       (data.totals.Q2+data.totals.Q3),precision))%>%
      RENAME_COLUMN(c('% Q1','% Q4','Q2_PERC','Q3_PERC')) %>%   #delete these columns
      RENAME_COLUMN('Q2Q3_PERC','% (Q2+Q3)')

  }


  data.temp<-data.temp%>%
    dplyr::ungroup()%>%
    RENAME_COLUMN(c('MONTH','VALID_in_MONTH','VALID_in_MONTH_PERC',
                    'QUARTER','VALID_in_QUARTER','VALID_in_QUARTER_PERC'))%>%
    unique()%>%
    merge(data.temp.month)%>%
    merge(data.temp.month.perc)%>%
    merge(data.temp.quarter)%>%
    merge(data.temp.quarter.perc)


  return(data.temp)
}

#' Created before pivot_wider and pivot_longer became available
GET_PIVOT_TABLE<-function(data.input,columnname.category,columnname.value,columnname.levels=NULL)
{


  #this will pivot the table
  #in order to properly sort, columnname.levels must contain the order of all items listed
  #   #------debug-----------------
  # data.input<-data.temp
  # # # columnname.levels=NULL
  # columnname.category<-'MONTH'
  # columnname.value<-'VALID_in_MONTH'
  # columnname.levels=month.abb
  # data.input<-data.input%>%
  # ungroup()%>%
  # select('STATION_NAME','INSTRUMENT','DATE_YEAR','MONTH','VALID_in_MONTH')
  #----------------------------
  print('pivoting the table')
  data.input<-data.input%>%
    RENAME_COLUMN(columnname.category,'TEMP_CATEGORY_COLUMN')%>%
    RENAME_COLUMN(columnname.value,'TEMP_VALUE_COLUMN')%>%
    arrange(TEMP_CATEGORY_COLUMN)
  temp.result<-data.input%>%
    RENAME_COLUMN(c('TEMP_CATEGORY_COLUMN','TEMP_VALUE_COLUMN'))%>%   #remove the category columns
    unique()

  #define the soring parameter
  if (is.null(columnname.levels))
  {
    category<-unique(data.input$TEMP_CATEGORY_COLUMN)
  } else
  {
    category<-unique(columnname.levels)
  }

  for (i in category)
  {
    print(paste('Pivoting the table for:',columnname.category,'Scanning:',i))
    temp<-data.input%>%
      dplyr::filter(TEMP_CATEGORY_COLUMN==i)%>%
      dplyr::mutate(TEMP_VALUE_COLUMN_RESULT=TEMP_VALUE_COLUMN)%>%
      RENAME_COLUMN('TEMP_VALUE_COLUMN_RESULT',i)%>%
      RENAME_COLUMN(c('TEMP_CATEGORY_COLUMN','TEMP_VALUE_COLUMN'))
    if (nrow(temp)>0)
    {temp.result<-merge(temp.result,temp,all.x=TRUE)}

  }
  return(unique(temp.result))
}

GET_PERCENTILES_NAPS<-function(data.hourly,column.name='ROUNDED_VALUE',percentiles=
                            c(0,10,25,50,75,90,95,98,99,99.5,99.9,100),precision=1)

{
  #---DESCRIPTION-----------
  #retrieves the percentile results from the data.hourly$ROUNDED_VALUE
  #2019-03-26 now uses percentile ranking instead of quantile


  #debug initialization
  #  data.hourly<-GET_DATA_FTP(2018,'so2')
  # #
  # data.hourly<-data.input
  # column.name='ROUNDED_VALUE'
  # percentiles=99
  # # # #
  # # # column.name='ROUNDED_VALUE'
  # percentiles=
  # c(0,10,25,50,75,90,95,98,99,99.5,99.9,100)
  # precision=1
  #end of debug
print('getting percentiles NAPS method')
  column.name<-toupper(column.name)
  data.hourly<-data.hourly%>%
    RENAME_COLUMN(column.name,'TEMP_EVALUATE_COLUMN')%>%  #rename the column
    dplyr::group_by(STATION_NAME,EMS_ID,INSTRUMENT)%>%
    dplyr::filter(!is.na(TEMP_EVALUATE_COLUMN))%>%
    dplyr::arrange(desc(TEMP_EVALUATE_COLUMN))%>%
    dplyr::mutate(ORDER=row_number())%>%
    dplyr::ungroup()%>%
    RENAME_COLUMN(ifelse(column.name=='RAW_VALUE',
                         'ROUNDED_VALUE','RAW_VALUE'))  #remove unused data column

  #build up result from this datatable
  data.result<-data.hourly%>%
    dplyr::select(STATION_NAME,EMS_ID,INSTRUMENT)%>%
    unique()

  for (i in percentiles)
  {
    print(paste('Getting the percentile:',i))

    #algorithm from CCME
    data.hourly.temp<-data.hourly%>%
      dplyr::group_by(STATION_NAME,EMS_ID,INSTRUMENT)%>%
      dplyr::mutate(N_Minus_i=n()-as.integer(n()*i/100))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(N_Minus_i=ifelse(N_Minus_i<1,1,N_Minus_i))%>%  #correction for 100 percentile
      dplyr::filter(N_Minus_i==ORDER)%>%
      unique()%>%
      RENAME_COLUMN(c('ORDER','N_Minus_i'))%>% #remove scratch columns
      dplyr::group_by(STATION_NAME,EMS_ID,INSTRUMENT)%>%
      dplyr::summarise(TEMP_EVALUATE_COLUMN=max(TEMP_EVALUATE_COLUMN))%>%   #merges same stations
      RENAME_COLUMN('TEMP_EVALUATE_COLUMN',paste(i,'%',sep=''))

    #print(paste('Result has',nrow(data.hourly.temp),'rows'))
    #code below from using percent_rank function, obsolete 2019-03-27
    # data.hourly.temp<-data.hourly%>%
    #   dplyr::mutate(DELTA_PERCENTAGE=(PERC_RANK-i))%>%
    #   dplyr::filter(DELTA_PERCENTAGE>=0)%>%
    #   dplyr::group_by(STATION_NAME,EMS_ID,INSTRUMENT)%>%
    #   dplyr::mutate(MIN_DELTA_PERCENTAGE=min(DELTA_PERCENTAGE,na.rm=TRUE))%>%
    #   dplyr::filter(DELTA_PERCENTAGE==MIN_DELTA_PERCENTAGE)%>%
    #   dplyr::ungroup()%>%
    #   dplyr::group_by(STATION_NAME,EMS_ID,INSTRUMENT)%>%  #regroup to eliminate all other duplicate entries
    #   dplyr::summarise(TEMP_PERCENTILE=max(TEMP_EVALUATE_COLUMN))%>%  #if exactly the same delta, higher percentile used
    #   RENAME_COLUMN('TEMP_PERCENTILE',paste(i,'%',sep=''))%>%
    #   dplyr::ungroup()
    data.result<-data.result%>%
      merge(data.hourly.temp)
  }
  data.result<-data.result%>%
    RENAME_COLUMN(c('DATE_PST','TEMP_EVALUATE_COLUMN','DATE','TIME'))%>%
    unique()
  return(data.result)
}


GET_DAILY_MEAN<-function(data.input,column.date='DATE_PST',column.name='ROUNDED_VALUE',precision=1,data.threshold=0.75)
  #get the daily mean of the hourly values
  #column.name define what will be averaged

  #----debug initialize---
  # data.input<-GET_DATA_FTP(2016,'pm10')
  # column.date<-'DATE_PST'
  # column.name<-'ROUNDED_VALUE'
{
  print('Getting daily mean')
  data.input<-data.input%>%
    RENAME_COLUMN(column.date,'TEMP_DATE_PST')%>%
    RENAME_COLUMN(column.name,'TEMP_VALUE_COLUMN')%>%
    dplyr::mutate(TEMP_DATE=as.POSIXct(as.character(TEMP_DATE_PST),tz='utc')-3600)%>%
    dplyr::mutate(DATE_DAY=format(TEMP_DATE,'%Y-%m-%d'),TEMP_VALUE_COLUMN=as.numeric(TEMP_VALUE_COLUMN))%>%
    dplyr::filter(!TEMP_VALUE_COLUMN=='')%>%
    dplyr::filter(TEMP_VALUE_COLUMN>-999)%>%
    dplyr::group_by(STATION_NAME,EMS_ID,INSTRUMENT,DATE_DAY)%>%
    dplyr::summarise(VALID_COUNT=n(),TEMP_MEAN_COLUMN=round2(mean(TEMP_VALUE_COLUMN),precision))%>%
    dplyr::ungroup()%>%
    dplyr::filter(VALID_COUNT>=data.threshold*24)%>%
    RENAME_COLUMN('TEMP_MEAN_COLUMN',paste(column.name,'_DAILY_MEAN',sep=''))

  return(data.input)

}

GET_EXCEEDANCE_COUNT<-function(data.input,column.name='ROUNDED_VALUE',data.exceedancethreshold)
{
  #PURPOSE: This will return the number of exceedance to the value defined in column.name
  #designed for just one threshold value in data.exceedancethreshold

  #--debug initialization---
  #   data.input<-GET_DATA_FTP(2016,'pm25')
  # column.name='ROUNDED_VALUE'
  # data.exceedancethreshold=25
  #   data.input<-data.D1HM
  # column.name<-'ROUNDED_VALUE_DAILY_MAX'
  # data.exceedancethreshold=70

print('Getting exceedance counts')
  data.complete<-data.input%>%
    dplyr::select(STATION_NAME,EMS_ID,INSTRUMENT)%>%
    unique()
  data.count<-data.input%>%
    dplyr::ungroup()%>%
    RENAME_COLUMN(column.name,'TEMP_VALUE_COLUMN')%>%
    dplyr::filter(!is.na(TEMP_VALUE_COLUMN))%>%
    dplyr::filter(TEMP_VALUE_COLUMN>-999)%>%
    dplyr::filter(as.numeric(TEMP_VALUE_COLUMN)>data.exceedancethreshold)%>%
    dplyr::group_by(STATION_NAME,EMS_ID,INSTRUMENT)%>%
    dplyr::summarise(EXCEEDANCE=n())%>%
    merge(data.complete,all.y=TRUE)%>%
    dplyr::mutate(EXCEEDANCE=ifelse(!is.na(EXCEEDANCE),EXCEEDANCE,0))

  return(data.count)
}



GET_DAILY_MAX<-function(data.input,column.name='ROUNDED_VALUE',column.date='DATE_PST',
                        data.threshold=0.75,exception_threshold=9999999)
  #PURPOSE: Gets the daily maximum from the spciefied column name and using the specified column date
  #exception_threshold, if defined(!=9999999), then

{

  #---debug initialization------
  #  data.input<-statistics.D8HM_percentiles#GET_DATA_FTP(2016,'o3')
  # data.input<-GET_DATA_FTP(2017,'so2')
  #
  # data.input<-data.input%>%
  #   dplyr::filter(STATION_NAME=='Trail Butler Park')
  # exception_threshold=70
  # #  column.name='ROUNDED_VALUE_8HR'
  # column.name='ROUNDED_VALUE'
  #  column.date='DATE_PST'
  #  data.threshold=0.75
  # #


  data.output<-data.input%>%
    RENAME_COLUMN(column.name,'TEMP_VALUE_COLUMN')%>%
    RENAME_COLUMN(column.date,'TEMP_DATE_PST')%>%
    dplyr::mutate(REMOVE=1)%>%   #default is remove all datasets (==1)
    dplyr::mutate(DATE_DAY=format(as.POSIXct(as.character(TEMP_DATE_PST),tz='utc')-3600,'%Y-%m-%d'))%>%
    dplyr::mutate(TEMP_VALUE_COLUMN=as.numeric(TEMP_VALUE_COLUMN))%>%
    dplyr::filter(!is.na(TEMP_VALUE_COLUMN))%>%
    dplyr::filter(TEMP_VALUE_COLUMN>-999)%>%
    dplyr::mutate(REMOVE=ifelse(as.numeric(TEMP_VALUE_COLUMN) > exception_threshold,0,REMOVE))%>%
    dplyr::group_by(STATION_NAME,INSTRUMENT,EMS_ID,DATE_DAY)%>%
    dplyr::mutate(TEMP_DAILY_MAX=max(TEMP_VALUE_COLUMN),TEMP_VALID_COUNT=n())%>%
    dplyr::ungroup()%>%
    dplyr::mutate(REMOVE=ifelse(TEMP_VALID_COUNT >= data.threshold*24,0,REMOVE))%>% #keep if data capture requirements
    dplyr::filter(!REMOVE==1)%>%   #remove when remove==1
    dplyr::select(STATION_NAME,INSTRUMENT,EMS_ID,DATE_DAY,
                  TEMP_DAILY_MAX,TEMP_VALID_COUNT)%>%

    RENAME_COLUMN('TEMP_DAILY_MAX',paste(column.name,'_DAILY_MAX',sep=''))%>%
    RENAME_COLUMN('TEMP_VALID_COUNT','COUNT')%>%
    unique()

  return(data.output)

}


#' Get negative and flat data reports
#'
#' @param parameter default is NULL to include all relevant parameters
#'                  or vector listing all parameters
#' @param year is the year to check for negative and flat,
#'             if NULL,
#' @param file.result is the location where resulting file is to be saved
#' @param exclude_MVRD means MVRD data is excluded
GET_NEGATIVE_FLAT <- function(parameter = NULL, YEAR = NULL,
                              file.result = 'negative_flats.csv',
                              exclude_MVRD = TRUE)
{
  if (0)
  {
    parameter = c('no2')
    file.result = 'negative_flats.csv'
    YEAR <- NULL
    exclude_MVRD = TRUE
    source('envairstatfunctions.R')
    source('importbc_data.R')
    source('envairfunctions.R')
  }

  #setup----
  RUN_PACKAGE(c('stringi','dplyr','plyr','data.table'))

  #define standard deviation threshold values for a flat
  df_flat_threshold <- tribble(
    ~parameter,~threshold,
    'CO',0.000001,
    'PM25',0.001,
    'PM10',0.001,
    'NO2',0.001,
    'NO',0.001,
    'NOx',0.001,
    'O3',0.001,
    'SO2',0.001,
    'TRS',0.001,
    'H2S',0.001,
    'WSPD_SCLR',0.001,
    'WSPD_VECT',0.001,
    'WDIR_UVEC',1,
    'WDIR_VECT',1
  )

  #specify all relevant parameters if null
  if (is.null(parameter))
  {
    parameter <- list_parameters()
    parameter <- parameter[!grepl('aqhi',parameter,ignore.case = TRUE)]
  }

  #specify the year after latest valid year if NULL
  if (is.null(YEAR))
  {
    #identify the latest validation cycle data
    data.source<-'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/'
    temp<-as.character(unlist(strsplit(RCurl::getURL(data.source,dirlistonly=TRUE),split='\r\n')))
    temp<-temp[nchar(temp)==4] #get only 4-digit folders
    validation.lastvalidationcycle<-max(as.numeric(temp),na.rm = TRUE)
    YEAR <- validation.lastvalidationcycle + 1

  }
  print(paste('Getting flats and negatives for',YEAR))
  df_result <- NULL
  for (param in parameter)
  {
    print(paste('Processing:',param))
    data.input <- GET_PARAMETER_DATA(param,year.start = YEAR,year.end = YEAR)
    if (exclude_MVRD)
    {
      #exclude Metro Vancouver stations
      data.input <- data.input%>%
        dplyr::filter(toupper(OWNER) != 'MVRD')
    }

    #get negative values-----

    #group together, create summary of negative
    data.input.negative <- data.input[data.input$ROUNDED_VALUE<0,] %>%
      dplyr::filter(!is.na(DATE_PST),
                    tolower(PARAMETER) != 'temp_mean')


    print(paste('Processing negative values. There are',nrow(data.input.negative),'results'))

    if (nrow(data.input.negative) >0)
    {
      df_result <- df_result %>%
        plyr::rbind.fill(
          data.input.negative%>%
            mutate(FLAG = 'NEGATIVE')
        )
    }


    #flat data analysis------
    df_flat_threshold_ <- df_flat_threshold %>%
      dplyr::filter(toupper(parameter) == toupper(param))
    if (nrow(df_flat_threshold_)>0)
    {
      dbl_threshold <- df_flat_threshold_$threshold[1]
    } else
    {
      #if not defined, it has to be zero
      dbl_threshold <- 0
    }
    data.input.flat <- data.input%>%
      dplyr::group_by(STATION_NAME_FULL,INSTRUMENT) %>%
      dplyr::arrange(DATE_PST)%>%
      mutate(RAW_01=lead(RAW_VALUE,1),
             RAW_02=lead(RAW_VALUE,2),
             RAW_03=lead(RAW_VALUE,3),
             RAW_04=lead(RAW_VALUE,4)) %>%
      dplyr::group_by(STATION_NAME_FULL,DATE_PST,INSTRUMENT) %>%
      dplyr::mutate(StDev = sd(c(RAW_VALUE,RAW_01,RAW_02,RAW_03,RAW_04),
                               na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(StDev <= dbl_threshold)
    print(paste('Processing flat values. There are',nrow(data.input.flat),'results'))
    if (nrow(data.input.flat) >0)
    {
      df_result <- df_result %>%
        plyr::rbind.fill(
          data.input.flat%>%
            select(-RAW_01,-RAW_02,-RAW_03,-RAW_04) %>%
            mutate(FLAG = 'FLAT')
        )
    }


    #summarize the flat and negative result--------
    #calculate how many negatives and flat events,
    try(
      df_result <- df_result %>%
        ungroup() %>%
        unique()%>%
        group_by(STATION_NAME_FULL,PARAMETER,INSTRUMENT,FLAG) %>%
        dplyr::arrange(DATE_PST) %>%
        dplyr::mutate(DATE_PST_next =  lead(DATE_PST)) %>%
        dplyr::mutate(DATE_diff = as.numeric(difftime(ymd_hms(as.character(DATE_PST_next)),ymd_hms(as.character(DATE_PST)),units = 'hours'))) %>%
        dplyr::mutate(DATE_diff = ifelse(is.na(DATE_diff),999,DATE_diff)) %>%
        dplyr::ungroup()%>%
        # #find which is first instance of event
        group_by(STATION_NAME_FULL,PARAMETER,INSTRUMENT,FLAG) %>%
        dplyr::arrange(DATE_PST) %>%

        #identify which is the end of an event, if the next item in sequence>1, but previous was 1, then end of event
        #these events have index = -999

        dplyr::mutate (index = ifelse((DATE_diff>1 & lag(DATE_diff) == 1),
                                      999,1)) %>%
        #remove events that are not the beginning or end,
        #these will have index of -1
        dplyr::mutate (index = ifelse((DATE_diff == 1 & lag(DATE_diff) == 1),
                                      -1,index))%>%
        #the remaining NA means these are at the  beginning of each group of STATION, PARAMETER,INSTRUMENT,FLAG
        dplyr::mutate(index = ifelse(is.na(index),1,index)) %>%
        #remove the middle values, non-start/ending
        dplyr::filter(!(DATE_diff ==1 & index == -1)) %>%
        #index == 0 means start of multi-hour event
        #index == 1 means single hour event
        dplyr::mutate(index = ifelse(DATE_diff != 999 & lead(index == 999),0,index)) %>%
        #create a start and stop time for each event
        dplyr::mutate(DATE_END = ifelse(index ==0, lead(DATE_PST),DATE_PST)) %>%
        dplyr::filter(index != 999) %>%
        #for flats, add 4 hours to duration
        dplyr::mutate(DATE_END = ifelse(FLAG == 'FLAT',
                                        as.character(ymd_hms(DATE_END) + 4*3600, format = '%Y-%m-%d %H:%M:%S'),
                                        DATE_END)) %>%
        dplyr::mutate(DURATION_hrs = as.numeric(difftime(ymd_hms(DATE_END),
                                                         ymd_hms(DATE_PST),
                                                         units = 'hours')) + 1) %>%
        dplyr::ungroup()%>%
        dplyr::select(DATE_PST,DATE_END,DURATION_hrs,STATION_NAME_FULL,PARAMETER,INSTRUMENT,FLAG,ROUNDED_VALUE)

    )

  }

  #save to file
  if (!is.null(df_result))
  {
    try(data.table::fwrite(df_result,file.result))
  } else
    try(data.table::fwrite(tibble(STATION_NAME = 'NONE',PARAMETER = toupper(param)),file.result))

}


#' Retrieve the nth highest number
#'
#' @param data.input is the dataframe input
#' @param column.name is the name of the column where the nth highest value is retrieved
#' @param data.order is the order of data
GET_nTH_HIGHEST<-function(data.input,column.name='ROUNDED_VALUE',data.order=4)
  #PURPOSE: Retrieves the nth highest value

{
  #----debig initialization-----
  # data.input<-GET_DATA_FTP(2015,'o3')
  # data.order=4
  # column.name='ROUNDED_VALUE'
  data.output<-data.input%>%
    RENAME_COLUMN(column.name,'TEMP_VALUE_COLUMN')%>%
    dplyr::select(STATION_NAME,INSTRUMENT,EMS_ID,TEMP_VALUE_COLUMN)%>%
    dplyr::mutate(TEMP_COUNT=1,TEMP_VALUE_COLUMN=as.numeric(TEMP_VALUE_COLUMN))%>%
    dplyr::arrange(STATION_NAME,INSTRUMENT,EMS_ID,desc(TEMP_VALUE_COLUMN))%>%
    dplyr::group_by(STATION_NAME,INSTRUMENT,EMS_ID)%>%
    dplyr::mutate(TEMP_INDEX=cumsum(TEMP_COUNT))%>%
    dplyr::filter(TEMP_INDEX==data.order)%>%
    RENAME_COLUMN(c('TEMP_COUNT','TEMP_INDEX'))%>%   #remove counter columns
    RENAME_COLUMN('TEMP_VALUE_COLUMN',column.name)%>%#rename column back to orig
    return()

}



