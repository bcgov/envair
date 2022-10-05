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

# The functions here are meant to create functions and data to prepare shiny data

#' Create a bar graph of the NPRI
#'
#' @param pollutant
#' @param categorytype is either source, sector, or subsector. Default is source
#' @param URL is the ECCC URL for the NPRI
#' @param output is the output type of either 'basic' or 'plotly'
#'
#' @export
plot_npri <- function(pollutant,categorytype = 'Source',URL=NULL,output = 'basic') {
  if (0) {
    pollutant <- c('pm25')
    categorytype <- 'Source'
    output = 'basic'
    URL=NULL
  }

  require(ggplot2)
  df_npri <- get_npri(pollutant = pollutant, categorytype = categorytype, URL = URL)%>%
    dplyr::rename(groupingcolumn= categorytype)

  #change pollutant for labelling purposes
  label <- pollutant
  if (grepl('pm25',pollutant,ignore.case = TRUE)) {
    label <- expression(PM[2.5]*' tonnes/year')
  }

  if (grepl('pm10',pollutant,ignore.case = TRUE)) {
    label <- expression(PM[10]*' tonnes/year')
  }

  if (grepl('nh3',pollutant,ignore.case = TRUE)) {
    label <- expression(NH[3]*' tonnes/year')
  }

  if (grepl('nox',pollutant,ignore.case = TRUE)) {
    label <- expression(NO[x]*' tonnes/year')
  }
  if (grepl('Sox',pollutant,ignore.case = TRUE)) {
    label <- expression(SO[x]*' tonnes/year')
  }


  #to arrange based on value
  levels_grouping <- df_npri %>%
    # filter(Year == max(df_npri$Year)) %>%
    arrange((value)) %>%
    pull(groupingcolumn) %>%
    unique()

  if (tolower(output) == 'basic') {


    a <- df_npri %>%
      filter(!is.na(groupingcolumn)) %>%
      # filter(tolower(groupingcolumn) != 'dust') %>%
      dplyr::mutate(groupingcolumn = factor(groupingcolumn,levels = levels_grouping)) %>%
      # pull(groupingcolumn) %>% unique()
      ggplot(aes(x=Year,y=value,fill = groupingcolumn)) +
      geom_col(colour = 'black') +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            panel.background = element_rect(fill=NA,colour = 'black')) +
      ylab(label) +
      scale_x_continuous(expand=c(0,0)) +
      guides(fill=guide_legend(ncol=5,reverse = TRUE))

    return(a)
  }

  if (tolower(output) == 'plotly') {
    require(plotly)
    a <- {
      plot_ly(df_npri,x=~Year, y= ~value, color = ~groupingcolumn, type = 'bar', source = 'scatter',
              marker = list(line = list(width = 1,color = 'rgb(0, 0, 0)'))
      ) %>%
        layout(barmode = 'stack',yaxis = list(title = paste(pollutant,'(tonnes/year)')))
    }
    return(a)
  }
}


#' Retrieve the emission inventory from ECCC
#'
#' Files are retrieved from ECCC APRI site
#'
#' @param pollutant is the pollutant to retrieve
#' @param categorytype is either source, sector, or subsector. Default is source
#' @param is the ECCC URL for the NPRI
#'
#' @export
get_npri <- function(pollutant,categorytype = 'Source',URL=NULL) {
  if (0) {
    pollutant <- c('pm25')
    categorytype <- 'Source'
  }
  if (is.null(URL)) {
    URL <- 'https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-air-pollutant-emissions-inventory/EN_APEI-Can-Prov_Terr.csv'
  }
  #These will be the static  portion of the data, to be included in all queries
  cols_include <- c('Region','Source','Sector','Subsector','Year')

  #retrieve data from ECCC
  df_npri <- readr::read_csv(URL)

  #retrieve the columns related relevant to the query
  cols <- colnames(df_npri)
  cols_include <- cols_include[tolower(cols_include) %in% tolower(cols)]

  #find colnames that are pollutants
  #note that these are colnames that have parentheses
  lst_pollutants <- cols[grepl(pattern = '\\(',cols)]
  pollutant_select <- lst_pollutants[grepl(pollutant,lst_pollutants,ignore.case=TRUE)]

  df_npri <- df_npri %>%
    tidyr::pivot_longer(cols = lst_pollutants) %>%
    dplyr::rename(pollutant = name)

  #retrieve source, sector, subsectors
  lst_source <- df_npri %>%
    pull(Source) %>% unique()
  lst_sector <- df_npri %>%
    pull(Sector) %>% unique()
  lst_subsector <- df_npri %>%
    pull(Subsector) %>% unique()

  #filter according to user preference
  df_npri <- df_npri %>%
    filter(Region == 'BC') %>%
    filter(pollutant == pollutant_select) %>%
    filter(!is.na(value))

  #retrieve the values based on the category type
  if (tolower(categorytype) == 'source') {
    df_npri <- df_npri %>%
      filter(is.na(Sector),is.na(Subsector))
  }
  if (tolower(categorytype) == 'sector') {
    df_npri <- df_npri %>%
      filter(!is.na(Sector),is.na(Subsector))
  }
  if (tolower(categorytype) == 'subsector') {
    df_npri <- df_npri %>%
      filter(!is.na(Subsector))
  }


  df_npri <- df_npri %>%
    filter(Source != 'GRAND TOTAL')

  return(df_npri)
}

#' Calculate annual metrics
#'
#' @param years is vector listing the years for CAAQS calculation
#' @param savedirectory is the directory where the saved files will be located
#'
create_metrics_annual <- function(years, savedirectory = NULL) {
  if (0) {
    years <- 2000:2010
    savedirectory <- './test_data'
  }

  if (is.null(savedirectory)) {
    savedirectory <- './'
  }


  if (0)
  {
    #debug, to retrieve from previous wide version
    #chose from these four
    savefile <- paste(savedirectory,'pm25_annual.csv',sep='/')
    savefile <- paste(savedirectory,'o3_annual.csv',sep='/')
    savefile <- paste(savedirectory,'no2_annual.csv',sep='/')
    savefile <- paste(savedirectory,'so2_annual.csv',sep='/')

    #run both
    df <- readr::read_csv(savefile)
    savefile <- paste(savedirectory,'annual_results.csv',sep='/')
  }

  # create annual metrics ----
  savefile <- paste(savedirectory,'annual_results.csv',sep='/')
  #define non-value columns
  cols_static <- c('parameter','year','station_name','site','instrument','station','station_name_full')


  #pm2.5
  # savefile <- paste(savedirectory,'pm25_annual.csv',sep='/')
  for (year in years) {

    try({
      df <- importBC_data_avg(parameter = 'pm25',years = year,
                              averaging_type = c('annual 98p 24h','annual mean 24h'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)
      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]

      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,year,metric,value)

      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })

  }

  #ozone
  # savefile <- paste(savedirectory,'o3_annual.csv',sep='/')
  for (year in years) {
    try({
      df <- importBC_data_avg(parameter = 'o3',years = year,
                              averaging_type = c('annual 4th d8hm'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)

      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]

      #make the result long
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,year,metric,value)

      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })

  }

  #no2
  # savefile <- paste(savedirectory,'no2_annual.csv',sep='/')
  for (year in years) {

    try({
      df <- importBC_data_avg(parameter = 'no2',years = year,
                              averaging_type = c('annual 98p d1hm', 'annual mean 1hr'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)

      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]

      #make the result long
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,year,metric,value)


      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })

  }

  #so2
  # savefile <- paste(savedirectory,'so2_annual.csv',sep='/')
  for (year in years) {

    try({
      df <- importBC_data_avg(parameter = 'so2',years = year,
                              averaging_type = c('annual 99p d1hm', 'annual mean 1hr'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)

      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]

      #make the result long
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,year,metric,value)

      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })

  }




  #create captures ------
  savefile <- paste(savedirectory,'captures.csv',sep='/')
  for (param in c('pm25','o3','no2','so2')) {
    for (year in years) {
      try({
        df <- get_captures(param = param, years = year, merge_Stations = TRUE)
        readr::write_csv(df,
                         file = savefile,
                         append = file.exists(savefile))
      })
    }
  }


}

#' Calculate annual CAAQS metrics
#'
#' @param years is vector listing the years for CAAQS calculation
#' @param savedirectory is the location where the result files are saved
#'
create_caaqs_annual <- function(years, savedirectory = NULL) {
  if (0) {
    years <- 2013:2021
    savedirectory <- './test_data'
  }

  savefile = paste(savedirectory,'caaqs_results.csv',sep='/')

  caaqs_result <- NULL  #this is the result summarized in dataframe here
  years <- (min(years)-2):max(years)
  #redegine the years, add the extra two years before the beginning

  #PM2.5-----
  df <- importBC_data('pm25',years = years, flag_TFEE = TRUE,merge_Stations = TRUE)

  df <- df %>%
    dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
    dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
    filter(!is.na(value)) %>%
    group_by(date_time,site,instrument) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index == 1) %>% select(-index)

  #without TFEE
  pm25_annual <- rcaaqs::pm_annual_caaqs(df,by=c('site','instrument'))
  pm25_24h <- rcaaqs::pm_24h_caaqs(df,by=c('site','instrument'))



  #with TFEE
  pm25_annual_tfee <- rcaaqs::pm_annual_caaqs(df %>% filter(!flag_tfee),by=c('site','instrument'))
  pm25_24h_tfee <- rcaaqs::pm_24h_caaqs(df%>%filter(!flag_tfee),by=c('site','instrument'))

  caaqs_result <-  caaqs_result %>%
    bind_rows(pm25_annual$caaqs %>% mutate(tfee = FALSE)) %>%
    bind_rows(pm25_24h$caaqs%>% mutate(tfee = FALSE)) %>%
    bind_rows(pm25_annual_tfee$caaqs%>% mutate(tfee = TRUE)) %>%
    bind_rows(pm25_24h_tfee$caaqs%>% mutate(tfee = TRUE))

  rm('pm25_annual')
  rm('pm25_24h')
  rm('pm25_annual_tfee')
  rm('pm25_24h_tfee')

  #save the result into a file
  #this will be updated and overwritten on the next parts of the code
  readr::write_csv(caaqs_result,file = savefile)


  #Ozone-----
  df <- importBC_data('o3',years = years, flag_TFEE = TRUE,merge_Stations = TRUE)

  df <- df %>%
    dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
    dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
    filter(!is.na(value)) %>%
    group_by(date_time,site,instrument) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index == 1) %>% select(-index)

  #without TFEE
  o3_8h <- rcaaqs::o3_caaqs(df,by=c('site'))


  #with TFEE
  o3_8h_tfee <- rcaaqs::o3_caaqs(df %>% filter(!flag_tfee) ,by=c('site'))


  caaqs_result <-  caaqs_result %>%
    bind_rows(o3_8h$caaqs%>% mutate(tfee = FALSE)) %>%
    bind_rows(o3_8h_tfee$caaqs%>% mutate(tfee = TRUE))

  rm('o3_8h')
  rm('o3_8h_tfee')
  #save the result into a file
  #this will be updated and overwritten on the next parts of the code
  readr::write_csv(caaqs_result,file = savefile)


  #NO2-----
  df <- importBC_data('no2',years = years, flag_TFEE = TRUE,merge_Stations = TRUE)

  df <- df %>%
    dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
    dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
    filter(!is.na(value)) %>%
    group_by(date_time,site,instrument) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index == 1) %>% select(-index)



  #without TFEE
  no2_1hr <- rcaaqs::no2_3yr_caaqs(df,by=c('site'))
  no2_ann <- rcaaqs::no2_1yr_caaqs(df,by=c('site'))

  caaqs_result <-  caaqs_result %>%
    bind_rows(no2_1hr$caaqs%>% mutate(tfee = FALSE)) %>%
    bind_rows(no2_ann$caaqs%>% mutate(tfee = FALSE))

  rm('no2_1hr')
  rm('no2_ann')

  #save the result into a file
  #this will be updated and overwritten on the next parts of the code
  readr::write_csv(caaqs_result,file = savefile)


  #SO2-----
  df <- importBC_data('so2',years = years, flag_TFEE = TRUE,merge_Stations = TRUE)

  df <- df %>%
    dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
    dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
    filter(!is.na(value)) %>%
    group_by(date_time,site,instrument) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index == 1) %>% select(-index)

  #without TFEE
  so2_1hr <- rcaaqs::so2_3yr_caaqs(df,by=c('site'))
  so2_ann <- rcaaqs::so2_1yr_caaqs(df,by=c('site'))

  caaqs_result <-  caaqs_result %>%
    bind_rows(so2_1hr$caaqs%>% mutate(tfee = FALSE)) %>%
    bind_rows(so2_ann$caaqs%>% mutate(tfee = FALSE))

  rm('so2_1hr')
  rm('so2_ann')


  #save the result into a file
  #remove the extra two years
  caaqs_result <- caaqs_result %>%
    filter(caaqs_year>= (min(years)+2))

  readr::write_csv(caaqs_result,file = savefile)

}


#' Calculate the management levels
#'   NOTE: needs future management, change from datafile to an actual dataframe entry
#'
#' @param datafile is the location of the file containing summarized CAAQS data.
#' This dataset was created with the create_metrics_annual function
#'
get_management <- function(datafile = NULL) {

  if (0) {
    datafile <- NULL
  }
  #retrieve data

  if (is.null(datafile)) {
    datafile <- '././test_data/caaqs_results.csv'
    # list.files(datafile)
  }

  df <- readr::read_csv(datafile) %>%
    dplyr::mutate(idx0 = 1:n())

  df_levels <- rcaaqs::management_levels %>%
    dplyr::rename(metric = parameter) %>%
    dplyr::mutate(idx1 = 1:n())

  df_levels_ <- df_levels %>%
    select(metric,idx1,lower_breaks,upper_breaks) %>%
    dplyr::mutate(lower_breaks = ifelse(is.na(lower_breaks),-9999,lower_breaks)) %>%
    dplyr::mutate(upper_breaks = ifelse(is.na(upper_breaks),0,upper_breaks)) %>%
    dplyr::mutate(upper_breaks = ifelse(is.infinite(upper_breaks),99999999,upper_breaks))

  #conditions for assigning management levels
  df_ <- df %>%
    left_join(df_levels_) %>%
    mutate(metric_value = ifelse(is.na(metric_value),-9999,metric_value)) %>%
    filter(metric_value >= lower_breaks & metric_value < upper_breaks) %>%
    select(-lower_breaks,-upper_breaks) %>%
    mutate(metric_value = ifelse(metric_value == -9999,NA, metric_value)) %>%
    left_join(df_levels) %>%
    select(-idx0,-idx1)

  #add column called colour_order to put sorting or numerical order to the colours
  df_colour <- tribble(
    ~colour_text, ~colour_order,
    'grey',0,
    'green',1,
    'yellow',2,
    'orange',3,
    'red',4
  )
  df_ <- left_join(df_,df_colour)

  if (nrow(df_) != nrow(df)) {
    print('Error, some rows ended up missing or not determined')
    return(NULL)
  } else {
    return(df_)
  }



}

#' Retrieves the management level summary of station and airzones
#'
#' @param outputtype is either 'complete','station', 'airzone'
#' 'complete' means that output is detailed for each metric, in each station
#' 'station' means that output is a summary of the management for the station. only metric with highest management level is displayed
#' 'airzone' means that output is a summary of the management for the airzones
#'
get_management_summary <- function(outputtype = 'complete') {


  #define the parameter for each metric
  #arrange in terms of an order
  df_metric <- tribble(
    ~metric,~parameter,
    "pm2.5_annual",'pm25',
    "pm2.5_24h",'pm25',
    "o3",'o3',
    "no2_1yr",'no2',
    "no2_3yr",'no2',
    "so2_1yr",'so2',
    "so2_3yr",'so2',
  )


  #retrieve station and air zone
  #note that some stations are merged based on the history, have to retrieve it
  stn_history <- get_station_history() %>%
    select(STATION_NAME,`Merged Station Name`) %>%
    filter(!is.na(`Merged Station Name`)) %>%
    unique()


  lst_stations <- listBC_stations(use_CAAQS = TRUE) %>%
    select(STATION_NAME,LAT,LONG,AIRZONE,Label) %>%
    arrange(Label) %>%
    group_by(STATION_NAME) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index == 1) %>% select(-index) %>%
    ungroup() %>%
    left_join(stn_history) %>%
    mutate(STATION_NAME = ifelse(is.na(`Merged Station Name`),STATION_NAME,`Merged Station Name`)) %>%
    select(-`Merged Station Name`) %>%
    dplyr::rename(site = STATION_NAME,
                  latitude = LAT,
                  longitude = LONG,
                  airzone = AIRZONE,
                  label = Label)

  df <- get_management()

  df <- df %>%
    select(site,instrument,caaqs_year,metric,metric_value,colour,colour_text,colour_order,tfee) %>%
    left_join(lst_stations) %>%
    left_join(df_metric)

#add order to the metric
  df$metric <- factor(df$metric,levels = df_metric$metric)
  #calculate and return result based on the type specified
  outputtype <- tolower(outputtype)
  if (outputtype == 'complete') {
    return(df)
  }

  if (outputtype == 'station') {

    df <- df %>%
      group_by(parameter,site,caaqs_year,airzone,tfee) %>%
      dplyr::mutate(max_colour_order = max(colour_order)) %>%
      ungroup() %>%
      filter(colour_order == max_colour_order) %>%
      arrange(metric) %>%   #this gives priority to annual over 24h/1h metrics
      group_by(parameter,site,caaqs_year,airzone,tfee) %>%
      dplyr::mutate(index =1:n()) %>%
      filter(index==1) %>% ungroup() %>% select(-index) %>%
      arrange(parameter,site,tfee,caaqs_year) %>%
      select(-max_colour_order)

    return(df)
    }

  if (outputtype == 'airzone') {

    df <- df %>%
      arrange(airzone,metric_value) %>%
      group_by(parameter,metric,caaqs_year,airzone,tfee) %>%
      dplyr::mutate(max_metric_value = max(metric_value,na.rm = TRUE)) %>%
      ungroup() %>%
      filter(metric_value == max_metric_value) %>%
      arrange(desc(colour_order), metric) %>%   #this gives prioroty to pm2.5annual or 24h, and no2_3yr over 1 yr
      group_by(parameter,caaqs_year,airzone,tfee) %>%
      dplyr::mutate(max_colour_order = max(colour_order),index = 1:n()) %>%
      filter(colour_order == max_colour_order,index == 1) %>% ungroup() %>% select(-index) %>%
      COLUMN_REORDER(c('parameter','airzone','tfee','caaqs_year')) %>%
      select(-max_colour_order,-max_metric_value) %>%
      arrange(parameter,airzone,tfee,caaqs_year)

    return(df)
  }

}

#' INCOMPLETE:
#' Create files that will make the CAAQS bar graph
#'
#' @param filedirectory is the location of the data files
#'
#' @return creates a file called air_data_summary.csv in the file directory
create_CAAQS_graph_files <- function(filedirectory = NULL) {

  if (0) {
    filedirectory <- NULL
  }

  if (is.null(filedirectory)) {
    filedirectory <- '././test_data'
    list.files(filedirectory)
  }

  file_annual <- paste(filedirectory,'annual_results.csv',sep='/')
  file_captures <- paste(filedirectory,'annual_results.csv',sep='/')
  file_ <- paste(filedirectory,'annual_results.csv',sep='/')
}

#' Create CAAQS bar graph
#'
#' @description Creates the bar graph use for CAAQS
#'
#' @param df is the dataframe containing CAAQS metrics, and non-CAAQS annual metrics
#' This dataframe is created using the create_CAAQS_graph_files()
#' @param parameter is the parameter of either '','','',''
#' @param is the station name
#' if NULL, result displays the available stations that can be listed
create_CAAQS_graph <- function(df, parameter, station = NULL) {

  if (0) {
    aq_summary <-  read_csv('././test_data/air_data_summary.csv') %>%
      mutate(metric = recode(metric,'o3' = 'ozone'  ,'o3_tfee' = 'ozone_tfee'))
    df <- aq_summary
  }
}
