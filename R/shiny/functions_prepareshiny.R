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
        select(parameter,site,instrument,tfee,year,metric,value)

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
        select(parameter,site,instrument,tfee,year,metric,value)

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
        select(parameter,site,instrument,tfee,year,metric,value)


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
        select(parameter,site,instrument,tfee,year,metric,value)

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
#' @description This script is dependent on the rcaaqs package
#' it already properly applies GDAD in calculation of yearly metrics, and the CAAQS
#' Better than create_metrics_annual() but may take longer to complete
#'
#' @param years is vector listing the years for CAAQS calculation
#' @param savedirectory is the location where the result files are saved
#'
create_caaqs_annual <- function(years, savedirectory = NULL) {
  if (0) {
    years <- 2013:2021
    savedirectory <- './test_data'

    for (files in list.files('././r/',full.names = TRUE)) {
      try(source(files))
    }
  }

  #where files will be saved
  savefile = paste(savedirectory,'caaqs_results.csv',sep='/')

  #defines the resulting column names in this order
  cols_final <- c('parameter','site','instrument','year',
                  'tfee','metric_value','metric','flag_two_of_three_years')

  for (param in c('pm25','o3','no2','so2')) {
    df <- NULL

    #Retrieve data, different retrieval for ozone
    if(param != 'o3') {
      try(
        df <- importBC_data(param,years = (min(years)-2):max(years),
                            flag_TFEE = TRUE,merge_Stations = TRUE)
      )
    } else {
      try(
        df <- importBC_data(param,years = (min(years)-3):max(years),
                            flag_TFEE = TRUE,merge_Stations = TRUE)
      )
    }

    #if there was no data retrieved
    if (is.null(df)) {
      next
    }

    if (nrow(df) == 0) {
      next
    }

    df <- df %>%
      dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
      dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
      filter(!is.na(value)) %>%
      group_by(date_time,site,instrument) %>%
      dplyr::mutate(index = 1:n()) %>%
      filter(index == 1) %>% select(-index)

    if (param == 'pm25') {
      #without TFEE
      pm25_annual <- rcaaqs::pm_annual_caaqs(df,by=c('site','instrument'))
      pm25_24h <- rcaaqs::pm_24h_caaqs(df,by=c('site','instrument'))

      pm25_caaqs <- pm25_annual$caaqs %>%
        bind_rows(pm25_24h$caaqs) %>%
        mutate(parameter = 'PM25',tfee = FALSE) %>%
        dplyr::rename(year = caaqs_year) %>%
        select(cols_final)

      pm25_yoy <- pm25_annual$yearly_avg %>%
        bind_rows(pm25_24h$yearly_98)%>%
        mutate(parameter = 'PM25',tfee = FALSE,flag_two_of_three_years = NA) %>%
        select(parameter,site,instrument,year,tfee,ann_avg,ann_98_percentile,flag_two_of_three_years) %>%
        tidyr::pivot_longer(cols = c(ann_avg,ann_98_percentile)) %>%
        filter(!is.na(value)) %>%
        dplyr::rename(metric = name,
                      metric_value = value)%>%
        select(cols_final)

      df_result <- pm25_caaqs %>%
        bind_rows(pm25_yoy)

      #with TFEE
      pm25_annual <- rcaaqs::pm_annual_caaqs(df %>% filter(!flag_tfee),by=c('site','instrument'))
      pm25_24h <- rcaaqs::pm_24h_caaqs(df %>% filter(!flag_tfee),by=c('site','instrument'))

      pm25_caaqs <- pm25_annual$caaqs %>%
        bind_rows(pm25_24h$caaqs) %>%
        mutate(parameter = 'PM25',tfee = TRUE) %>%
        dplyr::rename(year = caaqs_year) %>%
        select(cols_final)

      pm25_yoy <- pm25_annual$yearly_avg %>%
        bind_rows(pm25_24h$yearly_98)%>%
        mutate(parameter = 'PM25',tfee = TRUE,flag_two_of_three_years = NA) %>%
        select(parameter,site,instrument,year,tfee,ann_avg,ann_98_percentile) %>%
        tidyr::pivot_longer(cols = c(ann_avg,ann_98_percentile)) %>%
        filter(!is.na(value)) %>%
        dplyr::rename(metric = name,
                      metric_value = value)

      df_result <- df_result %>%
        bind_rows(pm25_caaqs) %>%
        bind_rows(pm25_yoy) %>%
        mutate(metric = recode(metric,
                               'pm2.5_annual'='pm25_annual',
                               'pm2.5_24h'='pm25_24h',
                               'ann_98_percentile'='pm25_24hr(1yr)',
                               'ann_avg'='pm25_ann(1yr)'
        ))

      rm('pm25_annual')
      rm('pm25_24h')
    }

    if (param == 'o3') {

      #without TFEE
      o3_8h <- rcaaqs::o3_caaqs(df,by=c('site'))

      #with TFEE
      o3_8h_tfee <- rcaaqs::o3_caaqs(df %>% filter(!flag_tfee) ,by=c('site'))

      o3_caaqs <-  o3_8h$caaqs%>% mutate(tfee = FALSE) %>%
        bind_rows(o3_8h_tfee$caaqs%>% mutate(tfee = TRUE)) %>%
        mutate(parameter = 'O3', instrument = NA)%>%
        dplyr::rename(year = caaqs_year) %>%
        select(cols_final)

      o3_yoy <- o3_8h$ann_4th_highest %>%
        mutate(tfee = FALSE) %>%
        bind_rows(
          o3_8h_tfee$ann_4th_highest %>% mutate(tfee = TRUE)
        ) %>%
        select(site,year,tfee,ann_4th_highest) %>%
        dplyr::rename(metric_value = ann_4th_highest) %>%
        mutate(parameter = 'O3', instrument = NA,
               metric = 'o3_8h(1yr)',flag_two_of_three_years = NA) %>%
        select(cols_final)

      df_result <- o3_caaqs %>%
        bind_rows(o3_yoy) %>%
        mutate(metric = recode(metric,'o3'='o3_8h'))

      rm('o3_8h')
      rm('o3_8h_tfee')
    }

    if (param == 'no2') {

      #without TFEE
      no2_1hr <- rcaaqs::no2_3yr_caaqs(df,by=c('site'))
      no2_ann <- rcaaqs::no2_1yr_caaqs(df,by=c('site'))

      no2_caaqs <-  no2_1hr$caaqs%>%
        mutate(tfee = FALSE) %>%
        bind_rows(
          no2_ann$caaqs%>%
            mutate(tfee = FALSE)
        ) %>%
        dplyr::rename(year = caaqs_year) %>%
        mutate(parameter = 'NO2', instrument = NA) %>%
        select(cols_final)

      no2_yoy <- no2_1hr$yearly_98 %>%
        bind_rows(
          no2_ann$yearly_hr
        ) %>%
        select(site,year,ann_98_percentile,avg_yearly) %>%
        mutate(parameter = 'NO2',tfee = FALSE,instrument = NA,flag_two_of_three_years=NA) %>%
        tidyr::pivot_longer(cols = c(ann_98_percentile,avg_yearly)) %>%
        dplyr::rename(metric_value = value, metric = name) %>%
        filter(!is.na(metric_value)) %>%
        select(cols_final)

      df_result <- no2_caaqs %>%
        bind_rows(no2_yoy)%>%
        mutate(metric = recode(metric,
                               'no2_3yr'='no2_1hr',
                               'no2_1yr'='no2_ann',
                               'ann_98_percentile'='no2_1hr(1yr)',
                               'avg_yearly'='no2_ann(1yr)'
        ))

      rm('no2_1hr')
      rm('no2_ann')

    }

    if (param == 'so2') {


      #without TFEE
      so2_1hr <- rcaaqs::so2_3yr_caaqs(df,by=c('site'))
      so2_ann <- rcaaqs::so2_1yr_caaqs(df,by=c('site'))

      so2_caaqs <-  so2_1hr$caaqs%>% mutate(tfee = FALSE) %>%
        bind_rows(so2_ann$caaqs%>% mutate(tfee = FALSE)) %>%
        dplyr::rename(year = caaqs_year) %>%
        mutate(parameter = 'SO2', instrument = NA) %>%
        select(cols_final)

      so2_yoy <- so2_1hr$yearly_99 %>%
        bind_rows(so2_ann$yearly_hr) %>%
        select(site,year,ann_99_percentile,avg_yearly) %>%
        tidyr::pivot_longer(cols = c(ann_99_percentile,avg_yearly)) %>%
        dplyr::rename(metric = name,
                      metric_value = value) %>%
        filter(!is.na(metric_value)) %>%
        mutate(tfee = FALSE,parameter = 'SO2',
               instrument = NA,flag_two_of_three_years = NA) %>%
        select(cols_final)

      df_result <- so2_caaqs %>%
        bind_rows(so2_yoy) %>%
        select(cols_final)%>%
        mutate(metric = recode(metric,
                               'so2_3yr'='so2_1hr',
                               'so2_1yr'='so2_ann',
                               'ann_99_percentile'='so2_1hr(1yr)',
                               'avg_yearly'='so2_ann(1yr)'
        ))

      rm('so2_1hr')
      rm('so2_ann')

    }



    #save the result into a file
    #append if the file exist
    #standardize the names of columns
    df_result <- df_result %>%
      filter(year %in% years) %>%
      arrange(parameter,site,instrument,year,tfee)

    readr::write_csv(df_result,file = savefile,append = file.exists(savefile))
  }

  df_result <- readr::read_csv(savefile)
  return(df_result)
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
#' @param parameter is the parameter of either 'pm25','o3','no2','so2'
#' @param is the station name
#' if NULL, result displays the available stations that can be listed
create_CAAQS_graph <- function(df, parameter, station = NULL) {

  if (0) {
    aq_summary1 <-  readr::read_csv('././test_data/air_data_summary.csv') %>%
      mutate(metric = recode(metric,'o3' = 'ozone'  ,'o3_tfee' = 'ozone_tfee'))

    aq_summary <-  readr::read_csv('./test_data/caaqs_results.csv')
    unique(aq_summary$metric)
    df <- aq_summary
    parameter <- 'pm2.5'
    station <- NULL
    station <- 'Prince George Plaza 400'


  }


  a <- NULL #output results


  #standardize the name of parameter
  parameter <- recode(parameter,'PM\u2082.\u2085' = 'pm2.5',
                      'NO\u2082' = 'no2',
                      'SO\u2082' ='so2',
                      'Ozone' = 'ozone')
  parameter <- tolower(parameter)
  parameter <- gsub('o3','ozone',parameter,ignore.case = TRUE)
  parameter <- gsub('pm25','pm2.5',parameter,ignore.case = TRUE)


  if (is.null(station)) {
    a <- df %>%
      filter(grepl(parameter,metric,ignore.case = TRUE)) %>%
      filter(!is.na(metric_value)) %>%
      pull(site) %>% unique() %>%  sort()
    return(a)
  }

  #this segment added as fix to the source file
  if (0) {
    df <- aq_summary
    unique(df$metric)

    colnames(aq_summary1)
    colnames(df)
  }
  try(
  df <- df %>%
    mutate(metric = gsub('pm25','pm2.5',metric,ignore.case = TRUE)) %>%
    mutate(metric = ifelse(tfee,paste(metric,'_tfee',sep=''),metric)) %>%
    mutate(metric = recode(metric,
                          'pm2.5_24h' = 'pm2.5_24h',
                          'pm2.5_24hr(1yr)' = 'pm2.5_24h (1yr)',
                          'pm2.5_24h_tfee' = 'pm2.5_24h_tfee',
                          'pm2.5_24hr(1yr)_tfee' = 'pm2.5_24h_tfee (1yr)',
                          'pm2.5_annual' = 'pm2.5_annual',
                          'pm2.5_ann(1yr)' = 'pm2.5_annual (1yr)',
                          'pm2.5_annual_tfee' =  'pm2.5_annual_tfee',
                          'pm2.5_ann(1yr)_tfee' = 'pm2.5_annual_tfee (1yr)',
                          'o3_8h' = 'ozone',
                          'o3_8h_tfee' = 'ozone_tfee',
                          'o3_8h(1yr)' = 'ozone_4th (1yr)',
                          'o3_8h(1yr)_tfee' = 'ozone_4th_tfee (1yr)',
                          'no2_1hr' = 'no2_1hr',
                          'no2_ann' = 'no2_annual',
                          'no2_1hr(1yr)' = 'no2_1hr (1yr)',
                          'no2_ann(1yr)' = 'no2_annual (1yr)',
                          'so2_1hr' = 'so2_1hr',
                          'so2_ann' = 'so2_annual',
                          'so2_1hr(1yr)' = 'so2_1hr (1yr)',
                          'so2_ann(1yr)' = 'so2_annual (1yr)'

                           )) %>%
    select(site,instrument,year,metric,metric_value))
  #lower the case for the station
  station <- tolower(station)
  # print(station)
  if (parameter == 'pm2.5')
  {




    aq <- df%>%
      filter(grepl('pm2.5',metric) ) %>%
      filter(tolower(site) == station)  #PROD

    # print(nrow(aq))
    #define data for line and bar graph
    #this will be the line trend
    aq_caaqs <- aq %>%
      filter(!grepl('1yr',metric))

    #this will be the bar graph
    #recalculate so tfee is only the delta value
    aq_1yr <-
      aq %>%
      filter(grepl('1yr',metric)) %>%
      tidyr::pivot_wider(names_from = metric, values_from = metric_value) %>%
      mutate(`pm2.5_24h (1yr)` = `pm2.5_24h (1yr)` - `pm2.5_24h_tfee (1yr)`,
             `pm2.5_annual (1yr)` = `pm2.5_annual (1yr)`- `pm2.5_annual_tfee (1yr)`
      ) %>%
      # View()
      tidyr::pivot_longer(cols = -c('site','instrument','year'),
                   names_to = 'metric', values_to = 'metric_value')


    #define the min and max of the data
    year_min = min(min(aq$year),2018)
    year_max = max(max(aq$year),2020)
    # x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    x_lbls <- year_min:year_max
    ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'pm2.5_24h (1yr)'],na.rm=TRUE),32)
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'pm2.5_annual (1yr)'],na.rm=TRUE),12)


    #managmeent levels
    # geom_rect(mapping=aes(xmin=grp_start,xmax=2019.5,ymin=28,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
    # geom_rect(mapping=aes(xmin=2019.5,xmax=2021,ymin=27,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +




    #the annual plot ------------

    p1_ann <-

      aq_caaqs  %>%
      filter(grepl('annual',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric),fill = NA)  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min,xmax=2019.5,ymin=10,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max+1,ymin=8.8,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +

      geom_rect(mapping=aes(xmin=year_min,xmax=2019.5,ymin=6.4,ymax=10),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max + 1,ymin=6.4,ymax=8.8),alpha=0.1,color=NA,fill='#F46D43') +

      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=4,ymax=6.4),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=4),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('annual',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('annual',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('annual',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black',fill='white') +
      geom_point(size=4,fill='white') +

      #CAAQS reference lines
      geom_segment(aes(x=year_min,y=10,xend=2019.5,yend=10),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=2019.5,y=8.8,xend=year_max + 1,yend=8.8),colour='red2',linetype='dashed',size=1) +
      geom_segment(aes(x=2019.5,y=8.8,xend=2019.5,yend=10),colour='red2',linetype='dashed',size=1)+


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=10+1,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
      annotate("text",x= year_max,y=8.8+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust=0.2) +

      # geom_hline(yintercept = 27, linetype ='dashed',colour = 'red') +
      # annotate("text",x= 2013,y=27,label = '2020 CAAQS',colour = 'red',angle =0) +


      # xlab('Reporting Period')+
      ylab(expression(paste('PM'[2.5],' Annual Metric (',mu,'g/',m^3,')'))) +


      labs(colour = 'Location',
           title = paste('Air Zone')) + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'right', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_rect(fill = 'white', colour = NA, size = 0.25),
            legend.background = element_blank(),
            # legend.key = element_blank(),
            axis.text.x = element_blank(),axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +
      # theme_minimal() +

      ylim(0,ymax_ann) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_ann), expand = c(0,0))+
      scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
      scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

      scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted'))

    #the 24-hour metric plot--------
    p1_24h <-

      aq_caaqs  %>%
      filter(grepl('24h',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max - 0.5,ymin=28,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_max - 0.5,xmax=year_max + 1,ymin=27,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +

      geom_rect(mapping=aes(xmin=year_min,xmax=year_max - 0.5,ymin=19,ymax=28),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_max - 0.5,xmax=year_max + 1,ymin=19,ymax=27),alpha=0.1,color=NA,fill='#F46D43') +

      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=10,ymax=19),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=10),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('24h',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('24h',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('24h',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +


      #CAAQS reference lines
      geom_segment(aes(x=year_min,y=28,xend=year_max - 0.5,yend=28),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=year_max - 0.5,y=27,xend=year_max + 1,yend=27),colour='red2',linetype='dashed',size=1) +
      geom_segment(aes(x=year_min,y=28,xend=year_max - 0.5,yend=28),colour='red2',linetype='dashed',size=1)+

      #labels to CAAQS reference lines
      annotate("text",x= year_min+1,y=28+4,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
      annotate("text",x= year_max,y=27+4,label = '2020 CAAQS',colour = 'white',angle =0,hjust=0.2) +

      # geom_hline(yintercept = 27, linetype ='dashed',colour = 'red') +
      # annotate("text",x= 2013,y=27,label = '2020 CAAQS',colour = 'red',angle =0) +


      # xlab('Reporting Period')+
      ylab(expression(paste('PM'[2.5],' 24-Hour Metric (',mu,'g/',m^3,')'))) +


      labs(colour = 'TFEE',
           title = '') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'none', legend.direction = 'horizontal',legend.justification = 'left',
            legend.title=element_blank(), legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_24h) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
      scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
      scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
      scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
      xlab('Three-year CAAQS Reporting Period')
    # theme(legend.position = 'none')
    a <- print(p1_ann/p1_24h)
  }

  if (parameter == 'ozone')
  {
    aq <- aq_summary%>%
      filter(grepl('ozone',metric)) %>%
      # filter(year >=2012) %>%
      # filter(site == 'Victoria Topaz')   #debug
      filter(tolower(site) == station)  #PROD



    #define data for line and bar graph
    #this will be the line trend
    aq_caaqs <- aq %>%
      filter(!grepl('1yr',metric))

    #this will be the bar graph
    #recalculate so tfee is only the delta value
    aq_1yr <-
      aq %>%
      filter(grepl('1yr',metric)) %>%
      pivot_wider(names_from = metric, values_from = metric_value) %>%
      mutate(`ozone_4th (1yr)` = `ozone_4th (1yr)` - `ozone_4th_tfee (1yr)`) %>%
      # View()   #debug
      select(-instrument) %>%
      pivot_longer(cols = -c('site','year'),
                   names_to = 'metric', values_to = 'metric_value')

    #define the min and max of the data
    year_min = min(min(aq$year),2019)
    year_max = max(max(aq$year),2020)
    # x-axis labels
    x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'ozone_4th (1yr)'],na.rm=TRUE),70)



    p1_ann <-

      aq_caaqs  %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min,xmax=2019.5,ymin=63,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max+1,ymin=62,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +

      geom_rect(mapping=aes(xmin=year_min,xmax=2019.5,ymin=56,ymax=63),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max + 1,ymin=56,ymax=62),alpha=0.1,color=NA,fill='#F46D43') +

      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=50,ymax=56),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=30,ymax=50),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year,
                   y= aq_1yr$metric_value,
                   fill = aq_1yr$metric
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +
      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +

      #CAAQS reference lines
      geom_segment(aes(x=year_min,y=63,xend=2019.5,yend=63),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=2019.5,y=62,xend=year_max + 1,yend=62),colour='red2',linetype='dashed',size=1) +
      geom_segment(aes(x=2019.5,y=62,xend=2019.5,yend=63),colour='red2',linetype='dashed',size=1)+


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=63+2,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
      annotate("text",x= year_max,y=62+2,label = '2020 CAAQS',colour = 'white',angle =0,hjust=0.2) +

      # geom_hline(yintercept = 27, linetype ='dashed',colour = 'red') +
      # annotate("text",x= 2013,y=27,label = '2020 CAAQS',colour = 'red',angle =0) +


      # xlab('Reporting Period')+
      ylab(expression(paste('Ozone 8-Hour Metric (ppb)'))) +


      labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'right', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_blank(),
            legend.background = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +
      ylim(0,ymax_ann) +
      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,year_max+1),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(30,ymax_ann), expand = c(0,0),oob=rescale_none,trans = 'reverse')+
      scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
      scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

      scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted')) +
      xlab('Three-year CAAQS Reporting Period')

    a <- print(p1_ann)

  }

  if (parameter %in% c('no2'))
  {
    aq <- aq_summary%>%
      filter(grepl('no2',metric)) %>%
      # filter(year >=2012) %>%
      select(-instrument) %>%
      # filter(site == 'Victoria Topaz')   #debug
      filter(tolower(site) == station)  #PROD

    aq_caaqs <- aq %>%
      filter(!grepl('1yr',metric))

    aq_1yr <-
      aq %>%
      filter(grepl('1yr',metric))

    #define the min and max of the data
    year_min = min(min(aq$year),2018)
    year_max = max(max(aq$year),2020)
    x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')

    #note that ymax_24h is 1-hour metric
    ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'no2_1hr (1yr)'],na.rm=TRUE),70)
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'no2_annual (1yr)'],na.rm=TRUE),20)


    p1_ann <-
      aq_caaqs  %>%
      filter(grepl('annual',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=17,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=7,ymax=17),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=2,ymax=7),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=2),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('annual',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('annual',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('annual',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +

      #CAAQS reference lines
      # geom_segment(aes(x=year_min,y=12,xend=2019.5,yend=12),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=year_min,y=17,xend=year_max + 1,yend=17),colour='red2',linetype='dashed',size=1) +


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=17+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust =0) +


      # xlab('Reporting Period')+
      ylab(expression(paste('NO'[2],' Annual Metric (ppb)'))) +

      labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'none', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_blank(),
            legend.background = element_blank(),
            axis.text.x = element_blank(),axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_ann) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_ann), expand = c(0,0))+
      scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
      scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

      scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted'))


    #the NO2 1-hour metric plot--------
    p1_24h <-

      aq_caaqs  %>%
      filter(grepl('1hr',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=60,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=31,ymax=60),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=20,ymax=31),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=20),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('1hr',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('1hr',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('1hr',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +


      #CAAQS reference lines
      geom_segment(aes(x=year_min,y=60,xend=year_max + 1,yend=60),colour='red2',linetype='dashed',size=1)+

      #labels to CAAQS reference lines
      annotate("text",x= year_min+1,y=65,label = '2020 CAAQS',colour = 'white',angle =0,hjust =0) +

      # xlab('Reporting Period')+
      ylab(expression(paste('NO'[2],' 1-Hour Metric (ppb)'))) +
      labs(colour = 'TFEE',
           title = '') +

      theme(legend.position = 'none', legend.direction = 'horizontal',legend.justification = 'left',
            legend.title=element_blank(), legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_24h) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
      scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
      scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
      scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
      xlab('Three-year CAAQS Reporting Period')


    a <- print(p1_ann/p1_24h)

  }

  if (parameter %in% c('so2'))
  {
    aq <- aq_summary%>%
      filter(grepl('so2',metric)) %>%
      # filter(year >=2012) %>%
      select(-instrument) %>%
      # filter(site == 'Victoria Topaz')   #debug
      filter(tolower(site) == station)  #PROD

    aq_caaqs <- aq %>%
      filter(!grepl('1yr',metric))

    aq_1yr <-
      aq %>%
      filter(grepl('1yr',metric))

    #define the min and max of the data
    year_min = min(min(aq$year),2018)
    year_max = max(max(aq$year),2020)
    x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')

    #note that ymax_24h is 1-hour metric
    ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'so2_1hr (1yr)'],na.rm=TRUE),75)
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'so2_annual (1yr)'],na.rm=TRUE),8)


    p1_ann <-
      aq_caaqs  %>%
      filter(grepl('annual',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=5,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=3,ymax=5),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=2,ymax=3),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=2),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('annual',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('annual',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('annual',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +

      #CAAQS reference lines
      # geom_segment(aes(x=year_min,y=12,xend=2019.5,yend=12),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=year_min,y=5,xend=year_max + 1,yend=5),colour='red2',linetype='dashed',size=1) +


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=5+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust =0) +


      # xlab('Reporting Period')+
      ylab(expression(paste('SO'[2],' Annual Metric (ppb)'))) +

      labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'none', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_blank(),
            legend.background = element_blank(),
            axis.text.x = element_blank(),axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_ann) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_ann), expand = c(0,0))+
      scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
      scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

      scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted'))


    #the so2 1-hour metric plot--------
    p1_24h <-

      aq_caaqs  %>%
      filter(grepl('1hr',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=70,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=50,ymax=70),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=30,ymax=50),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=30),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('1hr',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('1hr',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('1hr',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +


      #CAAQS reference lines
      geom_segment(aes(x=year_min,y=70,xend=year_max + 1,yend=70),colour='red2',linetype='dashed',size=1)+

      #labels to CAAQS reference lines
      annotate("text",x= year_min+1,y=65,label = '2020 CAAQS',colour = 'white',angle =0,hjust =0) +

      # xlab('Reporting Period')+
      ylab(expression(paste('SO'[2],' 1-Hour Metric (ppb)'))) +


      labs(colour = 'TFEE',
           title = '') +

      theme(legend.position = 'none', legend.direction = 'horizontal',legend.justification = 'left',
            legend.title=element_blank(), legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_24h) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
      scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
      scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
      scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
      xlab('Year')


    a <- print(p1_ann/p1_24h)
  }

  return(a)

}
