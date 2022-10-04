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
      plot_ly(df_npri,x=~Year, y= ~value, color = ~groupingcolumn, type = 'bar', source = 'scatter') %>%
        layout(barmode = 'stack')
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
create_metrics_annual <- function(years, savedirectory = NULL) {
  if (0) {
    years <- 2011:2021
    savedirectory <- './test_data'
  }

  if (is.null(savedirectory)) {
    savedirectory <- './'
  }


  # create annual metrics ----
  #pm2.5
  savefile <- paste(savedirectory,'pm25_annual.csv',sep='/')
  for (year in years) {
    df <- importBC_data_avg(parameter = 'pm25',years = year,
                            averaging_type = c('annual 98p 24h','annual mean 24h'),
                            flag_TFEE = TRUE,
                            merge_Stations = TRUE)

    readr::write_csv(df,
                     file = savefile,
                     append = file.exists(savefile))

  }

  #ozone
  savefile <- paste(savedirectory,'o3_annual.csv',sep='/')
  for (year in years) {
    df <- importBC_data_avg(parameter = 'o3',years = year,
                            averaging_type = c('annual 4th d8hm'),
                            flag_TFEE = TRUE,
                            merge_Stations = TRUE)

    readr::write_csv(df,
                     file = savefile,
                     append = file.exists(savefile))

  }

  #no2
  savefile <- paste(savedirectory,'no2_annual.csv',sep='/')
  for (year in years) {
    df <- importBC_data_avg(parameter = 'no2',years = year,
                            averaging_type = c('annual 98p d1hm', 'annual mean 1hr'),
                            flag_TFEE = TRUE,
                            merge_Stations = TRUE)

    readr::write_csv(df,
                     file = savefile,
                     append = file.exists(savefile))

  }

  #so2
  savefile <- paste(savedirectory,'so2_annual.csv',sep='/')
  for (year in years) {
    df <- importBC_data_avg(parameter = 'so2',years = year,
                            averaging_type = c('annual 99p d1hm', 'annual mean 1hr'),
                            flag_TFEE = TRUE,
                            merge_Stations = TRUE)

    readr::write_csv(df,
                     file = savefile,
                     append = file.exists(savefile))

  }

  #create captures ------
  savefile <- paste(savedirectory,'captures.csv',sep='/')
  for (param in c('pm25','o3','no2','so2')) {
    df <- get_captures(param = param, years = years, merge_Stations = TRUE)
    readr::write_csv(df,
                     file = savefile,
                     append = file.exists(savefile))

  }
}

#' Calculate annual CAAQS metrics
#'
#' @param years is vector listing the years for CAAQS calculation
#' @param savedirectory is the location where the result files are saved
create_metrics_annual <- function(years, savedirectory = NULL) {
  if (0) {
    years <- 2011:2021
    savedirectory <- './test_data'
  }

  savefile = paste(savedirectory,'caaqs_results.csv',sep='/')

  caaqs_result <- NULL  #this is the result summarized in dataframe here

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
  readr::write_csv(caaqs_result,file = savefile)

}
