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



#' Create a bar graph of the NPRI
#'
#' @param pollutant
#' @param df is the pollutant data. This can be retrieved with get_apei()
#' @param categorytype is either source, sector, or subsector. Default is source
#' @param URL is the ECCC URL for the NPRI
#' @param output is the output type of either 'basic' or 'plotly'
#'
#' @export
plot_npri <- function(pollutant,df=NULL,categorytype = 'Source',URL=NULL,output = 'basic') {
  if (0) {
    pollutant <- c('')
    categorytype <- 'Source'
    output = 'basic'
    URL=NULL
  }

  require(ggplot2)

  df <- get_apei(pollutant = pollutant, df=df, categorytype = categorytype, URL = URL)
  df_npri <- df %>%
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
  if (grepl('sox',pollutant,ignore.case = TRUE)) {
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
      guides(fill=guide_legend(ncol=3,reverse = TRUE))

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


#' Create a ranked bar graph for each metric
#'
#' @param df_caaqs_results is a dataframe containing caaqs results.
#' These are created by the create_caaqs_annual() function
#' @param pollutant is either pm25,o3,no2,so2
#' @param year is the year to display
#' @param airzone is the airzone that will be displayed. If NULL, it will include all airzones
#' @param df_stations is a dataframe containing the list of stations.
#' If NULL, it will retrieve using the listBC_stations() function
plot_bar_ranked <- function(df_caaqs_results,pollutant,year,airzone = NULL,df_stations = NULL) {
  if (0) {
    df_caaqs_results <- readr::read_csv('../test_data/caaqs_results.csv')
    # metric_ <- c('pm25_annual')
    # metric_ <- c('pm25_24h')
    airzone <- 'Central Interior'
    df_stations = NULL
    year <- 2015
    pollutant <- 'pm25'


  }

  library(patchwork)
  print(paste('plot_bar_ranked() function. pollutant=',pollutant))
  pollutant_filter <- pollutant
  print(pollutant_filter)
  #list of metrics
  df_metric <- tribble(
    ~pollutant,~parameter,~metric,
    'PM25','pm2.5_annual','pm25_annual',
    'O3','o3','o3_8h',
    'PM25','pm2.5_24h','pm25_24h',
    'NO2','no2_1yr','no2_ann',
    'NO2','no2_3yr','no2_1hr',
    'SO2','so2_1yr','so2_ann',
    'SO2','so2_3yr','so2_1hr'
  ) %>%
    dplyr::filter(tolower(pollutant) == tolower(pollutant_filter))


  df_caaqs_results_ <- df_caaqs_results
  year_ <- year
  airzone_ <- airzone
  df_stations_ <- df_stations

  print(paste('Metric:',df_metric$metric[1]))
  a <- plot_bar_ranked0(df_caaqs_results = df_caaqs_results_,metric = df_metric$metric[1],df_stations = df_stations_,year = year_, airzone = airzone_)
  if (nrow(df_metric) == 2)
  {
  b <- plot_bar_ranked0(df_caaqs_results = df_caaqs_results_,metric = df_metric$metric[2],df_stations = df_stations_,year = year_, airzone = airzone_)
  b <- b + theme(legend.position = 'right')
  a <- a + theme(legend.position = 'none')
  a <- a|b
  }
return(a)
}



#' Create a ranked bar graph (backend version)
#'
#' This is the back end of the plot_bar_ranked function
plot_bar_ranked0 <- function(df_caaqs_results,metric,year,airzone = NULL,df_stations = NULL) {

  if (0) {
    df_caaqs_results <- readr::read_csv('../test_data/caaqs_results.csv')
    metric <- c('pm25_annual')
    airzone <- NULL
    df_stations = NULL
    year <- 2015
  }

  #plotly version
  df_unit_plotly <- tribble(
    ~metric,~units,
    'pm25_annual',"Annual PM<sub>2.5</sub> Metric(&mu;g/m<sup>3</sup>)",
    'pm25_24h',"24-Hour PM<sub>2.5</sub> Metric(&mu;g/m<sup>3</sup>)",
    'o3_8h',"8-Hour O<sub>3</sub> Metric (ppb)",
    'no2_1hr',"1-Hour NO<sub>2</sub> Metric (ppb)",
    'no2_ann',"Annual NO<sub>2</sub> Metric (ppb)",
    'so2_1hr',"1-Hour SO<sub>2</sub> Metric (ppb)",
    'so2_ann',"Annual SO<sub>2</sub> Metric (ppb)"
  )

  #ggplot version
  df_unit <- tribble(
    ~metric,~units,
    'pm25_annual',bquote(~"Average "~PM[2.5]~","~mu~g/m^3),
    'pm25_24h',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'pm25_ann(1yr)',bquote("Annual "~PM[2.5]~","~mu~g/m^3),
    'pm25_24hr(1yr)',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'o3_8h',bquote("4th Highest"~O[3]~",ppb"),
    'o3_8h(1yr)',bquote("4th Highest"~O[3]~",ppb"),
    'no2_1hr',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann',bquote("Average "~NO[2]~",ppb"),
    'no2_1hr(1yr)',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann(1yr)',bquote("Average "~NO[2]~",ppb"),
    'so2_1hr',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann',bquote("Average "~SO[2]~",ppb"),
    'so2_1hr(1yr)',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann(1yr)',bquote("Average "~SO[2]~",ppb")
  )

  #define the CAAQS and the axis scale limits for display purposes
  #includes 2015 and 2020 CAAQS, based on the year
  if (year>=2020) {
    df_axis <- tribble(
      ~metric,~caaqs,~lbl_caaqs,~xmin,~xlab,
      'pm25_annual',8.8,'2020 CAAQS',10,9.5,
      'pm25_24h',27,'2020 CAAQS',30,29,
      'o3_8h',62,'2020 CAAQS',70,64,
      'no2_1hr',60,'2020 CAAQS',70,62,
      'no2_ann',17,'2020 CAAQS',20,19,
      'so2_1hr',70,'2020 CAAQS',80,72,
      'so2_ann',5,'2020 CAAQS',10,6
    )
  } else {
    df_axis <- tribble(
      ~metric,~caaqs,~lbl_caaqs,~xmin,~xlab,
      'pm25_annual',10,'2015 CAAQS',10,9.5,
      'pm25_24h',28,'2015 CAAQS',30,29,
      'o3_8h',63,'2015 CAAQS',70,64,
      'no2_1hr',60,'2020 CAAQS',70,62,
      'no2_ann',17,'2020 CAAQS',20,19,
      'so2_1hr',70,'2020 CAAQS',80,72,
      'so2_ann',5,'2020 CAAQS',10,6
    )
  }

  #redefined filtering variables to avoid confusion with column names
  airzone_filter <- airzone
  metric_filter <- metric
  year_filter <- year

  if (is.null(df_stations)) {
    df_stations <- listBC_stations(use_CAAQS = TRUE, merge_Stations = TRUE)%>%
      dplyr::rename(label = Label,
                    latitude  = LAT,
                    longitude = LONG,
                    airzone = AIRZONE) %>%
      select(site,label,airzone,latitude,longitude) %>%
      group_by(site) %>%
      slice(1) %>% ungroup() %>%
      filter(!is.na(airzone))
  }
  if (is.null(airzone_filter)) {
    airzone_filter <- unique(df_stations$airzone)
  }

  #prepare data for plotting
  df <- df_caaqs_results %>%
    filter(metric %in% metric_filter) %>%
    left_join(df_stations) %>%
    filter(tolower(airzone) %in% tolower(airzone_filter)) %>%
    left_join(df_unit) %>%
    left_join(df_axis)
  df <- df %>%
    filter(year %in% year_filter)

  units <- unique(df$units)[[1]]
  xmax <- max(df$metric_value*1.1,df$xmin,na.rm = TRUE)
  caaqs <- unique(df$caaqs)[1]
  caaqs_label <- unique(df$lbl_caaqs)[1]
  xlab <- unique(df$xlab)[1]


  #identify scale limits


  #fix for stations with multiple instruments
  df <-  df %>%
    group_by(parameter,label,year,tfee) %>%
    dplyr::mutate(count =n()) %>%
    ungroup() %>%
    dplyr::mutate(label = ifelse(count >1,
                                 paste(label,'\n(',tolower(instrument),')',sep=''),
                                 label)) %>%
    select(-count)
  #set order of site
  lvls_site <- df %>%
    filter(!tfee) %>%
    arrange(metric_value) %>%
    filter(!is.na(metric_value)) %>%
    pull(label) %>%
    unique()



  if (any(df$tfee)) {

    #there is TFEE to plot

    df <-  df %>%
      mutate(label=factor(label,levels=lvls_site)) %>%
      filter(!is.na(metric_value)) %>%
      mutate(`Data Adjustment (TFEE)` = ifelse(tfee,
                                               'Wildfire-adjusted\n(wildfire data removed)',
                                               'No Adjustment\n(wildfire data included)')) %>%
      mutate(`Data Adjustment (TFEE)` = factor(`Data Adjustment (TFEE)`,levels = c(
        'Wildfire-adjusted\n(wildfire data removed)','No Adjustment\n(wildfire data included)'
      )))
    a <- df %>%
      # head(20) %>%  #debug purposes, comment out

      ggplot2::ggplot(aes(x=label, y=metric_value,fill = `Data Adjustment (TFEE)`)) +
      geom_col(position='identity',colour = 'black') +
      coord_flip() +
      ylab(units) +
      scale_y_continuous(expand = c(0,0),limits = c(0,xmax)) +
      #add CAAQS line and text
      geom_hline(yintercept = caaqs, colour = 'red', linetype = 'dashed') +
      annotate("text",x=nrow(df%>%select(site)%>%distinct())/4, y=xlab,
               label = caaqs_label, angle = 90, colour = 'red') +
      # ylab(expression(PM[2.5])) +
      theme(panel.background = element_blank(),
            panel.border = element_rect(fill=NA, colour='black'),
            axis.title.y = element_blank(),
            legend.position = 'bottom') +
      scale_fill_manual(values = c('cornflowerblue','slategray3'))

    # plotly::ggplotly(a)
  } else {
    #no TFEE to plot
    df <-  df %>%
      mutate(label=factor(label,levels=lvls_site)) %>%
      filter(!is.na(metric_value))

    a <-
      df %>%

      # head(20) %>%  #debug purposes, comment out

      ggplot2::ggplot(aes(x=label, y=metric_value)) +
      geom_col(position='dodge',colour = 'black',fill='cornflowerblue') +
      coord_flip() +
      ylab(units) +
      scale_y_continuous(expand = c(0,0),limits = c(0,xmax)) +
      #add CAAQS line and text
      geom_hline(yintercept = caaqs, colour = 'red', linetype = 'dashed') +
      annotate("text",x=nrow(df%>%select(site)%>%distinct())/4, y=xlab,
               label = caaqs_label, angle = 90, colour = 'red') +
      # ylab(expression(PM[2.5])) +
      theme(panel.background = element_blank(),
            panel.border = element_rect(fill=NA, colour='black'),
            axis.title.y = element_blank(),
            legend.position = 'none') +
      scale_fill_manual(values = c('cornflowerblue'))
  }

  return(a)

}

# Air Zone graphig functions
# Including long term trends

#' Plot Long term trends
#'
#' @param pollutant is the metric to plot
#' @param station is the station that is emphasis of plot. It can also be a vector.
#' If NULL, the result will be a list of stations in the specified air zone
#' @param airzone is the airzone
#' @param tfee defines if data is adjusted for transboundary flow and exceptional events
#' @param df is the data file containing CAAQS summary.
#' use create_caaqs_annual() to generate caaqs_results.csv
#' @param lst_stations is the list of stations, use importBC_data()
#' If NULL, it will retrieve from the local test data location
#'
plot_trends <- function(pollutant,station=NULL,airzone = NULL, adjust_tfee = FALSE,
                        df=NULL,lst_stations=NULL)  {

  if (0) {
    pollutant <- 'PM25'
    df <- NULL
    station <- NULL
    station <- 'Squamish Elementary'
    airzone <- 'All'
  }

  print(paste('pollutant:',pollutant,'station:',station,'airzone:',airzone))

  df_plot_metric <- tribble(
    ~pollutant,~metric,
    'pm25','pm25_24h',
    'pm25','pm25_annual',
    'o3','o3_8h',
    'no2','no2_ann',
    'no2','no2_1hr',
    'so2','so2_ann',
    'so2','so2_1hr'
  )

  if (!pollutant %in% df_plot_metric$pollutant) {
    print('pollutant is not on list')
    return(NULL)
  }
  pollutant <- tolower(pollutant)

  plot_metrics <- df_plot_metric$metric[tolower(df_plot_metric$pollutant) == pollutant]

  #if stations is NULL, the result is list of stations in the air zone
  if (is.null(station)) {
    df_result <- plot_trends0(plot_metric = plot_metrics[1],station = NULL,
                              airzone = airzone,df = df, lst_stations = lst_stations)
    return(df_result)
  }


  #create
  if (length(plot_metrics)==2){
    a <- plot_trends0(plot_metric = plot_metrics[1],station = station,
                      airzone = airzone,df = df, lst_stations = lst_stations) +
      xlab('')
    b <- plot_trends0(plot_metric = plot_metrics[2],station = station,
                      airzone = airzone,df = df, lst_stations = lst_stations) +
      ggtitle('')

    a <- a/b

    return(a)
  } else
  {
    a <- plot_trends0(plot_metric = plot_metrics[1],station = station,
                      airzone = airzone,df = df, lst_stations = lst_stations)
    return(a)
  }
}

#' Back end functions for plot_trends()
plot_trends0 <- function(plot_metric,station=NULL,airzone = NULL, adjust_tfee = FALSE,
                         df=NULL,lst_stations=NULL) {

  if (0) {

    a <- plot_trends0('pm25_24h',station = 'Squamish Elementary',airzone = 'All',
                      df = NULL,lst_stations=NULL)
    b <- plot_trends0('pm25_annual',station = 'Squamish Elementary',airzone = 'All',
                      df = NULL,lst_stations=NULL)
    # readr::write_csv(lst_stations,'./test_data/listbc_stations.csv')
    lst_stations <- readr::read_csv('../test_data/listbc_stations.csv')
    lst_stations <- NULL
    df <- NULL
    plot_metric <- 'pm25_24h'
    station <- 'Squamish Elementary'
    airzone <- 'All'
    adjust_tfee = FALSE
    plot_metric
  }


  if (is.null(df)) {
    df <- readr::read_csv('../test_data/caaqs_results.csv')
  }

  if (is.null(airzone)) {
    airzone <- 'All'
  }
  airzone_filter <- airzone


  #if all airzones selected, assign NULL value for airzone_filter
  if (airzone_filter == 'All') {
    airzone_filter <- sort(c('Central Interior','Northeast','Georgia Strait','Lower Fraser Valley',
                             'Southern Interior','Northwest','Coastal'))
    airzone_label <- 'All Stations'
  } else
  {
    airzone_label <- paste(airzone_filter,'Air Zone')
  }

  print(paste('Airzone:',airzone_label))
  #units
  #define the units foe each metric
  df_unit <- tribble(
    ~metric,~units,
    'pm25_annual',bquote(~"Average "~PM[2.5]~","~mu~g/m^3),
    'pm25_24h',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'pm25_ann(1yr)',bquote("Annual "~PM[2.5]~","~mu~g/m^3),
    'pm25_24hr(1yr)',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'o3_8h',bquote("4th Highest"~O[3]~",ppb"),
    'o3_8h(1yr)',bquote("4th Highest"~O[3]~",ppb"),
    'no2_1hr',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann',bquote("Average "~NO[2]~",ppb"),
    'no2_1hr(1yr)',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann(1yr)',bquote("Average "~NO[2]~",ppb"),
    'so2_1hr',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann',bquote("Average "~SO[2]~",ppb"),
    'so2_1hr(1yr)',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann(1yr)',bquote("Average "~SO[2]~",ppb")
  )

  unit <- df_unit$units[df_unit$metric %in% plot_metric][[1]]
  #retrieve only list of stations that are in the data
  if (is.null(lst_stations)) {
    lst_stations <- listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE) %>%
      dplyr::rename(label = Label,
                    latitude  = LAT,
                    longitude = LONG,
                    airzone = AIRZONE) %>%
      select(site,label,airzone,latitude,longitude) %>%
      group_by(site) %>%
      slice(1) %>% ungroup()%>%
      filter(!is.na(airzone))
  }


  df <- df %>%
    left_join(lst_stations)

  #return with list of stations if station == NULL
  if (is.null(station)) {
    if (is.null(airzone_filter)) {
      airzone_filter <- df%>%
        pull(airzone) %>%
        unique()
    }
    print('extracting list of sites')
    a <- df %>%
      filter(airzone %in% airzone_filter,
             metric %in% plot_metric) %>%
      pull(site) %>%
      unique() %>%
      sort()

    return(a)
  }

  if (0) {
    df_test <- df

  }


  #get maximum value for an airzone
  df_airzone <- df %>%
    ungroup() %>%
    filter(airzone %in% airzone_filter) %>%
    group_by(year,metric,tfee) %>%
    dplyr::mutate(max_value = max(metric_value,na.rm = TRUE),
                  min_value = min(metric_value,na.rm = TRUE)) %>%
    select(year,metric,tfee,airzone,min_value,max_value) %>%
    unique()

  # filter based on user selection
  # and extract other filtering info
  df <- df %>%
    filter(metric %in% plot_metric,
           tolower(site) %in% tolower(station),
           tfee == adjust_tfee )

  df_airzone <- df_airzone %>%
    filter(metric %in% plot_metric,
           tfee == adjust_tfee,
           airzone %in% airzone_filter) %>%
    ungroup()



  #cleanup in case of duplicate entry for station or airzone
  df_airzone <- df_airzone %>%
    group_by(year,metric,tfee,airzone) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index == 1) %>% select(-index) %>%
    arrange(airzone,metric,year) %>%
    ungroup()

  df <- df %>%
    arrange(desc(metric_value)) %>%
    group_by(site,tfee,metric,year) %>%
    dplyr::mutate(index = 1:n(), count=n()) %>%
    filter(index == 1) %>% ungroup() %>%
    select(-index, -count)

  #make the dataframe match with the airzone dataframe
  df_airzone$min_value <- ifelse(is.infinite(df_airzone$min_value),NA,df_airzone$min_value)
  df_airzone$max_value <- ifelse(is.infinite(df_airzone$max_value),NA,df_airzone$max_value)

  df_ <- df_airzone %>%
    select(year) %>%
    left_join(df) %>%
    mutate(site = station) %>%
    arrange(site,year)

  df <- df_
  #define intervals for x-axis
  years <- min(df$year): max(df$year)
  years_intervals <- floor((max(years) - min(years))/5)

  #fix for infinite values

  a <-   df %>%
    arrange(site,year) %>%
    # filter(!is.na(metric_value)) %>%
    arrange(site,year) %>%
    ggplot(aes(x=year,y=metric_value,colour = site)) +
    geom_point(size=5) +
    geom_line(size=1.5) +
    geom_ribbon(mapping = aes(x=df_airzone$year,
                              ymax=df_airzone$max_value,
                              ymin = df_airzone$min_value),alpha = 0.2,
                fill = 'grey', colour = 'black') +
    # scale_x_continuous(limits = c(min(years),max(years))) +
    scale_x_continuous(breaks = seq(from = min(years) , to = max(years), by = years_intervals)) +
    scale_y_continuous(limits = c(0,max(df_airzone$max_value, na.rm=TRUE) + 10)) +
    theme(legend.position = 'none',
          legend.title = element_blank(),
          panel.background = element_rect(fill=NA,colour = 'black')) +
    xlab('Year')+
    ylab(unit) +
    ggtitle(paste(station,'and',airzone_label))
  # annotate("text",x=min(df_airzone$year,na.rm = TRUE),
  #          y= max(df_airzone$max_value, na.rm=TRUE),
  #          label = airzone_label,
  #          hjust = 0)

  return(a)
}

#' Create CAAQS bar graph
#'
#' @description Creates the bar graph use for CAAQS
#'
#' @param df is the dataframe containing CAAQS metrics, and non-CAAQS annual metrics
#' This dataframe is created using the create_CAAQS_graph_files()
#' @param parameter is the parameter of either 'pm25','o3','no2','so2'
#' @param station is the station name
#' if NULL, result displays the available stations that can be listed
#' @param startyear is the start year to include
create_CAAQS_graph <- function(df, parameter, station = NULL, startyear = 2013) {

  if (0) {
    aq_summary1 <-  readr::read_csv('././test_data/air_data_summary.csv') %>%
      mutate(metric = recode(metric,'o3' = 'ozone'  ,'o3_tfee' = 'ozone_tfee'))

    aq_summary <-  readr::read_csv('./test_data/caaqs_results.csv')
    unique(aq_summary$metric)
    df <- aq_summary
    parameter <- 'pm2.5'
    parameter <- 'so2'
    station <- NULL
    station <- 'Williams Lake Columneetza School'
    startyear <- 2013

  }


  a <- NULL #output results


  #standardize the name of parameter
  parameter <- recode(parameter,'PM\u2082.\u2085' = 'pm25',
                      'NO\u2082' = 'no2',
                      'SO\u2082' ='so2',
                      'Ozone' = 'ozone',
                      'pm2.5' = 'pm25')
  parameter <- tolower(parameter)
  parameter <- gsub('o3','ozone',parameter,ignore.case = TRUE)
  # parameter <- gsub('pm25','pm2.5',parameter,ignore.case = TRUE)
  parameter_filter <- parameter


  #this segment added as fix to the source file
  if (0) {
    df <- aq_summary
    unique(df$metric)

    colnames(aq_summary1)
    colnames(df)
  }

  try(
    df <- df %>%
      filter(year >= startyear) %>%
      mutate(metric = gsub('pm2.5','pm25',metric,ignore.case = TRUE)) %>%
      mutate(metric = ifelse(tfee,paste(metric,'_tfee',sep=''),metric)) %>%
      mutate(metric = recode(metric,
                             'pm25_24h' = 'pm25_24h',
                             'pm25_24hr(1yr)' = 'pm25_24h (1yr)',
                             'pm25_24h_tfee' = 'pm25_24h_tfee',
                             'pm25_24hr(1yr)_tfee' = 'pm25_24h_tfee (1yr)',
                             'pm25_annual' = 'pm25_annual',
                             'pm25_ann(1yr)' = 'pm25_annual (1yr)',
                             'pm25_annual_tfee' =  'pm25_annual_tfee',
                             'pm25_ann(1yr)_tfee' = 'pm25_annual_tfee (1yr)',
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
      select(site,year,instrument,metric,metric_value) %>%
      unique())
  if (is.null(station)) {
    a <- df %>%
      filter(grepl(parameter_filter,metric,ignore.case = TRUE)) %>%
      filter(!is.na(metric_value)) %>%
      pull(site) %>% unique() %>%  sort()
    return(a)
  }

  #cleanup to remove duplicate within station
  df <-   df %>%
    ungroup() %>%
    arrange(desc(metric_value)) %>%
    group_by(site,metric,year) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index ==1) %>% select(-index) %>% ungroup() %>%
    arrange(site,year)

  #fill blanks
  df <- df %>%
    select(site,metric) %>%
    unique() %>%
    merge(tibble(year = min(df$year):max(df$year))) %>%
    left_join(df)
  #lower the case for the station
  station <- tolower(station)
  # print(station)
  if (parameter_filter == 'pm25')
  {

    aq <- df%>%
      filter(grepl('pm25',metric,ignore.case = TRUE) ) %>%
      filter(tolower(site) == tolower(station))  #PROD

    # print(nrow(aq))
    #define data for line and bar graph
    #this will be the line trend
    aq_caaqs <- aq %>%
      filter(!grepl('1yr',metric))

    #this will be the bar graph
    #recalculate so tfee is only the delta value
    aq_1yr <-
      aq %>% ungroup() %>%
      filter(grepl('1yr',metric)) %>%
      mutate(instrument = 'instrument') %>%
      tidyr::pivot_wider(names_from = metric, values_from = metric_value) %>%
      dplyr::mutate(`pm25_24h (1yr)` = `pm25_24h (1yr)` - `pm25_24h_tfee (1yr)`,
                    `pm25_annual (1yr)` = `pm25_annual (1yr)`- `pm25_annual_tfee (1yr)`
      ) %>%
      # View()
      tidyr::pivot_longer(cols = -c('site','instrument','year'),
                          names_to = 'metric', values_to = 'metric_value')


    #define the min and max of the data
    year_min = min(min(aq$year),2018)
    year_max = max(max(aq$year),2020)
    # x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    x_lbls <- year_min:year_max
    ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'pm25_24h (1yr)'],na.rm=TRUE),32)
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'pm25_annual (1yr)'],na.rm=TRUE),12)


    #managmeent levels
    # geom_rect(mapping=aes(xmin=grp_start,xmax=2019.5,ymin=28,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
    # geom_rect(mapping=aes(xmin=2019.5,xmax=2021,ymin=27,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +




    #the annual plot ------------

    p1_ann <-

      aq_caaqs  %>%
      filter(grepl('annual',metric)) %>%

      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric),fill = NA)  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=2019.5,ymin=10,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max+1,ymin=8.8,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=2019.5,ymin=6.4,ymax=10),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max + 1,ymin=6.4,ymax=8.8),alpha=0.1,color=NA,fill='#F46D43') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=4,ymax=6.4),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=4),alpha=0.1,color=NA,fill='#A6D96A') +

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
      geom_segment(aes(x=year_min-0.2,y=10,xend=2019.5,yend=10),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=2019.5,y=8.8,xend=year_max + 1,yend=8.8),colour='red2',linetype='dashed',size=1) +
      geom_segment(aes(x=2019.5,y=8.8,xend=2019.5,yend=10),colour='red2',linetype='dashed',size=1)+


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=10+1,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
      annotate("text",x= year_max,y=8.8+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust=1) +

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
            # axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +
      # theme_minimal() +

      ylim(0,ymax_ann) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
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
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max - 0.5,ymin=28,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_max - 0.5,xmax=year_max + 1,ymin=27,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max - 0.5,ymin=19,ymax=28),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_max - 0.5,xmax=year_max + 1,ymin=19,ymax=27),alpha=0.1,color=NA,fill='#F46D43') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=10,ymax=19),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=10),alpha=0.1,color=NA,fill='#A6D96A') +

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
      geom_segment(aes(x=year_min-0.2,y=28,xend=year_max - 0.5,yend=28),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=year_max - 0.5,y=27,xend=year_max + 1,yend=27),colour='red2',linetype='dashed',size=1) +
      geom_segment(aes(x=year_min-0.2,y=28,xend=year_max - 0.5,yend=28),colour='red2',linetype='dashed',size=1)+

      #labels to CAAQS reference lines
      annotate("text",x= year_min+1,y=28+4,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
      annotate("text",x= year_max,y=27+4,label = '2020 CAAQS',colour = 'white',angle =0,hjust=1) +

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

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
      scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
      scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
      scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
      xlab('Year')
    # theme(legend.position = 'none')
    a <- print(p1_ann/p1_24h)
  }

  if (parameter_filter == 'ozone')
  {
    aq <- df%>%
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
    # x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    x_lbls <- year_min:year_max

    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'ozone_4th (1yr)'],na.rm=TRUE),70)



    p1_ann <-

      aq_caaqs  %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=2019.5,ymin=63,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max+1,ymin=62,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=2019.5,ymin=56,ymax=63),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max + 1,ymin=56,ymax=62),alpha=0.1,color=NA,fill='#F46D43') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=50,ymax=56),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=30,ymax=50),alpha=0.1,color=NA,fill='#A6D96A') +

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
      geom_segment(aes(x=year_min-0.2,y=63,xend=2019.5,yend=63),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=2019.5,y=62,xend=year_max + 1,yend=62),colour='red2',linetype='dashed',size=1) +
      geom_segment(aes(x=2019.5,y=62,xend=2019.5,yend=63),colour='red2',linetype='dashed',size=1)+


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=63+2,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
      annotate("text",x= year_max,y=62+2,label = '2020 CAAQS',colour = 'white',angle =0,hjust=1) +

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
      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,year_max+1),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(30,ymax_ann), expand = c(0,0),oob=rescale_none,trans = 'reverse')+
      scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
      scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

      scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted')) +
      xlab('Year')

    a <- print(p1_ann)

  }

  if (parameter_filter== 'no2')
  {
    aq <- df%>%
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
    # x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    x_lbls <- year_min:year_max
    #note that ymax_24h is 1-hour metric
    ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'no2_1hr (1yr)'],na.rm=TRUE),70)
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'no2_annual (1yr)'],na.rm=TRUE),20)


    p1_ann <-
      aq_caaqs  %>%
      filter(grepl('annual',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=17,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=7,ymax=17),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=2,ymax=7),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=2),alpha=0.1,color=NA,fill='#A6D96A') +

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
      geom_segment(aes(x=year_min-0.2,y=17,xend=year_max + 1,yend=17),colour='red2',linetype='dashed',size=1) +


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=17+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust =1) +


      # xlab('Reporting Period')+
      ylab(expression(paste('NO'[2],' Annual Metric (ppb)'))) +

      labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'none', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_ann) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
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
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=60,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=31,ymax=60),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=20,ymax=31),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=20),alpha=0.1,color=NA,fill='#A6D96A') +

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
      geom_segment(aes(x=year_min-0.2,y=60,xend=year_max + 1,yend=60),colour='red2',linetype='dashed',size=1)+

      #labels to CAAQS reference lines
      annotate("text",x= year_min+1,y=65,label = '2020 CAAQS',colour = 'white',angle =0,hjust =1) +

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

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
      scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
      scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
      scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
      xlab('Year')


    a <- print(p1_ann/p1_24h)

  }

  if (parameter_filter == 'so2')
  {
    aq <- df%>%
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
    # x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    x_lbls <- year_min:year_max
    #note that ymax_24h is 1-hour metric
    ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'so2_1hr (1yr)'],na.rm=TRUE),75)
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'so2_annual (1yr)'],na.rm=TRUE),8)


    p1_ann <-
      aq_caaqs  %>%
      filter(grepl('annual',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=5,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=3,ymax=5),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=2,ymax=3),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=2),alpha=0.1,color=NA,fill='#A6D96A') +

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
      geom_segment(aes(x=year_min-0.2,y=5,xend=year_max + 1,yend=5),colour='red2',linetype='dashed',size=1) +


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=5+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust =1) +


      # xlab('Reporting Period')+
      ylab(expression(paste('SO'[2],' Annual Metric (ppb)'))) +

      labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'none', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_ann) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
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
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=70,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=50,ymax=70),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=30,ymax=50),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=30),alpha=0.1,color=NA,fill='#A6D96A') +

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
      geom_segment(aes(x=year_min-0.2,y=70,xend=year_max + 1,yend=70),colour='red2',linetype='dashed',size=1)+

      #labels to CAAQS reference lines
      annotate("text",x= year_min+1,y=65,label = '2020 CAAQS',colour = 'white',angle =0,hjust =1) +

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

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
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

#' Create a graph for the emissions from each airzone
#'
#' @param pollutant is either pm25, no2, or so2
#' @param df is the dataframe containing NPRI data
#' @param airzone is the airzone. default of 'All' means sum of all values
#'
plot_npri_airzone <- function(pollutant,df, airzone = 'All') {

  if(0) {
    pollutant <- 'pm25'
    df <- readr::read_csv('../test_data/NPRI.csv')
    airzone <- 'Southern Interior'
  }
  require(ggplot2)
  require(plotly)
  airzone_filter <- airzone
  pollutant_filter <- pollutant

  if (airzone == 'All') {
    df_result <-  df %>%
      group_by(parameter,year,sector) %>%
      dplyr::summarise(emissions = sum(metric,na.rm = TRUE)) %>%
      filter(emissions>0)
  } else {
    df_result <-  df %>%
      group_by(parameter,year,sector,airzone) %>%
      dplyr::summarise(emissions = sum(metric,na.rm = TRUE)) %>%
      filter(airzone %in% airzone_filter) %>%
      filter(emissions>0)

  }

  #fix for Na sector
  df_result <- df_result %>%
    dplyr::mutate(sector = ifelse(is.na(sector),'Others',sector)) %>%
    filter(parameter %in% pollutant_filter)

  a <-
    plot_ly(df_result,x=~year, y= ~emissions, color = ~sector, type = 'bar', source = 'scatter',
            marker = list(line = list(width = 1,color = 'rgb(0, 0, 0)'))
    ) %>%
    layout(barmode = 'stack',yaxis = list(title = paste(pollutant,'(tonnes/year)')))

  return(a)
}
