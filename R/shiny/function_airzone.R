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

# Air Zone graphig functions
# Including long term trends

#' Plot Long term trends
#'
#' @param plot_metric is the metric to plot
#' @param station is the station that is emphasis of plot. It can also be a vector.
#' If NULL, the result will be a list of stations in the specified air zone
#' @param airzone is the airzone
#' @param tfee defines if data is adjusted for transboundary flow and exceptional events
#' @param df is the data file
#' @param lst_stations is the list of stations, use importBC_data()
#' If NULL, it will retrieve from the local test data location
plot_trends <- function(plot_metric,station=NULL,airzone = NULL, adjust_tfee = FALSE,
                        df=NULL,lst_stations=NULL) {

  if (0) {
    # readr::write_csv(lst_stations,'./test_data/listbc_stations.csv')
    lst_stations <- readr::read_csv('./test_data/listbc_stations.csv')
    plot_metric <- 'pm25_24h'
    station <- 'Squamish Elementary'
    adjust_tfee = FALSE
station <- NULL
df <- NULL
  }
  if (is.null(df)) {
    df <- readr::read_csv('./test_data/caaqs_results.csv')
  }

 print(paste('Looking for:',station,airzone))
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

  unit <- df_unit$units[df_unit$metric == plot_metric][[1]]
  #retrieve only list of stations that are in the data
  if (is.null(lst_stations)) {
    lst_stations <- listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE)
  }
  lst_stations <- lst_stations %>%
    filter(site %in% df$site) %>%
    arrange(STATUS) %>%
    group_by(site) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index ==1) %>% select(-index) %>% ungroup() %>%
    select(site,Label, LAT,LONG,AIRZONE)

  df <- df %>%
    left_join(lst_stations)
  #retrieve list of stations if station == NULL
  if (is.null(station)) {
    if (is.null(airzone)) {
      airzone <- df%>%
        pull(AIRZONE) %>%
        unique()
    }
    print('extracting list of sites')
    a <- df %>%
      filter(AIRZONE %in% airzone,
             metric == plot_metric) %>%
      pull(site) %>%
      unique() %>%
      sort()

    return(a)
  }



  #get maximum value for an airzone
  df_airzone <- df %>%
    group_by(AIRZONE,year,metric,tfee) %>%
    dplyr::summarise(max_value = max(metric_value,na.rm = TRUE),
                     min_value = min(metric_value,na.rm = TRUE))



  # filter based on user selection
  # and extract other filtering info
  df <- df %>%
    filter(metric %in% plot_metric,
           tolower(site) %in% tolower(station),
           tfee == adjust_tfee )
 if (is.null(airzone)) {
   airzone <- df %>%
     pull(AIRZONE) %>%
     unique()
 }
  df_airzone <- df_airzone %>%
    filter(metric %in% plot_metric,
           tfee == adjust_tfee,
           AIRZONE %in% airzone)



  #cleanup in case of duplicate entry for station
  df <- df %>%
    arrange(desc(metric_value)) %>%
    group_by(site,tfee,metric,year) %>%
    dplyr::mutate(index = 1:n(), count=n()) %>%
    filter(index == 1) %>% ungroup() %>%
    select(-index, - count)

  #make the dataframe match with the airzone dataframe
  df_ <- df_airzone %>%
    select(year) %>%
    left_join(df)

  df <- df_
  years <- min(df$year): max(df$year)
  a <- df %>%
    arrange(site,year) %>%
    ggplot(aes(x=year,y=metric_value,colour = site,fill = site)) +
    geom_point() +
    geom_line() +
    geom_ribbon(mapping = aes(x=df_airzone$year,
                              ymax=df_airzone$max_value,
                              ymin = df_airzone$min_value),alpha = 0.2,
                fill = 'grey', colour = 'black') +
    scale_x_continuous(breaks = years) +
    scale_y_continuous(limits = c(0,max(df_airzone$max_value, na.rm=TRUE) + 10)) +
    theme(legend.position = 'none',
          panel.background = element_rect(fill=NA,colour = 'black')) +
    xlab('Year')+
    ylab(unit)

  return(a)
}
