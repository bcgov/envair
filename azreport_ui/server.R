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

library(DT)
library(shiny)
library(dplyr)
library(lubridate)
library(tidyverse)
library(envair)
library(leaflet)
library(patchwork)
library(ggpattern)
library(scales)


aq_summary <-  readr::read_csv('../test_data/caaqs_results.csv')
df_npri <- readr::read_csv('../test_data/EN_APEI-Can-Prov_Terr.csv')
lst_stations <- listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE)
df_caaqs <- readr::read_csv('../test_data/caaqs_results.csv')

stationlist <- aq_summary %>%
  pull(site) %>%
  unique() %>%
  sort() %>%
  list()

yearlist <- aq_summary %>%
  pull(year) %>%
  unique() %>%
  sort() %>%
  list() %>%
  unlist()

parameterlist <- aq_summary %>%
  pull(metric) %>%
  unique() %>%
  sort() %>%
  list() %>%
  unlist()


parameters <- c('PM\u2082.\u2085',
                'Ozone',
                'NO\u2082',
                'SO\u2082')
server <-  function(input, output) {

  # developer lines to create summary
  if (0)
  {
    Parameter <- 'o3_tfee'
    year <- 2020

    aq_summary %>%
      dplyr::filter(metric == Parameter,
                    year == year) %>%
      filter(site == 'Victoria Topaz') %>%
      ggplot(aes(x=site, y=metric_value)) +
      geom_col()
    source('././R/shiny/functions_prepareshiny.R')
  }

  #Bar graph summary----
  #filter station list based on parameter
  output$stationSelect <- renderUI({
    site_list <- create_CAAQS_graph(aq_summary,parameter = input$Parameter)
    selectInput("Station","Select Station to Display:",choices = site_list)
  })


  output$plot1 <- renderPlot({

    create_CAAQS_graph(aq_summary,parameter = input$Parameter,
                       station = input$Station,
                       startyear = 2013)

  })

  #NPRI----

  output$plot3 <- renderPlotly({
    plot_npri(input$pollutant,df = df_npri, output = 'plotly')
  })

  #Long Term Trends-----
  output$stationSelect2 <- renderUI({
    site_list2 <- plot_trends(plot_metric = input$`pollutant trend`,station = NULL,
                             airzone = input$airzone,df=df_caaqs,lst_stations = lst_stations)
    selectInput("Station2","Select Station to Display:",choices = site_list2)
  })

  output$plot4 <- renderPlot({
    plot_trends(plot_metric = input$`pollutant trend`,
                station=input$Station2,airzone = input$airzone,
                df=df_caaqs,lst_stations = lst_stations)
  })
}
