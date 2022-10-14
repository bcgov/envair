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
library(leaflet)
library(patchwork)
library(ggpattern)
library(scales)
library(envreportutils)
library(plotly)


source('01_load.R')

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


# parameters <- c('PM\u2082.\u2085',
#                 'Ozone',
#                 'NO\u2082',
#                 'SO\u2082')

parameter <- c('PM<sub>2.5</sub>',
               'O<sub>3</sub>',
               'NO<sub>2</sub>',
               'SO<sub>2</sub>')
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

  #Create plots for long term trends ----
  output$plot4 <- renderPlot({
    plot_trends(plot_metric = input$`pollutant trend`,
                station=input$Station2,airzone = input$airzone,
                df=df_caaqs,lst_stations = lst_stations)
  })

  #create for map-----




  #list of stations-----
  df_current_list_all <- df_preload_management %>%
    filter(year == max(df_preload_management$year)) %>%
    select(site,latitude,longitude,parameter) %>%
    distinct() %>%
    group_by(site) %>%
    dplyr::mutate(parameter = paste0(unique(parameter),collapse = ',')) %>%
    slice(1) %>% ungroup()

  #assets
  df_current_list <-  df_preload_management %>%
    left_join(df_metric_list() %>%select(-parameter)) %>%
    select(site,tfee,instrument,year,latitude,longitude,pollutant,metric,metric_value,airzone,colour_order,colour,label,colour_text) %>%
    ungroup() %>%
    group_by(site,year,pollutant,tfee) %>%
    arrange(desc(metric),desc(colour_order)) %>%
    dplyr::mutate(mgmt = max(colour_order,na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::filter(mgmt == colour_order) %>%
    group_by(site,year,pollutant,tfee) %>%
    slice(1) %>% select(-mgmt) %>% ungroup()

  df_current_list_pm25 <- df_current_list %>%
    filter(year == max(df_current_list$year)) %>%
    filter(pollutant == 'PM25') %>%
    filter(!tfee)

  df_current_list_pm25_tfee <- df_current_list %>%
    filter(year == max(df_current_list$year)) %>%
    filter(pollutant == 'PM25')%>%
    filter(tfee)

  df_current_list_o3 <- df_current_list %>%
    filter(year == max(df_current_list$year)) %>%
    filter(pollutant == 'O3')

  df_current_list_no2 <- df_current_list %>%
    filter(year == max(df_current_list$year)) %>%
    filter(pollutant == 'NO2')

  df_current_list_so2 <- df_current_list %>%
    filter(year == max(df_current_list$year)) %>%
    filter(pollutant == 'SO2') %>%
    mutate(icon = '../assets/marker_orange.svg')


  #create the airzone background-----
  az_mgmt <- airzones() %>%
    st_make_valid() %>%
    st_transform(st_crs(bc_bound())) %>%
    st_intersection(st_geometry(bc_bound())) %>%
    group_by(airzone = Airzone) %>%
    summarize() %>%
    st_transform(4326)

#for guide on map: https://rstudio.github.io/leaflet/showhide.html



  output$mymap2 <- renderLeaflet({
    leaflet(width = "900px", height = "700px",
            options = leafletOptions(minZoom = 5)) %>%
      set_bc_view() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      add_bc_home_button() %>%

      #add monitoring stations
      addMarkers(lng = df_current_list_pm25_tfee$longitude,
                 lat = df_current_list_pm25_tfee$latitude,
                 popup = df_current_list_pm25_tfee$site,
                 group = 'tfee',
                 layerId = paste(df_current_list_pm25_tfee$site,"PM<sub>2.5</sub>",sep='')
                 )%>%
      addMarkers(lng = df_current_list_pm25$longitude,
                 lat = df_current_list_pm25$latitude,
                 popup = df_current_list_pm25$site,
                 group = 'Include wildfires',
                 layerId = paste(df_current_list_pm25_tfee$site,"PM<sub>2.5</sub>",sep='')
      )%>%
      addMarkers(lng = df_current_list_o3$longitude,
                 lat = df_current_list_o3$latitude,
                 popup = df_current_list_o3$site,
                 group = 'No_tfee',
                 layerId =  "O<sub>3</sub>"

                 ) %>%

      addMarkers(lng = df_current_list_no2$longitude,
                 lat = df_current_list_no2$latitude,
                 popup = df_current_list_no2$site,
                 group = 'No_tfee',
                 layerId = "NO<sub>2</sub>") %>%

      addMarkers(lng = df_current_list_so2$longitude,
                 lat = df_current_list_so2$latitude,
                 popup = df_current_list_so2$site,
                 icon = list(iconURL =  df_current_list_so2$icon),
                 layerId = "SO<sub>2</sub>",
                 group = 'Include wildfires') %>%

      #add background air zone, colours
     addPolygons(data = az_mgmt,
                 color = "white", weight = 2, opacity = 1, fillOpacity = 0.7) %>%

      addLayersControl(
        baseGroups = c("PM<sub>2.5</sub>", "O<sub>3</sub>", "NO<sub>2</sub>","SO<sub>2</sub>"),
        overlayGroups = c("Include wildfires"),
        options = layersControlOptions(collapsed = FALSE)
        )%>%

      # Legend (note that for each pollutant)
      addLegend("bottomleft", group = "PM<sub>2.5</sub>",
                data = az,
                # Ensure we get all levels
                colors = rev(labels_mgmt$colour),
                labels = rev(labels_mgmt$labels),
                opacity = 1,
                title = htmltools::HTML(
                  "<h4>PM<sub>2.5</sub> Management Levels</h4>"))
      #added check box selection tool
      #reference: https://stackoverflow.com/questions/62817005/r-leaflet-assign-multiple-groups-to-a-layer-to-filter-data-and-change-column-re
    #   htmlwidgets::onRender("
    # function(el, x) {
    #   var myMap = this;
    #   var baseLayer = 'PM<sub>2.5</sub>';
    #   myMap.eachLayer(function(layer){
    #     var id = layer.options.layerId;
    #     if (id){
    #       if ('PM<sub>2.5</sub>' !== id.substring(1,)){
    #         layer.getElement().style.display = 'none';
    #       }
    #     }
    #   })
    #   console.log(myMap.baselayer);
    #   myMap.on('baselayerchange',
    #     function (e) {
    #       baseLayer=e.name;
    #       myMap.eachLayer(function (layer) {
    #           var id = layer.options.layerId;
    #           if (id){
    #             if (e.name !== id.substring(1,)){
    #               layer.getElement().style.display = 'none';
    #               layer.closePopup();
    #             }
    #             if (e.name === id.substring(1,)){
    #               layer.getElement().style.display = 'block';
    #             }
    #           }
    #
    #       });
    #     })
    #     myMap.on('overlayadd', function(e){
    #       myMap.eachLayer(function(layer){
    #         var id = layer.options.layerId;
    #         if (id){
    #             if (baseLayer !== id.substring(1,)){
    #               layer.getElement().style.display = 'none';
    #             }
    #         }
    #       })
    #     })
    # }")
  })

  #create for air zone map-----


}
