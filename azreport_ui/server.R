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




parameters <- c('PM\u2082.\u2085',
                'Ozone',
                'NO\u2082',
                'SO\u2082')

# parameter <- c('PM<sub>2.5</sub>',
#                'O<sub>3</sub>',
#                'NO<sub>2</sub>',
#                'SO<sub>2</sub>')
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
  }, height =400, width=600)

  #NPRI----

  output$plot3 <- renderPlotly({
    plot_npri(input$pollutant,df = df_npri, output = 'plotly')
  })

  #Long Term Trends-----
  output$stationSelect2 <- renderUI({
    input_parameter <- df_parameter$parameter[df_parameter$display == input$`pollutant trend`]
    site_list2 <- plot_trends(pollutant = input_parameter,
                              station = NULL,
                             airzone = input$airzone,df=df_caaqs,lst_stations = lst_stations)
    selectInput("Station2","Select Station to Display:",choices = site_list2)
  })

  #Create plots for long term trends ----
  output$plot4 <- renderPlot({
    input_parameter <- df_parameter$parameter[df_parameter$display == input$`pollutant trend`]
    plot_trends(pollutant = input_parameter,
                station=input$Station2,airzone = input$airzone,
                df=df_caaqs,
                lst_stations = lst_stations)
  }, height = 800, width = 600)






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
                 popup = df_current_list_pm25_tfee$popup,
                 group = 'PM<sub>2.5</sub>',
                 layerId = paste(df_current_list_pm25_tfee$site,"PM<sub>2.5</sub>",sep='')
                 )%>%
      # addMarkers(lng = df_current_list_pm25$longitude,
      #            lat = df_current_list_pm25$latitude,
      #            popup = df_current_list_pm25$site,
      #            group = 'PM<sub>2.5</sub>',
      #            layerId = paste(df_current_list_pm25_tfee$site,"PM<sub>2.5</sub>",sep='')
      # )%>%
      addMarkers(lng = df_current_list_o3$longitude,
                 lat = df_current_list_o3$latitude,
                 popup = df_current_list_o3$popup,
                 group = 'O<sub>3</sub>',
                 layerId =  paste(df_current_list_pm25_tfee$site,"O<sub>3</sub>",sep='')

                 ) %>%

      addMarkers(lng = df_current_list_no2$longitude,
                 lat = df_current_list_no2$latitude,
                 popup = df_current_list_no2$popup,
                 group = 'NO<sub>2</sub>',
                 layerId = paste(df_current_list_pm25_tfee$site,"NO<sub>2</sub>",sep='')
                 )%>%

      addMarkers(lng = df_current_list_so2$longitude,
                 lat = df_current_list_so2$latitude,
                 popup = df_current_list_so2$popup,
                 # icon = ~oceanIcons[colour_text],
                 # icon = list(iconURL =  df_current_list_so2$icon),
                 layerId = paste(df_current_list_pm25_tfee$site,"SO<sub>2</sub>",sep=''),
                 group = 'SO<sub>2</sub>') %>%

      #add background air zone, colours


      # addPolygons(data = az_mgmt,
      #             color = 'black', weight = 2, opacity = 1, fillOpacity = 0) %>%

      #PM25 and other pollutants
      addPolygons(data = az_mgmt%>%filter(parameter == 'pm25'),
                  color = ~colour, weight = 0, opacity = 0, fillOpacity = 1,
                  group = 'PM<sub>2.5</sub>') %>%
      addPolygons(data = az_mgmt%>%filter(parameter == 'o3'),
                  color = ~colour, weight = 0, opacity = 0, fillOpacity = 1,
                  group = 'O<sub>3</sub>') %>%
     addPolygons(data = az_mgmt%>%filter(parameter == 'no2'),
                 color = ~colour, weight = 0, opacity = 0, fillOpacity = 1,
                 group = 'NO<sub>2</sub>') %>%
      addPolygons(data = az_mgmt%>%filter(parameter == 'so2'),
                  color = ~colour, weight = 0, opacity = 0, fillOpacity = 1,
                  group = 'SO<sub>2</sub>') %>%
      #lines only
      addPolylines(data = az_mgmt,color = 'black',weight = 2) %>%

      addLayersControl(
        baseGroups = c("PM<sub>2.5</sub>", "O<sub>3</sub>", "NO<sub>2</sub>","SO<sub>2</sub>"),
        # overlayGroups = c("Include wildfires"),
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

  #create for bar graph----
  output$plot5 <- renderPlot({
    plot_bar_ranked(df_caaqs_results = df_caaqs,df_stations = lst_stations,
                    pollutant = df_parameter$parameter[df_parameter$display == input$pollutants_bar],
                    airzone = input$airzone_bar,year = input$year_bar)
  }, height = 400, width = 600)


  #create interactive bar graph for airzone
  output$plot6 <- renderPlotly({
    plot_npri_airzone(pollutant = df_parameter$parameter[df_parameter$display == input$pollutant_emission_airzone],
                      df = df_NPRI,
                      airzone = input$AirZone_emission)
  })

  #create interactive bar graph for airzone
  output$plot7 <- renderPlotly({
    plot_woodstove(df = df_woodstove,
                      airzone = input$Woodstove_exchange)
  })
}
