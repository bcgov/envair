# Copyright 2020 Province of British Columbia
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

# app_ui <- function() {
## this is the shell of an app.R scriptwith the bcgov style template
## ensure that the .css file and logo are in the www folder in your app directory.
# Define UI ----
parameters <- c('PM\u2082.\u2085',
                'Ozone',
                'NO\u2082',
                'SO\u2082')

source('01_load.R')
df_parameter <- tribble(
  ~display, ~parameter,
  'PM\u2082.\u2085','pm25',
  'Ozone','o3',
  'NO\u2082','no2',
  'SO\u2082','so2'
)
# parameters <- c('<h5>PM<sub>2.5</sub></h5>',
#                'O<sub>3</sub>',
#                'NO<sub>2</sub>',
#                'SO<sub>2</sub>')
parameters_npri <- c('pm25','pm10','nh3','nox','sox'
)

airzones <- sort(c('Central Interior','Northeast','Georgia Strait','Lower Fraser Valley',
                   'Southern Interior','Northwest','Coastal'))

metrics <- c('pm25_24h','pm25_annual','o3_8h','no2_1hr','no2_ann','so2_1hr','so2_ann')

ui <- fluidPage(
  titlePanel(""),
  navbarPage(title = "B.C. Air Zone Report", theme = "../azreport_ui/www/bcgov.css", # add yellow highlighted text to theme the navigation bar
             tabPanel("Station Summary",
                      ## add this chunk for the footer =================================
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             tags$footer(class="footer",
                                         tags$div(class="container",style="display:flex; justify-content:center; flex-direction:column;
                              text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home",
                                                                    style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")))))),

                      ## end of footer chunk =========================================
                      selectInput("Parameter", "Select Pollutant to Display:",
                                  list(`Parameter` = parameters)
                      ),
                      uiOutput("stationSelect"),
                      plotOutput("plot1"),
                      plotly::plotlyOutput("plot2")
             ),
             tabPanel('Emission Inventory',
                      ## add this chunk for the footer =================================
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             tags$footer(class="footer",
                                         tags$div(class="container",style="display:flex; justify-content:center; flex-direction:column;
                              text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home",
                                                                    style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")))))),

                      ## end of footer chunk =========================================

                      selectInput("pollutant", "Select Air Pollutant to display:",
                                  list(`Parameter` = parameters_npri)
                      ),
                      plotly::plotlyOutput('plot3')

             ),
             tabPanel('Long Term Trends',
                      ## add this chunk for the footer =================================
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             tags$footer(class="footer",
                                         tags$div(class="container",style="display:flex; justify-content:center; flex-direction:column;
                              text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home",
                                                                    style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")))))),

                      ## end of footer chunk =========================================
                      selectInput("pollutant trend",'Select Air Pollutant to display',
                                  list(`Parameter` = df_parameter$display)
                      ),
                      selectInput("airzone",'Select Airzone',
                                  list(`Air Zone` = c('All',airzones))
                      ),
                      uiOutput("stationSelect2"),
                      plotOutput('plot4')

             ),
             tabPanel('Map',
                      ## add this chunk for the footer =================================
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             tags$footer(class="footer",
                                         tags$div(class="container",style="display:flex; justify-content:center; flex-direction:column;
                              text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home",
                                                                    style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")))))),

                      ## end of footer chunk =========================================
                      leaflet::leafletOutput("mymap2"),
                      p()
                      # actionButton("recalcPM25", "PM2.5"),
                      # actionButton("recalcOzone", "Ozone"),
                      # actionButton("recalcNO2", "NO2"),
                      # actionButton("recalcSO2", "SO2")


             ),
             tabPanel('Bar Graph',
                      ## add this chunk for the footer =================================
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             tags$footer(class="footer",
                                         tags$div(class="container",style="display:flex; justify-content:center; flex-direction:column;
                              text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home",
                                                                    style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")))))),

                      ## end of footer chunk =========================================
                      selectInput("pollutants_bar","Select Pollutant to Display",
                                  list(`Metrics` = parameters)
                      ),
                      selectInput("airzone_bar",'Select Airzone',
                                  list(`Air Zone` = airzones)
                      ),
                      sliderInput("year_bar",label = 'Year',
                                  min = min(yearlist), max = max(yearlist),
                                  value = max(yearlist),sep=''
                      ),
                      # selectInput("year_bar",'Select Year',
                      #             list(`Year` = yearlist)

                      plotOutput('plot5')
             ),
             tabPanel('Air Zone Emission Sources',
                      ## add this chunk for the footer =================================
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             tags$footer(class="footer",
                                         tags$div(class="container",style="display:flex; justify-content:center; flex-direction:column;
                              text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home",
                                                                    style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")))))),

                      ## end of footer chunk =========================================

                      selectInput("pollutant_emission_airzone", "Select Air Pollutant to display:",
                                  list(`Parameter` = df_parameter$display)
                      ),
                      selectInput("AirZone_emission", "Select Air Zone:",
                                  list(`Airzone` = c('All',airzones))
                      ),
                      plotly::plotlyOutput('plot6')

             ),
             tabPanel('Wood Smoke Reduction Program',
                      ## add this chunk for the footer =================================
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             tags$footer(class="footer",
                                         tags$div(class="container",style="display:flex; justify-content:center; flex-direction:column;
                              text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home",
                                                                    style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")))))),

                      ## end of footer chunk =========================================
                      selectInput("Woodstove_exchange", "Select Air Zone:",
                                  list(`Airzone` = c('All',airzones))
                      ),
                      plotly::plotlyOutput('plot7')

             )))


# }
