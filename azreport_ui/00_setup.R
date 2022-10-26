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

#This file is a required file to initiate server.R

library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
# library("ggtext")
library("stringr")
# library('envair')
library("sf")
library("bcmaps")
library('plotly')
library("rcaaqs")
library("envreportutils")




#Air Zone Graphing Functions----



#' List of metric and parameters
#'
#' @description This contains a list of metrics and the parameters these are related to.
#' Note that there are two types of parameters listed, these are created to ensure coverage for all
#' envair and rcaaqs have different list of metrics
#' parameter is from rcaaqs
#' metric is from envair
#'
#' @export
#'
df_metric_list <- function() {
  #define levels to put metrics and parameters in order
  levels_parameter <- c('pm2.5_annual','pm2.5_24h','o3','no2_1yr','no2_3yr','so2_1yr','so2_3y')
  levels_metric <- c('pm25_annual','pm25_24h','o3_8h','no2_ann','no2_1hr','so2_ann','so2_1hr')
  df_result <- tribble(
    ~pollutant,~parameter,~metric,
    'PM25','pm2.5_annual','pm25_annual',
    'O3','o3','o3_8h',
    'PM25','pm2.5_24h','pm25_24h',
    'NO2','no2_1yr','no2_ann',
    'NO2','no2_3yr','no2_1hr',
    'SO2','so2_1yr','so2_ann',
    'SO2','so2_3yr','so2_1hr'
  )

  df_result$parameter <- factor(df_result$parameter, levels = levels_parameter)
  df_result$metric <- factor(df_result$metric, levels = levels_metric)

  return(df_result)
}


