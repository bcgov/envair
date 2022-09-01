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

#' Calculate the statistucs for the pollutants based on CAAQS
#'
#' Note that these are based on the metrics defined by the CCME
#' Guidance Document of Achievement Determination
#'
#' @param param is the
#' @param datetime is a string defining the datetime field. This field shoule be in time-ending format.
#' @examples
#'
#' @export
#'
get_stats <- function(param, years=NULL)
{
  if (0) {

    source('./r/importbc_data.R')
    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')


    years <- NULL

  }

  #rename the parameter entries
  param <- tolower(param)
  param <- gsub('ozone','o3',param)
  param <- gsub('pm2.5','pm25',param)


  #assigning default values
  if (is.null(years)){
    years <- lubridate::year(Sys.Date())
  }
  df <- importBC_data()


}


