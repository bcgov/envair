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


#' Calculates the CAAQS values for a particular year and parameter
#'
#' @param parameter is the pollutant that will be processed, one pollutant at a time.
#' @param years are the years that will be included in the calculation of CAAQS values
#' @examples
#' get_caaqs_metrics('pm25',years = 2017:2020)
#'
#' @export
get_caaqs_metrics <- function(parameter,years) {
  if (0) {

    source('./r/importbc_data.R')
    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    source('./r/get_caaqs_stn_history.R')
    source('./r/envairfunctions.R')
    source('./r/importBC_data_avg.R')
    source('./r/get_data_completeness.R')
    source('./r/parallel_process.R')
    parameter <- 'no2'
    years <- 2023:2024
  }


  # -define the minimum and maximum years for data download and processing
  min_yrs <- min(years)-2
  max_yrs <- max(years)

  # -define new variable for the parameter, allows defining in filter
  parameter_select <- parameter

  #- simplified definition of metrics
  #- narrow down the metric names to simple terms (e.g., annual, 24h, 1h, 8h)
  df_metric <- tribble(
    ~metric_label,~metric_category,
    '98p_24h','24h',
    'mean','annual',
    'd1hm','1h',
    'd8hm','8h'
  )

  # -data completeness
  # -define the data count category requirement
  # -these will be the same category
  df_data_requirement <- tribble(
    ~parameter,~metric,~data_count_category,
    'PM25','annual','perc_days',
    'PM25','annual','perc_days_quarter',
    'PM25','24h','perc_days',
    'PM25','24h','perc_days_quarter',
    'NO2','annual','perc_hrs',
    'NO2','annual','perc_hrs_quarter',
    'NO2','1h','perc_days',
    'NO2','1h','perc_days_quarter',
    'SO2','annual','perc_hrs',
    'SO2','annual','perc_hrs_quarter',
    'SO2','1h','perc_days',
    'SO2','1h','perc_days_quarter',
    'O3','8h','perc_q2+q3'
  )


  # -retrieve relevant info, data, and data captures info
  aq_data <- importBC_data_avg(parameter = parameter_select,years = min_yrs:max_yrs,
                               flag_TFEE = TRUE, merge_stations =  TRUE)



  cols_all <- colnames(aq_data)
  cols_data_rounded <- cols_all[grepl('rounded_',cols_all)]
  cols_data_raw <- cols_all[grepl('raw_',cols_all)]


  aq_data_wide <- aq_data %>%
    select(-any_of(cols_data_rounded)) %>%
    pivot_longer(cols = any_of(cols_data_raw)) %>%
    mutate(tfee = grepl('tfee',name,ignore.case = TRUE)) %>%
    rename(metric = name)

  # -categorize the metrics
  df_aq_data_metric <- tibble(metric = unique(aq_data_wide$metric))
  df_aq_data_metric <- df_aq_data_metric %>%
    cross_join(df_metric) %>%
    mutate(index = 1:n()) %>%
    group_by(index) %>%
    filter(grepl(metric_label,metric)) %>%
    ungroup() %>%
    select(metric,metric_category)

  # -merge the caaqs values to the data, determine if exceeded
  caaqs <- get_caaqs(years = unique(aq_data$year)) %>%
    filter(parameter %in% toupper(parameter_select))


  aq_data_wide <- aq_data_wide %>%
    left_join(df_aq_data_metric) %>%
    select(-metric) %>%
    rename(metric = metric_category) %>%
    left_join(caaqs, by=c('parameter','metric','year'))


  # merge with data completeness
  aq_completeness <- get_data_completeness(parameter = parameter_select,
                                           years = min_yrs:max_yrs,merge_Stations = TRUE)



  aq_completeness_summary <- aq_completeness$annual_days %>%
    bind_rows(aq_completeness$quarter_days) %>%
    bind_rows(aq_completeness$`quarter_q2+q3`) %>%
    bind_rows(aq_completeness$annual_hour) %>%
    bind_rows(aq_completeness$quarter_hours)

  # -fix if instrument column is not included
  if (!'instrument' %in%  colnames(aq_completeness_summary)) {
    aq_completeness_summary <- aq_completeness_summary %>%
      left_join(aq_data_wide %>% select(station_name,instrument,parameter,year) %>% unique())
  }

  cols_aq_all <- colnames(aq_completeness_summary)
  cols_aq_main <- c('parameter','station_name','instrument','year','quarter')
  cols_aq_values <- cols_aq_all[!(cols_aq_all %in% cols_aq_main)]
  cols_aq_values <- cols_aq_values[grepl('perc',cols_aq_values)]




  # -assess if data completeness satisfied
  # -this means >=75% data valid, but >=60% for quarter data
  # -separate the two to quarter, and non-quarter requirements
  # -also has marker for valid over 50% as part of NO2, SO2 exception requirements, set as FALSE for other pollutant

  ### DEBUG 20251205
  # aq_completeness_summary_result
  aq_valid_ <- ungroup(aq_completeness_summary) %>%

    #_DEBUG
    # filter(grepl('plaza',station_name,ignore.case = TRUE)) %>%


    select(c(cols_aq_main,cols_aq_values)) %>%
    pivot_longer(cols = cols_aq_values) %>%
    rename(data_count_category = name,
           count = value) %>%
    filter(!is.na(count)) %>%
    mutate(count = round2(count)) %>%
    mutate(is_quarter = grepl('quarter',data_count_category,ignore.case = TRUE)) %>%
    mutate(valid_quarter = (count>=60),
           valid_annual = (count>=75),
           valid_over50 = ifelse(parameter %in% c('NO2','SO2'),(count>=50),FALSE)) %>%
    left_join(df_data_requirement, by = c('parameter','data_count_category'),
              relationship = 'many-to-many') %>%
    filter(!is.na(metric))

  # -evaluate if CAAQS exceeded, since this forms an exception in some cases
  # -the CAAQS_exceeded boolean is set to FALSE for PM2.5 annual
  aq_valid_data_ <- aq_valid_  %>%
    #_DEBUG
    # filter(grepl('plaza',station_name,ignore.case = TRUE)) %>%

    left_join(aq_data_wide,by = c('parameter','station_name','instrument','year','metric'),
              relationship = 'many-to-many') %>%
    mutate(value_rounded = round2(value,n=data_precision)) %>%
    mutate(CAAQS_exceeded = (value_rounded>CAAQS_value)) %>%
    mutate(CAAQS_exceeded = ifelse((parameter == 'PM25' & metric == 'annual'),FALSE,
                                   CAAQS_exceeded))

  # -separate evaluation of annual and of quarter

  # evaluate annual
  aq_valid_data_annual_ <- aq_valid_data_ %>%
    filter(!is_quarter) %>%
    mutate(valid = ifelse(valid_annual,
                          TRUE,
                          (CAAQS_exceeded & valid_over50)
    )) %>%
    mutate(valid_flag = ifelse(valid_annual,
                               FALSE,
                               (CAAQS_exceeded & valid_over50)
    ))%>%
    group_by(parameter,station_name,instrument,year,metric,tfee,data_count_category) %>%
    summarise(valid = all(valid), valid_flag =any(valid_flag),quarter_count = n())%>%
    ungroup()

  #evaluate quarter

  aq_valid_data_quarter_ <- aq_valid_data_ %>%
    filter(is_quarter)%>%
    mutate(valid = ifelse(valid_quarter,
                          TRUE,
                          (CAAQS_exceeded)
    ))%>%
    mutate(valid_flag = ifelse(valid_quarter,
                               FALSE,
                               (CAAQS_exceeded)
    )) %>%
    group_by(parameter,station_name,instrument,year,metric,tfee,data_count_category) %>%
    summarise(valid = all(valid), valid_flag =any(valid_flag),quarter_count = n()) %>%
    ungroup()

  # -error stop if quarter <4
  if (any(aq_valid_data_quarter_$quarter_count != 4)) {
    stop('Error in quarter assessment')
  }


  aq_valid_result <- bind_rows(aq_valid_data_annual_,aq_valid_data_quarter_) %>%
    group_by(parameter,station_name,instrument,year,metric,tfee) %>%
    summarise( valid = all(valid),valid_flag = any(valid_flag)) %>%
    mutate(valid_flag = ifelse(!valid,FALSE,valid_flag)) %>%
    ungroup()


  aq_wide <- left_join( aq_valid_result,aq_data_wide,relationship = 'many-to-many') %>%
    mutate(value = ifelse(valid,value,NA)) %>%
    mutate(value_rounded = round2(value,n=data_precision))


  # -calculate three year averaging
  df <- aq_wide %>%
    mutate(orig_year = year)
  for (i in 1:2) {
    df_ <- aq_wide %>%
      mutate(orig_year = year) %>%
      mutate(year = year + i)
    df <- bind_rows(df,df_)
  }

  df_result <- df %>%
    filter(year %in% years) %>%
    mutate(counter = ifelse(is.na(value),0,1)) %>%
    group_by(parameter,station_name,instrument,
             year,metric,tfee,years_averaged,data_precision) %>%
    mutate(value_3yr = mean(value,na.rm = TRUE), count = sum(counter)) %>%
    mutate(valid_3yr = (count>=2),
           valid_flag_yrs_3 = any(valid_flag),
           valid_2of3 = (count == 2)) %>%
    mutate(value_3yr = ifelse(valid_3yr,value_3yr,NA)) %>%
    mutate(metric_value = ifelse(years_averaged == 3,value_3yr,value),
           valid = ifelse(years_averaged == 3,valid_3yr,valid),
           valid_flag = ifelse(years_averaged == 3,valid_flag_yrs_3,valid_flag),
           valid_2of3 = ifelse(years_averaged == 3,valid_2of3,NA)

           )%>%
    ungroup() %>%
    select(parameter,station_name,instrument,year,metric,
           metric_value,tfee,valid,valid_flag,valid_2of3,data_precision,orig_year) %>%
    filter(year == orig_year) %>%
    distinct()


  df_result_mgmt_level <- get_management(df_result)


  result <- df_result %>%
    select(parameter,station_name,instrument,
           tfee,year,metric,valid,valid_flag,valid_2of3) %>%
    unique() %>%
    left_join(df_result_mgmt_level)



  return(result)
}

#' Define the caaqs values
#'
#' @param years are the years to be included
#' @examples
#' get_caaqs(years = 2010:2030)
#'
#' @export
get_caaqs <- function(years = NULL) {
  caaqs_history <- tribble(
    ~parameter,~metric,    ~year,  ~value,~data_precision,~years_averaged,
    'PM25',    'annual',   2015,   10,    1,              3,
    'PM25',    'annual',   2020,   8.8,   1,              3,
    'PM25',    'annual',   2030,   8,     1,              3,
    'PM25',    '24h',      2015,   28,    0,              3,
    'PM25',    '24h',      2020,   27,    0,              3,
    'PM25',    '24h',      2030,   23,    0,              3,
    'O3',      '8h',       2015,   63,    0,              3,
    'O3',      '8h',       2020,   62,    0,              3,
    'O3',      '8h',       2025,   60,    0,              3,
    'NO2',     'annual',   2020,   17,    1,              1,
    'NO2',     'annual',   2025,   12,    1,              1,
    'NO2',     '1h',       2020,   60,    0,              3,
    'NO2',     '1h',       2025,   42,    0,              3,
    'SO2',     'annual',   2020,   5.0,   1,              1,
    'SO2',     'annual',   2025,   4.0,   1,              1,
    'SO2',     '1h',       2020,   70,    0,              3,
    'SO2',     '1h',       2025,   65,    0,              3
  )

  # - apply CAAQS to data from 2010, everything before first CAAQS applies to dates before the CAAQS were defined
  min_year <- 2010
  max_year <- max(year(Sys.Date()),caaqs_history$year)

  # -if user defined range for years, it will take the wider value
  # -results will be filtered based on user request at the end of function
  if (!is.null(years)) {
    min_year <- min(years,min_year)
    max_year <- max(years,max_year)
  } else{
    # -defines default value for years if not specified by user
    years <- min_year:max_year
  }



  df_pad <- tibble(year = min_year:max_year) %>%
    cross_join(
      caaqs_history %>%
        select(parameter,metric,data_precision,years_averaged) %>%
        unique()
    )

  df_caaqs_history <- left_join(df_pad,caaqs_history)

  df_caaqs_history_blank <- df_caaqs_history %>%
    select(-value)
  df_caaqs_history_nonblank <- df_caaqs_history[!is.na(df_caaqs_history$value),] %>%
    rename(CAAQS_year = year)

  df_result <- left_join(df_caaqs_history_blank,
                         df_caaqs_history_nonblank,
                         relationship = "many-to-many") %>%
    mutate(dev_year  = year - CAAQS_year)

  # -identify representative CAAQS for each year
  # -filter out before specific CAAQS is in place
  # -except before CAAQS  has been established for that pollutant
  df <- df_result %>%
    group_by(parameter,metric) %>%
    mutate(min_CAAQS_year = min(CAAQS_year)) %>%
    ungroup() %>%
    filter((dev_year>=0 | year<min_CAAQS_year)) %>%
    select(-min_CAAQS_year) %>%
    group_by(parameter,metric,year) %>%
    mutate(dev_year = abs(dev_year)) %>%
    mutate(min_dev_year = min(dev_year)) %>%
    filter(dev_year == min_dev_year) %>%
    select(year,parameter,metric,CAAQS_year, value) %>%
    rename(CAAQS_value = value) %>%
    filter(year %in% years)

  # -combine with precision
  df <- df %>%
    left_join(
      caaqs_history %>%
        select(parameter,metric,data_precision,years_averaged) %>%
        unique()
    ) %>%
    ungroup()

  return(df)
}


#' Define the management levels of given it's parameter, metric, value, and year
#'
#' NOTE: updated until 2025-12-03
#' If CAAQS or management levels are updated, please update this and the get_caaqs() function
#'
#' @param df is the dataframe. Must have the following columns: parameter, year, metric,metric_value,tfee
get_management <- function(df) {

  if (0) {
    df <- df_result
  }

  cols_needed <- c('parameter','year','metric','metric_value')
  cols_extra <- c('station_name','instrument','tfee')

  cols_needed <- c(cols_extra,cols_needed)
  cols_needed <- cols_needed[cols_needed %in% colnames(df)]

  df <- ungroup(df) %>%
    select(all_of(cols_needed))


  # -NOTE: TO be maintained routinely
  # -table updated on 2025-12-03
  # -manual update needed once new CAAQS is implemented
  # -lower_limit is evaluatest ">", upper_limit is evaluated "<="
  df_mgmt <- {tribble(
    ~CAAQS_year,~parameter,~metric,    ~lower_limit,~upper_limit,~mgmt_level,
    2015,       'PM25',     'annual',   10,          Inf,          'red',
    2015,       'PM25',     'annual',   6.4,         10,           'orange',
    2015,       'PM25',     'annual',   4.0,         6.4,          'yellow',
    2015,       'PM25',     'annual',   0,           4.0,          'green',
    2015,       'PM25',     '24h',      28,          Inf,          'red',
    2015,       'PM25',     '24h',      19,          28,           'orange',
    2015,       'PM25',     '24h',      10,          19,           'yellow',
    2015,       'PM25',     '24h',      0,           10,           'green',

    2020,       'PM25',     'annual',   8.8,         Inf,          'red',
    2020,       'PM25',     'annual',   6.4,         8.8,          'orange',
    2020,       'PM25',     'annual',   4.0,         6.4,          'yellow',
    2020,       'PM25',     'annual',   0,           4.0,          'green',
    2020,       'PM25',     '24h',      27,          Inf,          'red',
    2020,       'PM25',     '24h',      19,          27,           'orange',
    2020,       'PM25',     '24h',      10,          19,           'yellow',
    2020,       'PM25',     '24h',      0,           10,           'green',

    2030,       'PM25',     'annual',   8.0,         Inf,          'red',
    2030,       'PM25',     'annual',   6.0,         8.0,          'orange',
    2030,       'PM25',     'annual',   4.0,         6.0,          'yellow',
    2030,       'PM25',     'annual',   0,           4.0,          'green',
    2030,       'PM25',     '24h',      23,          Inf,          'red',
    2030,       'PM25',     '24h',      16,          23,           'orange',
    2030,       'PM25',     '24h',      10,          16,           'yellow',
    2030,       'PM25',     '24h',      0,           10,           'green',

    2015,       'O3',       '8h',       63,          Inf,          'red',
    2015,       'O3',       '8h',       56,          63,           'orange',
    2015,       'O3',       '8h',       50,          56,           'yellow',
    2015,       'O3',       '8h',       0,           50,           'green',

    2020,       'O3',       '8h',       62,          Inf,          'red',
    2020,       'O3',       '8h',       56,          62,           'orange',
    2020,       'O3',       '8h',       50,          56,           'yellow',
    2020,       'O3',       '8h',       0,           50,           'green',

    2025,       'O3',       '8h',       60,          Inf,          'red',
    2025,       'O3',       '8h',       56,          60,           'orange',
    2025,       'O3',       '8h',       50,          56,           'yellow',
    2025,       'O3',       '8h',       0,           50,           'green',

    2020,       'NO2',     'annual',    17,          Inf,          'red',
    2020,       'NO2',     'annual',    7.0,         17,           'orange',
    2020,       'NO2',     'annual',    2.0,         7.0,          'yellow',
    2020,       'NO2',     'annual',    0,           2.0,          'green',
    2020,       'NO2',     '1h',        60,          Inf,          'red',
    2020,       'NO2',     '1h',        31,          60,           'orange',
    2020,       'NO2',     '1h',        20,          31,           'yellow',
    2020,       'NO2',     '1h',        0,           20,           'green',

    2025,       'NO2',     'annual',    12,          Inf,          'red',
    2025,       'NO2',     'annual',    7.0,         12,           'orange',
    2025,       'NO2',     'annual',    2.0,         7.0,          'yellow',
    2025,       'NO2',     'annual',    0,           2.0,          'green',
    2025,       'NO2',     '1h',        42,          Inf,          'red',
    2025,       'NO2',     '1h',        31,          42,           'orange',
    2025,       'NO2',     '1h',        20,          31,           'yellow',
    2025,       'NO2',     '1h',        0,           20,           'green',

    2020,       'SO2',     'annual',   5,            Inf,          'red',
    2020,       'SO2',     'annual',   3.0,          5,            'orange',
    2020,       'SO2',     'annual',   2.0,          3.0,          'yellow',
    2020,       'SO2',     'annual',   0,            2.0,          'green',
    2020,       'SO2',     '1h',       70,           Inf,          'red',
    2020,       'SO2',     '1h',       50,           70,           'orange',
    2020,       'SO2',     '1h',       30,           50,           'yellow',
    2020,       'SO2',     '1h',       0,            30,           'green',

    2025,       'SO2',     'annual',   4,            Inf,          'red',
    2025,       'SO2',     'annual',   3.0,          4,            'orange',
    2025,       'SO2',     'annual',   2.0,          3.0,          'yellow',
    2025,       'SO2',     'annual',   0,            2.0,          'green',
    2025,       'SO2',     '1h',      65,            Inf,          'red',
    2025,       'SO2',     '1h',      50,            65,           'orange',
    2025,       'SO2',     '1h',      30,            50,           'yellow',
    2025,       'SO2',     '1h',      0,             30,           'green'

  )}

  # - change mgmt level to factor
  df_mgmt$mgmt_level <- factor(df_mgmt$mgmt_level,levels = c('red','orange','yellow','green','gray'))
  # -populate the rest of the dataframe to include more years
  yrs <- unique(df$year)

  df_caaqs <- get_caaqs(years = yrs)
  df_caaqs_mgmt <- df_caaqs %>%
    left_join(df_mgmt, by = c('CAAQS_year','parameter','metric'), relationship = 'many-to-many')

  df_calculate <- left_join(df,df_caaqs_mgmt, by = c('parameter','metric','year'), relationship = 'many-to-many')

  df_calculate <- df_calculate %>%
    mutate(metric_value_rounded = round2(metric_value,n=data_precision )) %>%
    mutate(dev_ll = metric_value_rounded - lower_limit,
           dev_up = upper_limit - metric_value_rounded) %>%
    mutate(between = (dev_ll>0 & dev_up>=0)) %>%
    filter(between) %>%
    select(c(cols_needed,'metric_value_rounded','mgmt_level'))

  return(df_calculate)

}

#' Get Annual metrics
#'
#' Same as importBC_data_avg, added for simplicity and consistent
#'
#' @param parameter is the pollutant. Only one pollutant at a time
#' @param years are the years where the annual value is calculated
#'
#' @examples
#' get_annual_metrics('pm25',years = 2017:2020)
#'
#' @export
get_annual_metrics <- function(parameter,years) {

  parameter_select <- parameter
  yrs_select <- years

  df <- importBC_data_avg(parameter = parameter_select,years = yrs_select,flag_TFEE = TRUE,merge_stations = TRUE)

  return(df)
}
