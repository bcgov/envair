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

#' Retrieve excel sheet tables from BC ENV Open data Portal
#'
#' This function helps access excel sheets table that are located in FTP Open Data Portal
#'
#' @param ExcelURL is the complete URL of the excel file.
#' @param sheet defines the worksheet names in the excel file. can be a vector listing sheets
#' for vector entry (multiple sheets), it will combine result into a single table
#' if NULL and excel file only has one worksheet, it will process that worksheet.
#' If NULL and there are multiple worksheets, function will return with list of worksheets
#' @param header_row specifies which row is the header.
#' If NULL, the function will scan the most likely header row
#' @param data_row specifies which row is the first data start. Typically header_row +1
#' If NULL, the function assumes the row below the header_row
#'
#' @examples
#' get_excel_table('ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx')
#' get_excel_table('ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx',sheet = c('TFEE PM2.5','TFEE O3'))
#' @export
get_excel_table <- function(ExcelURL,sheet=NULL,header_row=NULL,data_row=NULL,silent = FALSE) {
if (0) {
  ExcelURL <- 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx'
  sheet <- c('TFEE PM2.5','TFEE O3')
  sheet <- NULL
  header_row <- NULL
  data_row <- NULL
  ExcelURL <- woodstove_file
  silent = FALSE

}
  require(dplyr)

  Mytmpfile = tempfile()
  RCurl::getBinaryURL(ExcelURL)%>%
    writeBin(con=Mytmpfile)

  #retrieves list of sheets

  suppressWarnings(suppressMessages(sheetlist <- readxl::excel_sheets(Mytmpfile)))

  if (is.null(sheet)) {
    # return with list of sheets or the only sheet
    if (length(sheetlist) > 1) {
      #print sheet list and return null
      if (!silent) {print('Please specify sheet value. Choose among:')
       print(sheetlist)
      }
      return(sheetlist)
    }
  }
  sheet <- sheet[sheet %in% sheetlist]

df_result <- NULL

for (sheet_ in sheet)
{
  header_row_ <- header_row
  if (is.null(header_row)) {
    #scan for number of columns
    #assumes header is somewhere within the first 50 rows
    df_cols <- NULL
    options(warn = -1)  #suppress warnings
    for (i in 1:10)
    {
      df_ <- readxl::read_excel(Mytmpfile,sheet=sheet_,skip=i-1,n_max = 10)
      df_cols <- df_cols %>%
        bind_rows(
          dplyr::tibble(
            column=gsub('\\.\\.\\.','',colnames(df_))
          ) %>%
            dplyr::mutate(index =i) %>%
            dplyr::mutate(rows = nrow(df_)) %>%
            mutate(valid_col_name = (is.na(as.numeric(column))))
        )
    }
    options(warn = 0)  #resume warnings
    header_row_ <-  df_cols %>%
      group_by(index) %>%
      dplyr::summarise(columns = n(),allvalid = all(valid_col_name)) %>%
      filter(allvalid) %>%
      pull(index) %>%
      min()

    if (!silent) {print(paste('For quick future access, Use header_row=',header_row_))}
  }
  if (is.null(data_row)) {
    data_row_ <- header_row_ + 1
  }



  df <- readxl::read_excel(Mytmpfile,sheet=sheet_,skip=header_row_ - 1)

  colnames_df <- colnames(df)

  if (data_row_ != header_row_ + 1)
  {
    df2 <- readxl::read_excel(Mytmpfile,sheet=sheet_,skip=header_row_ - 1,col_names=colnames(df))
    df <- df2
  }
  df_result <- df_result %>%
    dplyr::bind_rows(df %>% dplyr::mutate(sheet = sheet_))
}

if (length(sheet) == 1) {
  #remove sheet column
  df_result <- dplyr::select(df_result,-sheet)
}
  return(df_result)

}

#' Retrieve TFEE data
#'
#' This function retrieve TFEE data from the FTP
#'
#' @param ExcelURL is the complete uRL of the excel file containing TFEE data
#' leave NULL to use known location of fTP at 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx'
#' @examples
#' df_tfee <- get_tfee()
#' @export
get_tfee <- function(ExcelURL = NULL) {

  if (0) {
    ExcelURL <- NULL
  }

  if (is.null(ExcelURL)) {
    ExcelURL <- 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx'
  }

  suppressWarnings(suppressMessages(sheets <- get_excel_table(ExcelURL = ExcelURL,silent = TRUE)))
  sheets <- sheets[grepl('TFEE',sheets)]
  suppressWarnings(suppressMessages(df <- get_excel_table(ExcelURL = ExcelURL, sheet = sheets, silent = TRUE)))

  df <- df %>%
    mutate(STATION_NAME = gsub('[^[:alnum:]]',' ',STATION_NAME)) %>%
    mutate(STATION_NAME = gsub('\u00A0',' ',STATION_NAME)) %>%
    mutate(STATION_NAME = gsub('\\s+',' ',STATION_NAME)) %>%
    dplyr::rename(PARAMETER = sheet) %>%
    dplyr::mutate(PARAMETER = gsub('TFEE','',PARAMETER)) %>%
    dplyr::mutate(PARAMETER = gsub('_','',PARAMETER)) %>%
    dplyr::mutate(PARAMETER = gsub('\\.','',PARAMETER)) %>%
    dplyr::mutate(PARAMETER = gsub(' ','',PARAMETER)) %>%
    arrange(DATE)

  return(df)

}

#' Retrieve station history for CAAQS-reporting
#'
#' This function retrieves station and instrument history for CAAQS-reporting purposes
#' The output is a dataframe showing the station and instrument start and end dates, and merged name
#'
#' @param ExcelURL is the complete URL of the excel file containing station history data
#' leave NULL to use known location of FTP at 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx'
#' @export
get_station_history <- function(ExcelURL=NULL) {
  if (0) {
    ExcelURL <- NULL
  }

  if (is.null(ExcelURL)) {
    ExcelURL <- 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx'
  }


  suppressWarnings(suppressMessages(df_excel <- get_excel_table(ExcelURL,sheet = 'Data Merge',silent=TRUE)))

  # -remove hidden space characters
  try({
  df_excel$STATION_NAME <- gsub('[^[:alnum:]]',' ',df_excel$STATION_NAME)
  df_excel$STATION_NAME <- gsub('\\s+',' ',df_excel$STATION_NAME)
  })
  return(as.data.frame(df_excel))

}

#' check the data for TFEE, and add TFEE flag
#'
#' This function adds TFEE flag to hourly or daily data
#'
#' @param df is the dataframe containing hourly or daily data
#' @param station_column is a string defining the station name.
#' By default it will be the column containg "station" or "site" in the name
#' @param date_column is the column containing the date.
#' Note that if this is date/time column, is_time_ending should be specified
#' @param param_column is the column defining the pollutant or parameter
#' @param is_time_ending is either TRUE/FALSE. default is TRUE.
#' time_ending means that the start of the day is AFTER and NOT including midnight.
#' @examples
#' df <- importBC_data('pm25',years = 2020)
#' df_tfee <- add_TFEE(df)
#' df_tfee <- add_TFEE(df, station_column = 'STATION_NAME',date_column = 'DATE_PST',is_time_ending = TRUE)
#' @export
add_TFEE <- function(df,station_column = NULL,date_column = NULL,param_column = NULL, is_time_ending = TRUE) {

  if (0) {
       df <- importBC_data('pm25',2020)
       station_column <- NULL
       date_column <- NULL
       is_time_ending <- TRUE
       param_column <- NULL
  }

  cols <- colnames(df)

  #find the columns if not defined
  if (is.null(station_column)) {
    #in order of preference

    lst_stations <- sort(cols[grepl('STATION_NAME',ignore.case = TRUE,cols)])

    lst_stations <- c(lst_stations,cols[grepl('site',ignore.case = TRUE,cols)])
    lst_stations <- c(lst_stations,cols[grepl('location',ignore.case = TRUE,cols)])
    station_column <- unique(lst_stations)[1]
  }
  if (is.null(date_column)) {
      #in order of preference
    lst_dates <- tolower(c('date','datetime','time'))
    lst_dates <- lst_dates[lst_dates %in% tolower(cols)][1]
    date_column <- cols[tolower(cols) %in% lst_dates]
  }

  if (is.null(param_column)) {
    #in order of preference
    lst_params <- tolower(c('PARAMETERS','PARAMETER','POLLUTANT'))
    lst_params <- lst_params[lst_params %in% tolower(cols)][1]
    param_column <- cols[tolower(cols) %in% lst_params]
  }


# Add specified columns for station, parameter, dates
  df$col_stn <- df[,station_column]
  df$col_param <- df[,param_column]
  df$col_datetime <- lubridate::force_tz(df[,date_column],tz='etc/Gmt+8')
  df$col_date <- as.Date(lubridate::force_tz(df[,date_column],tz='etc/Gmt+8'))

  #identify if date column is pure date or POSIX
  if (!all(df$col_datetime == df$col_date)) {

    if (is_time_ending) {
      df$col_datetime <- df[,date_column] - lubridate::hours(1)
    } else {
      df$col_datetime <- df[,date_column]
    }
    df$col_date <- as.Date(df$col_datetime, tz='etc/GMt+8')
  }
  df <- df %>%
    dplyr::mutate(index = paste(col_stn,col_param,col_date))

  #retrieve TFEE data
  list_tfee <- get_tfee() %>%
    dplyr::mutate(STATION_NAME = gsub('\u00A0',' ',STATION_NAME),
                  PARAMETER = gsub('\u00A0',' ',PARAMETER),
                  DATE = gsub('\u00A0',' ',DATE)) %>%
    mutate(STATION_NAME = gsub('\\s+',' ',STATION_NAME)) %>%
    dplyr::mutate(index = paste(STATION_NAME,PARAMETER,DATE)) %>%
    pull(index)

  df_tfee <- df%>%
    filter(index %in% list_tfee) %>%
    dplyr::mutate(flag_tfee = TRUE)

  df <- df%>%
    filter(!index %in% list_tfee) %>%
    dplyr::mutate(flag_tfee = FALSE) %>%
    dplyr::bind_rows(df_tfee) %>%
    arrange(col_stn,col_date) %>%
    dplyr::select(-index,-col_stn,-col_date,-col_param,-col_datetime)

  return(df)
}

#' Merges station names for reporting purposes
#'
#' This function combines station names for CAQQS-reporting purposes
#' It uses the list created in the FTP server 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/BC_CAAQS_station_history.xlsx'
#'
#' @param df is the dataframe containing the data
#' @param station_column is a string defining the station name.
#' By default it will be the column containg "station" or "site" in the name
#' @param data_column is the column containing the pollutant data
#' @param instrument_column is the column defining the instrument type
#' @param date_column is the column containing the date.
#' Note that if this is date/time column, is_time_ending should be specified
#' @param param_column is the column defining the pollutant or parameter
#' @param is_time_ending is either TRUE/FALSE. default is TRUE.
#' time_ending means that the start of the day is AFTER and NOT including midnight.
#' @examples
#' df <- importBC_data('pm25',2019)
#' df <- merge_STATIONS(df)
#' @export
merge_STATIONS <- function(df,station_column = NULL,data_column = NULL,instrument_column = NULL,
                           date_column = NULL,param_column = NULL, is_time_ending = TRUE) {

  if (0) {
    source('./r/importbc_data.R')
    source('./r/paddatafunction.R')
    source('./r/listBC_stations.R')
    source('./r/envairfunctions.R')
    source('./r/get_caaqs_stn_history.R')
    df <- importBC_data('pm25',2020,pad = FALSE)
    station_column <- NULL
    date_column <- 'DATE_PST'
    is_time_ending <- TRUE
    param_column <- NULL
    instrument_column <- NULL
    data_column <- NULL
  }

  cols <- colnames(df)
  df <- ungroup(df)
  df <- tibble::rowid_to_column(df,'row_index')

  #find the columns if not defined
  if (is.null(station_column)) {
    #in order of preference

    lst_stations <- sort(cols[grepl('STATION_NAME',ignore.case = TRUE,cols)])

    lst_stations <- c(lst_stations,cols[grepl('site',ignore.case = TRUE,cols)])
    lst_stations <- c(lst_stations,cols[grepl('location',ignore.case = TRUE,cols)])
    station_column <- unique(lst_stations)[1]
  }
  if (is.null(date_column)) {
    #in order of preference
    lst_dates <- tolower(c('date','datetime','time'))
    lst_dates <- lst_dates[lst_dates %in% tolower(cols)][1]
    date_column <- cols[tolower(cols) %in% lst_dates]
  }

  if (is.null(param_column)) {
    #in order of preference
    lst_params <- tolower(c('PARAMETERS','PARAMETER','POLLUTANT'))
    lst_params <- lst_params[lst_params %in% tolower(cols)][1]
    param_column <- cols[tolower(cols) %in% lst_params]
  }

  if (is.null(data_column)) {
    #in order of preference
    lst_data <- tolower(c('RAW_VALUE','ROUNDED_VALUE','value'))
    lst_data <- lst_data[lst_data %in% tolower(cols)][1]
    data_column <- cols[tolower(cols) %in% lst_data]
  }

  if (is.null(instrument_column)) {
    #in order of preference
    lst_inst <- tolower(c('INSTRUMENT','INST','equipment'))
    lst_inst <- lst_inst[lst_inst %in% tolower(cols)][1]
    instrument_column <- cols[tolower(cols) %in% lst_inst]
  }



  # Add specified columns for station, parameter, dates
  df$col_stn <- df[,station_column]
  df$col_data <- df[,data_column]
  df$col_inst <- df[,instrument_column]
  df$col_param <- df[,param_column]
  df$col_datetime <- lubridate::force_tz(df[,date_column],tz='etc/Gmt+8')
  df$col_date <- as.Date(lubridate::force_tz(df[,date_column],tz='etc/Gmt+8'))

  df <- df %>%
      dplyr::mutate(index = paste(col_stn,col_inst))

  #identify if date column is pure date or POSIX
  if (!all(df$col_datetime == df$col_date)) {

    if (is_time_ending) {
      df$col_datetime <- df[,date_column] - lubridate::hours(1)
    } else {
      df$col_datetime <- df[,date_column]
    }
    df$col_date <- as.Date(df$col_datetime, tz='etc/GMt+8')
  }

  df$STATION_NAME_ORIGINAL <- df$col_stn
  df$INSTRUMENT_ORIGINAL <- df$col_inst



  df_stn_history <- get_station_history()
  df_stn_history <- df_stn_history %>%
    dplyr::mutate(STATION_NAME = gsub('\u00A0',' ',STATION_NAME),
                  INSTRUMENT = gsub('\u00A0',' ',INSTRUMENT),
                  `Start Date` = gsub('\u00A0',' ',`Start Date`),
                  `End Date` = gsub('\u00A0',' ',`End Date`)
                  ) %>%
    mutate(STATION_NAME = gsub('\\s+',' ',STATION_NAME)) %>%
    dplyr::mutate(index = paste(STATION_NAME,INSTRUMENT))

  try(df_stn_history$`Start Date` <- lubridate::force_tz(df_stn_history$`Start Date`,tz='etc/Gmt+8'),silent = TRUE)
  try(df_stn_history$`End Date` <- lubridate::force_tz(df_stn_history$`End Date`,tz='etc/Gmt+8'),silent = TRUE)
  df_stn_history$`Start Date` <-  as.Date(df_stn_history$`Start Date`, tz='etc/Gmt+8')
  df_stn_history$`End Date` <-  as.Date(df_stn_history$`End Date`, tz='etc/Gmt+8')

  #remove stations before start dates and after end dates----
  lst_remove <- NULL   #list of row_index to remove
  df_stn_history_strt <- df_stn_history%>%
    filter(!is.na(`Start Date`))

  for (i in 1:nrow(df_stn_history_strt)) {

    stn_index <- df_stn_history_strt[i,]$index
    datastart <- df_stn_history_strt[i,]$`Start Date`

    lst_remove_ <- df %>%
      filter(index == stn_index) %>%
      filter(col_date < datastart) %>%
      pull(row_index)
    lst_remove <- unique(c(lst_remove,lst_remove_))
  }


  df_stn_history_end <- df_stn_history%>%
    filter(!is.na(`End Date`))
  for (i in 1:nrow(df_stn_history_end)) {

    stn_index <- df_stn_history_end[i,]$index
    dataend <- df_stn_history_end[i,]$`End Date`

    lst_remove_ <- df %>%
      filter(index == stn_index) %>%
      filter(col_date > dataend) %>%
      pull(row_index)
    lst_remove <- c(lst_remove,lst_remove_)
  }

  df <- df %>%
    filter(!row_index %in% lst_remove)

  #rename instruments------
  df_stn_history_inst <- df_stn_history %>%
    filter(!is.na(`Merged Instrument Name`))

  for (i in 1:nrow(df_stn_history_inst)) {
    stn_index <- df_stn_history_inst[i,]$index
    merged_inst_name <- df_stn_history_inst[i,]$`Merged Instrument Name`
    df$INSTRUMENT[df$index == stn_index] <- merged_inst_name
  }

  #rename stations
  df_stn_history_merge_stn <- df_stn_history %>%
    filter(!is.na(`Merged Station Name`))

  for (i in 1:nrow(df_stn_history_merge_stn)) {
    stn_index <- df_stn_history_merge_stn[i,]$STATION_NAME
    merged_stn_name <- df_stn_history_merge_stn[i,]$`Merged Station Name`
    df$STATION_NAME[df$STATION_NAME == stn_index] <- merged_stn_name
  }

  df$STATION_NAME_ORIGINAL[df$STATION_NAME == df$STATION_NAME_ORIGINAL] <- ''
  df$INSTRUMENT_ORIGINAL[df$INSTRUMENT == df$INSTRUMENT_ORIGINAL] <- ''
  df <- df %>%
    arrange(col_stn,col_datetime) %>%
    dplyr::select(-col_stn,-col_data,-col_inst,-col_param,-col_datetime,-col_date,-row_index,-index)%>%
    COLUMN_REORDER(c(station_column,'STATION_NAME_ORIGINAL',
                     instrument_column,'INSTRUMENT_ORIGINAL',
                     param_column,date_column))

  return(df)


}
