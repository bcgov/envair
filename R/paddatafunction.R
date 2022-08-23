#' Pad data to completely represent entire years
#'
#' This function inserts fillers to data set. The function can identify separate columns of DATE and TIME
#'
#' @param df is the dataframe containing date_time parameters, and values columns
#' @param date_time defines the date_time column. Default is DATE_PST
#' @param values defines the columns with the reported measurements. If NULL, it wil use RAW_VALUE and ROUNDED_VALUE
#' @param time_ending if TRUE means date starts AFTER and EXCLUDES midnight
#' @param add_DATETIME if TRUE, means to force and include DATETIME
#'
#' @export
pad_data <- function(df, date_time = NULL,values = NULL,time_ending = TRUE,add_DATETIME = NULL)
{
  if (0)  {
    df <- readRDS('./test_data/raw_data.Rds')
    # df <- envair::importBC_data('no2',2020)
    df <- data.result
    date_time <- NULL
    values <- NULL
    time_ending <- TRUE
    add_DATETIME = TRUE
  }

  require(dplyr)

  df <- ungroup(df)
  cols_ <- colnames(df)

  if (is.null(add_DATETIME)) {
    # find out if separate DATE and TIME columns are included
    separate_DATETIME <- all(c('DATE','TIME') %in% cols_)
  } else {
    separate_DATETIME <- add_DATETIME
  }

  # assign values for default entries
  if (is.null(date_time)){
    date_time <- cols_[cols_ %in% c('DATE_PST','datetime','date_time',
                                    'date_pst','DATEPST')]
    date_time <- date_time[1]
  }

  if (is.null(values)) {
    values <- cols_[cols_ %in% c('RAW_VALUE','ROUNDED_VALUE','metric_value')]
  }


  cols_select <- c(cols_[!cols_ %in% c(values,date_time)])

  if (separate_DATETIME) {
    cols_select <- cols_select[!cols_select %in% c('DATE','TIME')]
    try(df <- df %>%
      dplyr::select(-DATE,-TIME), silent = TRUE)
  }

  #convert all date and time to time-beginning
  if (time_ending) {
    df[[date_time]] <- df[[date_time]] - lubridate::hours(1)

  }

  lst_datetime <- df %>%
    pull(date_time)

  start_date <- lubridate::ymd_hm(paste(lubridate::year(min(lst_datetime)),'-01-01 00:00',sep=''), tz='etc/GMT+8')
  end_date <- lubridate::ymd_hm(paste(lubridate::year(max(lst_datetime)),'-12-31 23:00',sep=''), tz='etc/GMT+8')

  df_datetime <- dplyr::tibble(!!date_time := seq.POSIXt(from = start_date, to=end_date , by='hour'))

  if (separate_DATETIME) {
    df_datetime$DATE <- lubridate::date(df_datetime[[date_time]])
    df_datetime$TIME <- lubridate::hour(df_datetime[[date_time]])

    if (time_ending)
    {
      df_datetime$TIME <- df_datetime$TIME +1
      df_datetime$TIME <- paste(df_datetime$TIME,':00',sep='')
    }
  }

  df_result <- df_datetime %>%
    merge(
      df %>%
        dplyr::select(`cols_select`) %>%
        unique()
    ) %>%
    dplyr::left_join(df) %>%
    arrange(date_time)

  #convert back the time if time ending
  if (time_ending) {
    df_result[[date_time]] <- df_result[[date_time]] + lubridate::hours(1)

  }

  print(paste('Added/padded rows:',nrow(df_result) - nrow(df)))
  return(df_result)
}
