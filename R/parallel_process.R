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

#' Downloads files into the URL
#'
#' @param file_url defines the URLs and destinaton
download_file <- function(file_url) {


  # Download the file using RCurl
  tryCatch({
    # curl::curl_download(file_url$url, file_url$destfile)
    download.file(file_url$url, file_url$destfile, method = "curl",mode='wb')
    return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = "Downloaded"))
  }, error = function(e) {
    return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = paste("Failed:", e$message)))
  })
}



#' Download a list of FTP files
#'
#' uses parallel CPU processing to perform simultaneous downloads
#'
#' @import parallel
#' @param url_list is a vector containing list of URLs to download
#' @param save_dir is the directory where files are saved, NULL means temporary filenames used
download_files <- function(url_list,save_dir = NULL) {

  library(dplyr)

  if (is.null(save_dir)) {
    # create list of tempfiles
    df_urls <- tibble(url = url_list) %>%
      group_by(url) %>%
      mutate(destfile = tempfile()) %>%
      ungroup()
  } else
  {
    df_urls <- tibble(url = url_list) %>%
      mutate(index=1:n()) %>%
      group_by(url) %>%
      mutate(destfile = paste(save_dir,'\\file',index,'.parquet_',sep='')) %>%
      ungroup() %>%
      select(-index)
  }

  df_urls <- split(df_urls, seq(nrow(df_urls)))
  # Set up parallel processing
  num_cores <- parallel::detectCores() - 1  # Use one less than the number of available cores
  cl <- parallel::makeCluster(num_cores)

  # Export the necessary function to the cluster
  clusterExport(cl, c("download_file"),envir = environment())

  # Perform the download in parallel and combine the results into a dataframe
  download_results <- do.call(rbind,
                              parallel::parLapply(cl, df_urls, download_file))

  # Stop the cluster
  stopCluster(cl)

  # Print the results
  return(download_results)
}



# Optional functions below========

# from tests, found these processes are less efficient
#' Download a list of FTP files
#'
#' uses parallel CPU processing to perform simultaneous downloads
#'
#' NOTE: This is found significantly slower than using GET_FTP_DETAILS() on multiple FTP files
#' @param url_list is a vector containing list of URLs to download
get_ftp_parallel <- function(url_list) {



  df_urls <- split(url_list, seq(nrow(url_list)))

  # Set up parallel processing
  num_cores <- parallel::detectCores() - 1  # Use one less than the number of available cores
  cl <- parallel::makeCluster(num_cores)

  # Export the necessary function to the cluster
  clusterExport(cl, c("GET_FTP_DETAILS_"), envir = environment())

  # Perform the download in parallel and combine the results into a dataframe
  download_results <- do.call(bind_rows,
                              parallel::parLapply(cl, url_list, GET_FTP_DETAILS_))

  # Stop the cluster
  stopCluster(cl)

  # Print the results
  return(download_results)
}


#' Parallel process load only
#'
#' This function retrieves data from the FTP
process_ftp_files_ <- function(file_url) {


  # Download the file using RCurl
  tryCatch({


    # -original version
    #curl::curl_download(file_url$url, file_url$destfile)
    #return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = "Downloaded"))

    # -test version
    tmp_file <- tempfile()
    curl::curl_download(file_url$url[[1]], tmp_file)
    df_data <- arrow::read_parquet(tmp_file)
    return(df_data)

  }, error = function(e) {
    return()
    # return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = paste("Failed:", e$message)))
  })
}



#' Download a list of FTP files
#'
#' uses parallel CPU processing to perform simultaneous downloads
#'
#' @import parallel
#' @param url_list is a vector containing list of URLs to download
process_ftp_files <- function(url_list) {

  library(dplyr)


  if (0) {
    url_list <- c("ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/binary/PRESSURE.parquet",
                  "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/binary/SNOW.parquet")

  }
  # create list of tempfiles
  df_urls <- data.frame(url = url_list)

  df_urls <- split(df_urls, seq(nrow(df_urls)))
  # Set up parallel processing
  num_cores <- parallel::detectCores() - 1  # Use one less than the number of available cores
  cl <- parallel::makeCluster(num_cores)

  # Export the necessary function to the cluster
  clusterExport(cl, c("process_ftp_files_"),envir = environment())

  # Perform the download in parallel and combine the results into a dataframe
  results <- do.call(rbind,
                     parallel::parLapply(cl, df_urls, process_ftp_files_))

  # Stop the cluster
  stopCluster(cl)

  # Print the results
  return(results)
}

