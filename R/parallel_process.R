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
# download_file <- function(file_url) {
#
#
#   # Download the file using RCurl
#   tryCatch({
#     curl::curl_download(file_url$url, file_url$destfile)
#     # download.file(file_url$url, file_url$destfile, method = "curl",mode='wb')
#     return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = "Downloaded"))
#   }, error = function(e) {
#     return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = paste("Failed:", e$message)))
#   })
# }



#' Download a list of FTP files
#'
#' uses parallel CPU processing to perform simultaneous downloads
#'
#' @import parallel
#' @param url_list is a vector containing list of URLs to download
download_files <- function(url_list) {

  library(parallel)
  library(curl)


  # create list of tempfiles
  df_urls <- tibble(url = url_list) %>%
    group_by(url) %>%
    mutate(destfile = tempfile()) %>%
    ungroup()

  df_urls <- split(df_urls, seq(nrow(df_urls)))
  # Set up parallel processing
  num_cores <- detectCores() - 1  # Use one less than the number of available cores
  cl <- makeCluster(num_cores)

  # Export the necessary function to the cluster
  clusterExport(cl, c("download_file", "tempfile"))

  # Perform the download in parallel and combine the results into a dataframe
  download_results <- do.call(rbind,
                              parallel::parLapply(cl, df_urls, function(file_url) {


                                # Download the file using RCurl
                                tryCatch({
                                  curl::curl_download(file_url$url, file_url$destfile)
                                  # download.file(file_url$url, file_url$destfile, method = "curl",mode='wb')
                                  return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = "Downloaded"))
                                }, error = function(e) {
                                  return(data.frame(URL = file_url$url, TempFile = file_url$destfile, Status = paste("Failed:", e$message)))
                                })
                              }))

  # Stop the cluster
  stopCluster(cl)

  # Print the results
  return(download_results)
}
