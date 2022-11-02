#define source of data
saveDirectory <- '../data/out'
list.files(saveDirectory)

# Remove the results from non-AQMS sites

list_stations <- listBC_stations(use_CAAQS = TRUE)

#list of stations to remove from results
lst_remove <- list_stations %>%
  filter(AQMS == 'N') %>%
  pull(site)

#remove from caaqs_results.csv
df_caaqs_results <- readr::read_csv(paste(saveDirectory,'caaqs_results.csv',sep='/')) %>%
  filter(!site %in% lst_remove)

df_management <- readr::read_csv(paste(saveDirectory,'management.csv',sep='/'))%>%
  filter(!site %in% lst_remove)


#Cleanup for North Vancouver- 2nd Narrows
#due to the interference from construction
#exclude data from 2019-2021
#assigne NA or gray colour to management
df_caaqs_results <- df_caaqs_results %>%
  mutate(metric_value = ifelse(site == 'North Vancouver Second Narrows' & (year>=2019 & year <=2021),
                               NA,metric_value)

  )



#rewrite into the file
readr::write_csv(df_caaqs_results,paste(saveDirectory,'caaqs_results.csv',sep='/'))

#recalculate the managmenet levels
df_management_summary <- get_management_summary(datafile = paste(saveDirectory,'caaqs_results.csv',sep='/'))
readr::write_csv(df_management_summary,paste(saveDirectory,'management.csv',sep='/'))
