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


#' Create CAAQS bar graph
#'
#' @description Creates the bar graph use for CAAQS
#'
#' @param df is the dataframe containing CAAQS metrics, and non-CAAQS annual metrics
#' This dataframe is created using the create_CAAQS_graph_files()
#' @param parameter is the parameter of either 'pm25','o3','no2','so2'
#' @param station is the station name
#' if NULL, result displays the available stations that can be listed
#' @param startyear is the start year to include
create_CAAQS_graph <- function(df, parameter, station = NULL, startyear = 2013) {

  if (0) {
    aq_summary1 <-  readr::read_csv('././test_data/air_data_summary.csv') %>%
      mutate(metric = recode(metric,'o3' = 'ozone'  ,'o3_tfee' = 'ozone_tfee'))

    aq_summary <-  readr::read_csv('./test_data/caaqs_results.csv')
    unique(aq_summary$metric)
    df <- aq_summary
    parameter <- 'pm2.5'
    parameter <- 'so2'
    station <- NULL
    station <- 'Prince George Plaza 400'


  }


  a <- NULL #output results


  #standardize the name of parameter
  parameter <- recode(parameter,'PM\u2082.\u2085' = 'pm2.5',
                      'NO\u2082' = 'no2',
                      'SO\u2082' ='so2',
                      'Ozone' = 'ozone')
  parameter <- tolower(parameter)
  parameter <- gsub('o3','ozone',parameter,ignore.case = TRUE)
  parameter <- gsub('pm25','pm2.5',parameter,ignore.case = TRUE)


  if (is.null(station)) {
    a <- df %>%
      filter(grepl(parameter,metric,ignore.case = TRUE)) %>%
      filter(!is.na(metric_value)) %>%
      pull(site) %>% unique() %>%  sort()
    return(a)
  }

  #this segment added as fix to the source file
  if (0) {
    df <- aq_summary
    unique(df$metric)

    colnames(aq_summary1)
    colnames(df)
  }
  try(
    df <- df %>%
      filter(year >= startyear) %>%
      mutate(metric = gsub('pm25','pm2.5',metric,ignore.case = TRUE)) %>%
      mutate(metric = ifelse(tfee,paste(metric,'_tfee',sep=''),metric)) %>%
      mutate(metric = recode(metric,
                             'pm2.5_24h' = 'pm2.5_24h',
                             'pm2.5_24hr(1yr)' = 'pm2.5_24h (1yr)',
                             'pm2.5_24h_tfee' = 'pm2.5_24h_tfee',
                             'pm2.5_24hr(1yr)_tfee' = 'pm2.5_24h_tfee (1yr)',
                             'pm2.5_annual' = 'pm2.5_annual',
                             'pm2.5_ann(1yr)' = 'pm2.5_annual (1yr)',
                             'pm2.5_annual_tfee' =  'pm2.5_annual_tfee',
                             'pm2.5_ann(1yr)_tfee' = 'pm2.5_annual_tfee (1yr)',
                             'o3_8h' = 'ozone',
                             'o3_8h_tfee' = 'ozone_tfee',
                             'o3_8h(1yr)' = 'ozone_4th (1yr)',
                             'o3_8h(1yr)_tfee' = 'ozone_4th_tfee (1yr)',
                             'no2_1hr' = 'no2_1hr',
                             'no2_ann' = 'no2_annual',
                             'no2_1hr(1yr)' = 'no2_1hr (1yr)',
                             'no2_ann(1yr)' = 'no2_annual (1yr)',
                             'so2_1hr' = 'so2_1hr',
                             'so2_ann' = 'so2_annual',
                             'so2_1hr(1yr)' = 'so2_1hr (1yr)',
                             'so2_ann(1yr)' = 'so2_annual (1yr)'

      )) %>%
      select(site,year,instrument,metric,metric_value) %>%
      unique())

  #cleanup to remove duplicate within station
  df <- df %>%
    arrange(desc(metric_value)) %>%
    group_by(site,metric,year) %>%
    dplyr::mutate(index = 1:n()) %>%
    filter(index ==1) %>% select(-index) %>% ungroup() %>%
    arrange(site,year)

  #fill blanks
  df <- df %>%
    select(site,metric) %>%
    unique() %>%
    merge(tibble(year = min(df$year):max(df$year))) %>%
    left_join(df)
  #lower the case for the station
  station <- tolower(station)
  # print(station)
  if (parameter == 'pm2.5')
  {

    aq <- df%>%
      filter(grepl('pm2.5',metric) ) %>%
      filter(tolower(site) == station)  #PROD

    # print(nrow(aq))
    #define data for line and bar graph
    #this will be the line trend
    aq_caaqs <- aq %>%
      filter(!grepl('1yr',metric))

    #this will be the bar graph
    #recalculate so tfee is only the delta value
    aq_1yr <-
      aq %>%
      filter(grepl('1yr',metric)) %>%
      tidyr::pivot_wider(names_from = metric, values_from = metric_value) %>%
      mutate(`pm2.5_24h (1yr)` = `pm2.5_24h (1yr)` - `pm2.5_24h_tfee (1yr)`,
             `pm2.5_annual (1yr)` = `pm2.5_annual (1yr)`- `pm2.5_annual_tfee (1yr)`
      ) %>%
      # View()
      tidyr::pivot_longer(cols = -c('site','instrument','year'),
                          names_to = 'metric', values_to = 'metric_value')


    #define the min and max of the data
    year_min = min(min(aq$year),2018)
    year_max = max(max(aq$year),2020)
    # x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    x_lbls <- year_min:year_max
    ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'pm2.5_24h (1yr)'],na.rm=TRUE),32)
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'pm2.5_annual (1yr)'],na.rm=TRUE),12)


    #managmeent levels
    # geom_rect(mapping=aes(xmin=grp_start,xmax=2019.5,ymin=28,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
    # geom_rect(mapping=aes(xmin=2019.5,xmax=2021,ymin=27,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +




    #the annual plot ------------

    p1_ann <-

      aq_caaqs  %>%
      filter(grepl('annual',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric),fill = NA)  +
      # geom_point()

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=2019.5,ymin=10,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max+1,ymin=8.8,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=2019.5,ymin=6.4,ymax=10),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max + 1,ymin=6.4,ymax=8.8),alpha=0.1,color=NA,fill='#F46D43') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=4,ymax=6.4),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=4),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('annual',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('annual',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('annual',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black',fill='white') +
      geom_point(size=4,fill='white') +

      #CAAQS reference lines
      geom_segment(aes(x=year_min-0.2,y=10,xend=2019.5,yend=10),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=2019.5,y=8.8,xend=year_max + 1,yend=8.8),colour='red2',linetype='dashed',size=1) +
      geom_segment(aes(x=2019.5,y=8.8,xend=2019.5,yend=10),colour='red2',linetype='dashed',size=1)+


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=10+1,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
      annotate("text",x= year_max,y=8.8+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust=1) +

      # geom_hline(yintercept = 27, linetype ='dashed',colour = 'red') +
      # annotate("text",x= 2013,y=27,label = '2020 CAAQS',colour = 'red',angle =0) +


      # xlab('Reporting Period')+
      ylab(expression(paste('PM'[2.5],' Annual Metric (',mu,'g/',m^3,')'))) +


      labs(colour = 'Location',
           title = paste('Air Zone')) + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'right', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_rect(fill = 'white', colour = NA, size = 0.25),
            legend.background = element_blank(),
            # legend.key = element_blank(),
            # axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +
      # theme_minimal() +

      ylim(0,ymax_ann) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_ann), expand = c(0,0))+
      scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
      scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

      scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted'))

    #the 24-hour metric plot--------
    p1_24h <-

      aq_caaqs  %>%
      filter(grepl('24h',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max - 0.5,ymin=28,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_max - 0.5,xmax=year_max + 1,ymin=27,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max - 0.5,ymin=19,ymax=28),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_max - 0.5,xmax=year_max + 1,ymin=19,ymax=27),alpha=0.1,color=NA,fill='#F46D43') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=10,ymax=19),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=10),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('24h',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('24h',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('24h',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +


      #CAAQS reference lines
      geom_segment(aes(x=year_min-0.2,y=28,xend=year_max - 0.5,yend=28),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=year_max - 0.5,y=27,xend=year_max + 1,yend=27),colour='red2',linetype='dashed',size=1) +
      geom_segment(aes(x=year_min-0.2,y=28,xend=year_max - 0.5,yend=28),colour='red2',linetype='dashed',size=1)+

      #labels to CAAQS reference lines
      annotate("text",x= year_min+1,y=28+4,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
      annotate("text",x= year_max,y=27+4,label = '2020 CAAQS',colour = 'white',angle =0,hjust=1) +

      # geom_hline(yintercept = 27, linetype ='dashed',colour = 'red') +
      # annotate("text",x= 2013,y=27,label = '2020 CAAQS',colour = 'red',angle =0) +


      # xlab('Reporting Period')+
      ylab(expression(paste('PM'[2.5],' 24-Hour Metric (',mu,'g/',m^3,')'))) +


      labs(colour = 'TFEE',
           title = '') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'none', legend.direction = 'horizontal',legend.justification = 'left',
            legend.title=element_blank(), legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_24h) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
      scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
      scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
      scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
      xlab('Year')
    # theme(legend.position = 'none')
    a <- print(p1_ann/p1_24h)
  }

  if (parameter == 'ozone')
  {
    aq <- df%>%
      filter(grepl('ozone',metric)) %>%
      # filter(year >=2012) %>%
      # filter(site == 'Victoria Topaz')   #debug
      filter(tolower(site) == station)  #PROD



    #define data for line and bar graph
    #this will be the line trend
    aq_caaqs <- aq %>%
      filter(!grepl('1yr',metric))

    #this will be the bar graph
    #recalculate so tfee is only the delta value
    aq_1yr <-
      aq %>%
      filter(grepl('1yr',metric)) %>%
      pivot_wider(names_from = metric, values_from = metric_value) %>%
      mutate(`ozone_4th (1yr)` = `ozone_4th (1yr)` - `ozone_4th_tfee (1yr)`) %>%
      # View()   #debug
      select(-instrument) %>%
      pivot_longer(cols = -c('site','year'),
                   names_to = 'metric', values_to = 'metric_value')

    #define the min and max of the data
    year_min = min(min(aq$year),2019)
    year_max = max(max(aq$year),2020)
    # x-axis labels
    # x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    x_lbls <- year_min:year_max

    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'ozone_4th (1yr)'],na.rm=TRUE),70)



    p1_ann <-

      aq_caaqs  %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=2019.5,ymin=63,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max+1,ymin=62,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=2019.5,ymin=56,ymax=63),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=2019.5,xmax=year_max + 1,ymin=56,ymax=62),alpha=0.1,color=NA,fill='#F46D43') +

      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=50,ymax=56),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=30,ymax=50),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year,
                   y= aq_1yr$metric_value,
                   fill = aq_1yr$metric
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +
      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +

      #CAAQS reference lines
      geom_segment(aes(x=year_min-0.2,y=63,xend=2019.5,yend=63),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=2019.5,y=62,xend=year_max + 1,yend=62),colour='red2',linetype='dashed',size=1) +
      geom_segment(aes(x=2019.5,y=62,xend=2019.5,yend=63),colour='red2',linetype='dashed',size=1)+


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=63+2,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
      annotate("text",x= year_max,y=62+2,label = '2020 CAAQS',colour = 'white',angle =0,hjust=1) +

      # geom_hline(yintercept = 27, linetype ='dashed',colour = 'red') +
      # annotate("text",x= 2013,y=27,label = '2020 CAAQS',colour = 'red',angle =0) +


      # xlab('Reporting Period')+
      ylab(expression(paste('Ozone 8-Hour Metric (ppb)'))) +


      labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'right', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_blank(),
            legend.background = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +
      ylim(0,ymax_ann) +
      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,year_max+1),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(30,ymax_ann), expand = c(0,0),oob=rescale_none,trans = 'reverse')+
      scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
      scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

      scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted')) +
      xlab('Year')

    a <- print(p1_ann)

  }

  if (parameter %in% c('no2'))
  {
    aq <- df%>%
      filter(grepl('no2',metric)) %>%
      # filter(year >=2012) %>%
      select(-instrument) %>%
      # filter(site == 'Victoria Topaz')   #debug
      filter(tolower(site) == station)  #PROD

    aq_caaqs <- aq %>%
      filter(!grepl('1yr',metric))

    aq_1yr <-
      aq %>%
      filter(grepl('1yr',metric))

    #define the min and max of the data
    year_min = min(min(aq$year),2018)
    year_max = max(max(aq$year),2020)
    # x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    x_lbls <- year_min:year_max
    #note that ymax_24h is 1-hour metric
    ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'no2_1hr (1yr)'],na.rm=TRUE),70)
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'no2_annual (1yr)'],na.rm=TRUE),20)


    p1_ann <-
      aq_caaqs  %>%
      filter(grepl('annual',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=17,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=7,ymax=17),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=2,ymax=7),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=2),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('annual',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('annual',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('annual',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +

      #CAAQS reference lines
      # geom_segment(aes(x=year_min,y=12,xend=2019.5,yend=12),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=year_min-0.2,y=17,xend=year_max + 1,yend=17),colour='red2',linetype='dashed',size=1) +


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=17+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust =1) +


      # xlab('Reporting Period')+
      ylab(expression(paste('NO'[2],' Annual Metric (ppb)'))) +

      labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'none', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_ann) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_ann), expand = c(0,0))+
      scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
      scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

      scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted'))


    #the NO2 1-hour metric plot--------
    p1_24h <-

      aq_caaqs  %>%
      filter(grepl('1hr',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=60,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=31,ymax=60),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=20,ymax=31),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=20),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('1hr',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('1hr',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('1hr',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +


      #CAAQS reference lines
      geom_segment(aes(x=year_min-0.2,y=60,xend=year_max + 1,yend=60),colour='red2',linetype='dashed',size=1)+

      #labels to CAAQS reference lines
      annotate("text",x= year_min+1,y=65,label = '2020 CAAQS',colour = 'white',angle =0,hjust =1) +

      # xlab('Reporting Period')+
      ylab(expression(paste('NO'[2],' 1-Hour Metric (ppb)'))) +
      labs(colour = 'TFEE',
           title = '') +

      theme(legend.position = 'none', legend.direction = 'horizontal',legend.justification = 'left',
            legend.title=element_blank(), legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_24h) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
      scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
      scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
      scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
      xlab('Year')


    a <- print(p1_ann/p1_24h)

  }

  if (parameter %in% c('so2'))
  {
    aq <- df%>%
      filter(grepl('so2',metric)) %>%
      # filter(year >=2012) %>%
      select(-instrument) %>%
      # filter(site == 'Victoria Topaz')   #debug
      filter(tolower(site) == station)  #PROD

    aq_caaqs <- aq %>%
      filter(!grepl('1yr',metric))

    aq_1yr <-
      aq %>%
      filter(grepl('1yr',metric))

    #define the min and max of the data
    year_min = min(min(aq$year),2018)
    year_max = max(max(aq$year),2020)
    # x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
    x_lbls <- year_min:year_max
    #note that ymax_24h is 1-hour metric
    ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'so2_1hr (1yr)'],na.rm=TRUE),75)
    ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'so2_annual (1yr)'],na.rm=TRUE),8)


    p1_ann <-
      aq_caaqs  %>%
      filter(grepl('annual',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=5,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=3,ymax=5),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=2,ymax=3),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=2),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('annual',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('annual',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('annual',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +

      #CAAQS reference lines
      # geom_segment(aes(x=year_min,y=12,xend=2019.5,yend=12),colour='red2',linetype='dashed',size=1)+
      geom_segment(aes(x=year_min-0.2,y=5,xend=year_max + 1,yend=5),colour='red2',linetype='dashed',size=1) +


      #Labels for CAAQS reference lines
      annotate("text",x= year_min+1,y=5+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust =1) +


      # xlab('Reporting Period')+
      ylab(expression(paste('SO'[2],' Annual Metric (ppb)'))) +

      labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

      # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

      theme(legend.position = 'none', legend.direction = 'vertical',legend.justification = 'left',
            legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_ann) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_ann), expand = c(0,0))+
      scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
      scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

      scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted'))


    #the so2 1-hour metric plot--------
    p1_24h <-

      aq_caaqs  %>%
      filter(grepl('1hr',metric)) %>%
      ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

      #management level background
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=70,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=50,ymax=70),alpha=0.1,color=NA,fill='#F46D43') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=30,ymax=50),alpha=0.1,color=NA,fill='#FEE08B') +
      geom_rect(mapping=aes(xmin=year_min-0.2,xmax=year_max + 1,ymin=0,ymax=30),alpha=0.1,color=NA,fill='#A6D96A') +

      #bar graph
      geom_col(aes(x = aq_1yr$year[grepl('1hr',aq_1yr$metric)],
                   y= aq_1yr$metric_value[grepl('1hr',aq_1yr$metric)],
                   fill = aq_1yr$metric[grepl('1hr',aq_1yr$metric)]
      ),
      alpha=1, colour = 'black',linetype = 'solid', width =0.25) +

      #line graph
      geom_line(size=1, colour='black') +
      geom_point(size=4) +


      #CAAQS reference lines
      geom_segment(aes(x=year_min-0.2,y=70,xend=year_max + 1,yend=70),colour='red2',linetype='dashed',size=1)+

      #labels to CAAQS reference lines
      annotate("text",x= year_min+1,y=65,label = '2020 CAAQS',colour = 'white',angle =0,hjust =1) +

      # xlab('Reporting Period')+
      ylab(expression(paste('SO'[2],' 1-Hour Metric (ppb)'))) +


      labs(colour = 'TFEE',
           title = '') +

      theme(legend.position = 'none', legend.direction = 'horizontal',legend.justification = 'left',
            legend.title=element_blank(), legend.key = element_blank(),
            legend.background = element_blank(),
            # axis.text.x = element_blank(),axis.title.x = element_blank(),
            panel.background=element_rect(fill='white',colour='black'),
            panel.border = element_rect(colour='black',fill=NA)
      ) +

      ylim(0,ymax_24h) +

      scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min-0.2,max(year_max+1,2020)),
                         labels = x_lbls,expand=c(0,0)) +
      scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
      scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
      scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
      scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
      xlab('Year')


    a <- print(p1_ann/p1_24h)
  }

  return(a)

}

#' Create a bar graph of the NPRI
#'
#' @param pollutant
#' @param df is the pollutant data. This can be retrieved with get_npri()
#' @param categorytype is either source, sector, or subsector. Default is source
#' @param URL is the ECCC URL for the NPRI
#' @param output is the output type of either 'basic' or 'plotly'
#'
#' @export
plot_npri <- function(pollutant,df=NULL,categorytype = 'Source',URL=NULL,output = 'basic') {
  if (0) {
    pollutant <- c('')
    categorytype <- 'Source'
    output = 'basic'
    URL=NULL
  }

  require(ggplot2)

  df <- get_npri(pollutant = pollutant, df=df, categorytype = categorytype, URL = URL)
  df_npri <- df %>%
    dplyr::rename(groupingcolumn= categorytype)

  #change pollutant for labelling purposes
  label <- pollutant
  if (grepl('pm25',pollutant,ignore.case = TRUE)) {
    label <- expression(PM[2.5]*' tonnes/year')
  }

  if (grepl('pm10',pollutant,ignore.case = TRUE)) {
    label <- expression(PM[10]*' tonnes/year')
  }

  if (grepl('nh3',pollutant,ignore.case = TRUE)) {
    label <- expression(NH[3]*' tonnes/year')
  }

  if (grepl('nox',pollutant,ignore.case = TRUE)) {
    label <- expression(NO[x]*' tonnes/year')
  }
  if (grepl('sox',pollutant,ignore.case = TRUE)) {
    label <- expression(SO[x]*' tonnes/year')
  }


  #to arrange based on value
  levels_grouping <- df_npri %>%
    # filter(Year == max(df_npri$Year)) %>%
    arrange((value)) %>%
    pull(groupingcolumn) %>%
    unique()

  if (tolower(output) == 'basic') {


    a <- df_npri %>%
      filter(!is.na(groupingcolumn)) %>%
      # filter(tolower(groupingcolumn) != 'dust') %>%
      dplyr::mutate(groupingcolumn = factor(groupingcolumn,levels = levels_grouping)) %>%
      # pull(groupingcolumn) %>% unique()
      ggplot(aes(x=Year,y=value,fill = groupingcolumn)) +
      geom_col(colour = 'black') +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            panel.background = element_rect(fill=NA,colour = 'black')) +
      ylab(label) +
      scale_x_continuous(expand=c(0,0)) +
      guides(fill=guide_legend(ncol=3,reverse = TRUE))

    return(a)
  }

  if (tolower(output) == 'plotly') {
    require(plotly)
    a <- {
      plot_ly(df_npri,x=~Year, y= ~value, color = ~groupingcolumn, type = 'bar', source = 'scatter',
              marker = list(line = list(width = 1,color = 'rgb(0, 0, 0)'))
      ) %>%
        layout(barmode = 'stack',yaxis = list(title = paste(pollutant,'(tonnes/year)')))
    }
    return(a)
  }
}

