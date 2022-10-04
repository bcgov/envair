# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

  })
  #filter station list based on parameter
  output$stationSelect <- renderUI({
    param_ <- recode(input$Parameter,'PM\u2082.\u2085' = 'pm2.5',
                     'NO\u2082' = 'no2',
                     'SO\u2082' ='so2',
                     'Ozone' = 'ozone')
    site_list <- aq_summary %>%
      filter(grepl(param_,metric,ignore.case = TRUE)) %>%
      pull(site) %>%
      unique() %>% sort()
    selectInput("Station","Select Station to Display:",choices = site_list)
  })


  output$plot1 <- renderPlot({
    aq_result <- aq_summary %>%
      dplyr::filter(site == input$Station)  %>%
      arrange(desc(metric_value))

    param_ <- recode(input$Parameter,'PM\u2082.\u2085' = 'pm2.5',
                     'NO\u2082' = 'no2',
                     'SO\u2082' ='so2',
                     'Ozone' = 'ozone')


    #customized graphing for pm2.5, no2, and so2
    if (param_ == 'pm2.5')
    {

      aq <- aq_summary%>%
        filter(grepl('pm2.5',metric)) %>%
        filter(year >=2012) %>%
        # filter(site == 'Victoria Topaz')   #debug
        filter(site == input$Station)  #PROD

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
        mutate(`pm2.5_24h (1yr)` = `pm2.5_24h (1yr)` - `pm2.5_24h_tfee (1yr)`,
               `pm2.5_annual (1yr)` = `pm2.5_annual (1yr)`- `pm2.5_annual_tfee (1yr)`
        ) %>%
        # View()
        pivot_longer(cols = -c('site','instrument','year'),
                     names_to = 'metric', values_to = 'metric_value')


      #define the min and max of the data
      year_min = min(min(aq$year),2018)
      year_max = max(max(aq$year),2020)
      x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
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

        #management level background
        geom_rect(mapping=aes(xmin=year_min,xmax=2019.5,ymin=10,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
        geom_rect(mapping=aes(xmin=2019.5,xmax=year_max+1,ymin=8.8,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +

        geom_rect(mapping=aes(xmin=year_min,xmax=2019.5,ymin=6.4,ymax=10),alpha=0.1,color=NA,fill='#F46D43') +
        geom_rect(mapping=aes(xmin=2019.5,xmax=year_max + 1,ymin=6.4,ymax=8.8),alpha=0.1,color=NA,fill='#F46D43') +

        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=4,ymax=6.4),alpha=0.1,color=NA,fill='#FEE08B') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=4),alpha=0.1,color=NA,fill='#A6D96A') +

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
        geom_segment(aes(x=year_min,y=10,xend=2019.5,yend=10),colour='red2',linetype='dashed',size=1)+
        geom_segment(aes(x=2019.5,y=8.8,xend=year_max + 1,yend=8.8),colour='red2',linetype='dashed',size=1) +
        geom_segment(aes(x=2019.5,y=8.8,xend=2019.5,yend=10),colour='red2',linetype='dashed',size=1)+


        #Labels for CAAQS reference lines
        annotate("text",x= year_min+1,y=10+1,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
        annotate("text",x= year_max,y=8.8+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust=0.2) +

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
              axis.text.x = element_blank(),axis.title.x = element_blank(),
              panel.background=element_rect(fill='white',colour='black'),
              panel.border = element_rect(colour='black',fill=NA)
        ) +
        # theme_minimal() +

        ylim(0,ymax_ann) +

        scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
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
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max - 0.5,ymin=28,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
        geom_rect(mapping=aes(xmin=year_max - 0.5,xmax=year_max + 1,ymin=27,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +

        geom_rect(mapping=aes(xmin=year_min,xmax=year_max - 0.5,ymin=19,ymax=28),alpha=0.1,color=NA,fill='#F46D43') +
        geom_rect(mapping=aes(xmin=year_max - 0.5,xmax=year_max + 1,ymin=19,ymax=27),alpha=0.1,color=NA,fill='#F46D43') +

        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=10,ymax=19),alpha=0.1,color=NA,fill='#FEE08B') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=10),alpha=0.1,color=NA,fill='#A6D96A') +

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
        geom_segment(aes(x=year_min,y=28,xend=year_max - 0.5,yend=28),colour='red2',linetype='dashed',size=1)+
        geom_segment(aes(x=year_max - 0.5,y=27,xend=year_max + 1,yend=27),colour='red2',linetype='dashed',size=1) +
        geom_segment(aes(x=year_min,y=28,xend=year_max - 0.5,yend=28),colour='red2',linetype='dashed',size=1)+

        #labels to CAAQS reference lines
        annotate("text",x= year_min+1,y=28+4,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
        annotate("text",x= year_max,y=27+4,label = '2020 CAAQS',colour = 'white',angle =0,hjust=0.2) +

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

        scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
                           labels = x_lbls,expand=c(0,0)) +
        scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
        scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
        scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
        scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
        xlab('Three-year CAAQS Reporting Period')
      # theme(legend.position = 'none')
      print(p1_ann/p1_24h)
    }

    if (param_ == 'ozone')
    {
      aq <- aq_summary%>%
        filter(grepl('ozone',metric)) %>%
        filter(year >=2012) %>%
        # filter(site == 'Victoria Topaz')   #debug
        filter(site == input$Station)  #PROD



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
      x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')
      ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'ozone_4th (1yr)'],na.rm=TRUE),70)



      p1_ann <-

        aq_caaqs  %>%
        ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

        #management level background
        geom_rect(mapping=aes(xmin=year_min,xmax=2019.5,ymin=63,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
        geom_rect(mapping=aes(xmin=2019.5,xmax=year_max+1,ymin=62,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +

        geom_rect(mapping=aes(xmin=year_min,xmax=2019.5,ymin=56,ymax=63),alpha=0.1,color=NA,fill='#F46D43') +
        geom_rect(mapping=aes(xmin=2019.5,xmax=year_max + 1,ymin=56,ymax=62),alpha=0.1,color=NA,fill='#F46D43') +

        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=50,ymax=56),alpha=0.1,color=NA,fill='#FEE08B') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=30,ymax=50),alpha=0.1,color=NA,fill='#A6D96A') +

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
        geom_segment(aes(x=year_min,y=63,xend=2019.5,yend=63),colour='red2',linetype='dashed',size=1)+
        geom_segment(aes(x=2019.5,y=62,xend=year_max + 1,yend=62),colour='red2',linetype='dashed',size=1) +
        geom_segment(aes(x=2019.5,y=62,xend=2019.5,yend=63),colour='red2',linetype='dashed',size=1)+


        #Labels for CAAQS reference lines
        annotate("text",x= year_min+1,y=63+2,label = '2015 CAAQS',colour = 'white',angle =0,hjust =0) +
        annotate("text",x= year_max,y=62+2,label = '2020 CAAQS',colour = 'white',angle =0,hjust=0.2) +

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
        scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,year_max+1),
                           labels = x_lbls,expand=c(0,0)) +
        scale_y_continuous(limits = c(30,ymax_ann), expand = c(0,0),oob=rescale_none,trans = 'reverse')+
        scale_colour_manual(name = 'CAAQS Metrics (3-Year)', values =c('darkorange3','deepskyblue3'),labels = c('No Adjustment','Wildfire-adjusted')) +
        scale_linetype_manual(name = 'CAAQS Metrics (3-Year)',values = c('dashed','solid'),labels=c('No Adjustment','Wildfire-adjusted')) +

        scale_fill_manual(name = 'Single Year Levels', values =c('gray88','grey60'),labels = c('No Adjustment','Wildfire-adjusted')) +
        xlab('Three-year CAAQS Reporting Period')

      print(p1_ann)

    }

    if (param_ %in% c('no2'))
    {
      aq <- aq_summary%>%
        filter(grepl('no2',metric)) %>%
        filter(year >=2012) %>%
        select(-instrument) %>%
        # filter(site == 'Victoria Topaz')   #debug
        filter(site == input$Station)  #PROD

      aq_caaqs <- aq %>%
        filter(!grepl('1yr',metric))

      aq_1yr <-
        aq %>%
        filter(grepl('1yr',metric))

      #define the min and max of the data
      year_min = min(min(aq$year),2018)
      year_max = max(max(aq$year),2020)
      x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')

      #note that ymax_24h is 1-hour metric
      ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'no2_1hr (1yr)'],na.rm=TRUE),70)
      ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'no2_annual (1yr)'],na.rm=TRUE),20)


      p1_ann <-
        aq_caaqs  %>%
        filter(grepl('annual',metric)) %>%
        ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

        #management level background
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=17,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=7,ymax=17),alpha=0.1,color=NA,fill='#F46D43') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=2,ymax=7),alpha=0.1,color=NA,fill='#FEE08B') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=2),alpha=0.1,color=NA,fill='#A6D96A') +

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
        geom_segment(aes(x=year_min,y=17,xend=year_max + 1,yend=17),colour='red2',linetype='dashed',size=1) +


        #Labels for CAAQS reference lines
        annotate("text",x= year_min+1,y=17+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust =0) +


        # xlab('Reporting Period')+
        ylab(expression(paste('NO'[2],' Annual Metric (ppb)'))) +

        labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

        # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

        theme(legend.position = 'none', legend.direction = 'vertical',legend.justification = 'left',
              legend.key = element_blank(),
              legend.background = element_blank(),
              axis.text.x = element_blank(),axis.title.x = element_blank(),
              panel.background=element_rect(fill='white',colour='black'),
              panel.border = element_rect(colour='black',fill=NA)
        ) +

        ylim(0,ymax_ann) +

        scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
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
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=60,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=31,ymax=60),alpha=0.1,color=NA,fill='#F46D43') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=20,ymax=31),alpha=0.1,color=NA,fill='#FEE08B') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=20),alpha=0.1,color=NA,fill='#A6D96A') +

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
        geom_segment(aes(x=year_min,y=60,xend=year_max + 1,yend=60),colour='red2',linetype='dashed',size=1)+

        #labels to CAAQS reference lines
        annotate("text",x= year_min+1,y=65,label = '2020 CAAQS',colour = 'white',angle =0,hjust =0) +

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

        scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
                           labels = x_lbls,expand=c(0,0)) +
        scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
        scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
        scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
        scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
        xlab('Three-year CAAQS Reporting Period')


      print(p1_ann/p1_24h)

    }

    if (param_ %in% c('so2'))
    {
      aq <- aq_summary%>%
        filter(grepl('so2',metric)) %>%
        filter(year >=2012) %>%
        select(-instrument) %>%
        # filter(site == 'Victoria Topaz')   #debug
        filter(site == input$Station)  #PROD

      aq_caaqs <- aq %>%
        filter(!grepl('1yr',metric))

      aq_1yr <-
        aq %>%
        filter(grepl('1yr',metric))

      #define the min and max of the data
      year_min = min(min(aq$year),2018)
      year_max = max(max(aq$year),2020)
      x_lbls <- paste( (year_min-2):(year_max-2),year_min:year_max,sep='-')

      #note that ymax_24h is 1-hour metric
      ymax_24h <- max(max(5+aq$metric_value[aq$metric == 'so2_1hr (1yr)'],na.rm=TRUE),75)
      ymax_ann <-  max(max(2+aq$metric_value[aq$metric == 'so2_annual (1yr)'],na.rm=TRUE),8)


      p1_ann <-
        aq_caaqs  %>%
        filter(grepl('annual',metric)) %>%
        ggplot(aes(x=year, y= metric_value,linetype = metric,colour = metric))  +

        #management level background
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=5,ymax=ymax_ann),alpha=0.1,color=NA,fill='#A50026') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=3,ymax=5),alpha=0.1,color=NA,fill='#F46D43') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=2,ymax=3),alpha=0.1,color=NA,fill='#FEE08B') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=2),alpha=0.1,color=NA,fill='#A6D96A') +

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
        geom_segment(aes(x=year_min,y=5,xend=year_max + 1,yend=5),colour='red2',linetype='dashed',size=1) +


        #Labels for CAAQS reference lines
        annotate("text",x= year_min+1,y=5+1,label = '2020 CAAQS',colour = 'white',angle =0,hjust =0) +


        # xlab('Reporting Period')+
        ylab(expression(paste('SO'[2],' Annual Metric (ppb)'))) +

        labs(colour = 'Location') + #paste(AIRZONE_,' (',year_min,'-',max(reporting_year),')',sep='')) +

        # geom_text(aes(x=site,y=txt_position,label=txt_label),angle = 270) +

        theme(legend.position = 'none', legend.direction = 'vertical',legend.justification = 'left',
              legend.key = element_blank(),
              legend.background = element_blank(),
              axis.text.x = element_blank(),axis.title.x = element_blank(),
              panel.background=element_rect(fill='white',colour='black'),
              panel.border = element_rect(colour='black',fill=NA)
        ) +

        ylim(0,ymax_ann) +

        scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
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
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=70,ymax=ymax_24h),alpha=0.1,color=NA,fill='#A50026') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=50,ymax=70),alpha=0.1,color=NA,fill='#F46D43') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=30,ymax=50),alpha=0.1,color=NA,fill='#FEE08B') +
        geom_rect(mapping=aes(xmin=year_min,xmax=year_max + 1,ymin=0,ymax=30),alpha=0.1,color=NA,fill='#A6D96A') +

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
        geom_segment(aes(x=year_min,y=70,xend=year_max + 1,yend=70),colour='red2',linetype='dashed',size=1)+

        #labels to CAAQS reference lines
        annotate("text",x= year_min+1,y=65,label = '2020 CAAQS',colour = 'white',angle =0,hjust =0) +

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

        scale_x_continuous(breaks = c(year_min:year_max),limits = c(year_min,max(year_max+1,2020)),
                           labels = x_lbls,expand=c(0,0)) +
        scale_y_continuous(limits = c(0,ymax_24h), expand = c(0,0))+
        scale_linetype_manual(name = 'TFEE',values = c('dashed','solid')) +
        scale_colour_manual(name = 'TFEE', values =c('darkorange3','deepskyblue3')) +
        scale_fill_manual(name = 'TFEE', values =c('gray88','grey60')) +
        xlab('Three-year CAAQS Reporting Period')


      print(p1_ann/p1_24h)
    }
  })
}
