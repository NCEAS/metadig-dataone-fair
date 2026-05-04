############ Server block ############ 
server <- function(input, output) {
  
  #report dateTime of most recent dataset upload within the FAIR score dataset
  output$most_recent_date <- renderText(paste0("most recent data uploaded on ", as.POSIXct(most_recent_upload$date), " PT"))
  
  data_subset <- eventReactive(input$clicks, {
    aggScore_clean %>%
      dplyr::filter(dateUploaded >= input$timeframe[1] & dateUploaded <= input$timeframe[2])},
    ignoreNULL = FALSE)
  
  
  
  ####barplot
  output$barplot_detailed_scores <- renderPlot({
    
    plotData_sidewaysScatter <- data_subset() %>%
      dplyr::filter(dateSplit != "INTERMEDIATE") %>% 
      group_by(dateSplit) %>% 
      summarise(OVERALL = mean(scoreOverall),
                Findable = mean(scoreFindable),
                Accessible = mean(scoreAccessible),
                Interoperable = mean(scoreInteroperable),
                Reusable = mean(scoreReusable)) %>%
      pivot_longer(cols=c(OVERALL, Findable, Accessible, Interoperable, Reusable),
                   names_to = "scoreType",
                   values_to = "meanScore")
    
    plotData_sidewaysScatter$scoreType <- factor(plotData_sidewaysScatter$scoreType, levels = c("Reusable", "Interoperable", "Accessible", "Findable", "OVERALL"))
    
    
    

    sideways_binned_scatterplot <- ggplot(plotData_sidewaysScatter, aes(x=meanScore, y=scoreType)) +
      geom_line(aes(group=scoreType), color="gray60", size=1.5) +
      geom_point(aes(fill=dateSplit, shape=dateSplit, size=dateSplit)) +
      scale_shape_manual(values=shapeValues,
                         name="",
                         breaks=c("INITIAL", "FINAL"),
                         labels=c("Initial  ", "Most Recent")) +
      scale_fill_manual(values=fillValues,
                        name="",
                        breaks=c("INITIAL", "FINAL"),
                        labels=c("Initial  ", "Most Recent")) +
      scale_size_manual(values=sizeValues,
                        name="",
                        breaks=c("INITIAL", "FINAL"),
                        labels=c("Initial  ", "Most Recent")) +
      xlim(0,1) +
      theme_ADC_modified +
      xlab("Mean Score for the Selected Time Period") +
      ylab("") +
      theme(legend.position="top")
    
    sideways_binned_scatterplot
    
  })
  
  
  
  ##### create plot for the FAIR score for each version of a sequenceID ####
  output$binned_scatterplot_packageLevel <- renderPlot({
    
    #obtain sequenceIds for any updated within from user-specified timeframe
    sequenceId_over_timeperiod <- data_subset() %>%
      dplyr::summarize(sequenceId = unique(sequenceId))
    
    #subset dataframe using list of sequenceIds
    plotData_dataPackages <- aggScore_clean[aggScore_clean$sequenceId %in% sequenceId_over_timeperiod$sequenceId,]
    
    #order sequenceIds factor levels by chronology
    seqId_axis_order_chronology <- plotData_dataPackages %>% 
      group_by(sequenceId) %>% 
      arrange(dateUploaded, pid) %>% 
      slice(tail(row_number(), 1)) %>% 
      select(sequenceId, dateUploaded)
    
    #graph overall scores on y-axis and sequenceIds on the x-axis, with the score of each pid represented by a point
    
    scatter_plot <- ggplot(data=plotData_dataPackages, aes(x=sequenceId, y=scoreOverall)) +
      geom_jitter(data=plotData_dataPackages[plotData_dataPackages$dateSplit=="INTERMEDIATE",], aes(color=aesMap, fill=aesMap, shape=aesMap, size=aesMap), alpha=0.3, width=0.3, height=0) +
      geom_point(data=plotData_dataPackages[plotData_dataPackages$dateSplit!="INTERMEDIATE",], aes(color=aesMap, fill=aesMap, shape=aesMap, size=aesMap)) +
      geom_point(data=plotData_dataPackages[plotData_dataPackages$dateSplit=="FINAL" & plotData_dataPackages$DOI_present=="DOI",], aes(color=aesMap, fill=aesMap, shape=aesMap, size=aesMap)) +
      theme_ADC_modified +
      ylim(0,1) +
      ylab("Overall Score") +
      xlab("Unique Data Packages for Selected Time Period \n (ordered chronologically by most recent update)") +
      scale_x_discrete(limits = seqId_axis_order_chronology$sequenceId[order(seqId_axis_order_chronology$dateUploaded)]) +
      scale_fill_manual(values=fillValues,
                        name="",
                        breaks=c("INITIAL", "INTERMEDIATE", "FINAL", "DOI"),
                        labels=c("Initial  ", "Intermediate  ", "Most Recent  ", "Most Recent w/ Issued DOI")) +
      scale_color_manual(values=colorValues,
                         name="",
                         breaks=c("INITIAL", "INTERMEDIATE", "FINAL", "DOI"),
                         labels=c("Initial  ", "Intermediate  ", "Most Recent  ", "Most Recent w/ Issued DOI")) +
      scale_shape_manual(values=shapeValues,
                         name="",
                         breaks=c("INITIAL", "INTERMEDIATE", "FINAL", "DOI"),
                         labels=c("Initial  ", "Intermediate  ", "Most Recent  ", "Most Recent w/ Issued DOI")) +
      scale_size_manual(values=sizeValues,
                        name="",
                        breaks=c("INITIAL", "INTERMEDIATE", "FINAL", "DOI"),
                        labels=c("Initial  ", "Intermediate  ", "Most Recent  ", "Most Recent w/ Issued DOI")) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    scatter_plot
  })
  
  
  ########## NEW ##############
  
  output$data_package_info <- renderText({
    test_object <- nearPoints(aggScore_clean, input$click_data_package_info, threshold = 5, maxpoints = 1) %>% 
      dplyr::select(pid, dateUploaded, sequenceId)
    
    paste("<B>Title:</B> [field not yet functional]", "<br><B>Submitter:</B> [field not yet functional]", "<br><B>PID:</B>" , test_object$pid, "<br><B>Date Uploaded:</B>", test_object$dateUploaded)
    
  })
  
  
  #### FAIR score time series ####
  output$linegraph_FAIR_overview <- renderPlot({
    
    #summarize FAIR scores and uploads on a monthly basis
    gganimate_NSF_monthly <- aggregate_score_ADC %>%
      filter(dateUploaded > as.Date("2016-03-20")) %>%
      mutate(year = lubridate::year(dateUploaded),
             month = lubridate::month(dateUploaded),
             week = lubridate::week(dateUploaded),
             date_floor = lubridate::floor_date(dateUploaded, "1 month")) %>%
      group_by(year, month, date_floor) %>%
      summarize(n=n(),
                meanOverall = mean(scoreOverall),
                meanFindable = mean(scoreFindable),
                meanAccessible = mean(scoreAccessible),
                meanInteroperable = mean(scoreInteroperable),
                meanReusable = mean(scoreReusable))
    
    gganimate_NSF_monthly_nOnly <- gganimate_NSF_monthly %>% 
      select(date_floor, n)
    
    gganimate_NSF_monthly <- gganimate_NSF_monthly %>% 
      select(!n) %>% 
      pivot_longer(cols = c(meanOverall, meanFindable, meanAccessible, meanInteroperable, meanReusable),
                   names_to = "type",
                   values_to = "score")
    
    #set levels for better plotting later
    gganimate_NSF_monthly$type <- factor(gganimate_NSF_monthly$type, levels = c("meanOverall", "meanFindable", "meanAccessible", "meanInteroperable", "meanReusable"))
    
    #set graphic parameters
    colorValues <- c("meanOverall" = "black",
                     "meanFindable" = "darkgreen", 
                     "meanAccessible" = "darkblue",
                     "meanInteroperable" = "orange",
                     "meanReusable" = "firebrick")
    
    lineValues <- c("meanOverall" = "solid",
                    "meanFindable" = "dashed", 
                    "meanAccessible" = "dashed",
                    "meanInteroperable" = "dashed",
                    "meanReusable" = "dashed")
    
    sizeValues <- c("meanOverall" = 1.5,
                    "meanFindable" = 0.5, 
                    "meanAccessible" = 0.5,
                    "meanInteroperable" = 0.5,
                    "meanReusable" = 0.5)
    
    alphaValues <- c("meanOverall" = 1.0,
                     "meanFindable" = 0.75, 
                     "meanAccessible" = 0.75,
                     "meanInteroperable" = 0.75,
                     "meanReusable" = 0.75)
    
    
    #create static figure
    ggplot() +
      geom_bar(data = gganimate_NSF_monthly_nOnly, aes(x=date_floor, y=n, group=seq_along(date_floor)), fill="gray65", stat = 'identity', alpha=0.8) +
      geom_line(data = gganimate_NSF_monthly, aes(x=date_floor, y=score*4000, linetype=type, color=type, size=type, alpha=type)) +
      scale_color_manual(values=colorValues,
                         name="",
                         labels=c("Overall", "Findable", "Accessible", "Interoperable", "Reusable")) +
      scale_linetype_manual(values=lineValues,
                            name="",
                            labels=c("Overall", "Findable", "Accessible", "Interoperable", "Reusable")) +
      scale_size_manual(values=sizeValues,
                        name="",
                        labels=c("Overall", "Findable", "Accessible", "Interoperable", "Reusable")) +
      scale_alpha_manual(values=alphaValues,
                         name="",
                         labels=c("Overall", "Findable", "Accessible", "Interoperable", "Reusable")) +
      scale_y_continuous(name = 'Monthly Dataset Uploads', 
                         sec.axis = sec_axis(~./4000, name = "Mean Monthly FAIR Score")) +
      labs(x = "Date") +
      scale_x_datetime(date_breaks = "1 year", date_labels="%Y") +
      theme_ADC_modified +
      theme(legend.position = "top") +
      theme(axis.line.y.left = element_line(color = "gray40"),
            axis.ticks.y.left = element_line(color = "gray40"),
            axis.text.y.left = element_text(color="gray40"),
            axis.title.y.left = element_text(color="gray40")) +
      annotate('rect', xmin = as.POSIXct(input$timeframe[1]), xmax = as.POSIXct(input$timeframe[2]), ymin = -Inf, ymax = Inf, fill='gray80', alpha=0.3)
    
  })
  
  
  
}