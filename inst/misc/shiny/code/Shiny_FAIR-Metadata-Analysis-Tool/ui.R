############ UI block ############

ui <- fluidPage(
  tags$h1("FAIR Metadata Analysis Tool"),
  fluidRow(
    column(4, 
           verbatimTextOutput("most_recent_date"),
           wellPanel(sliderInput(inputId = "timeframe", 
                                 label = "Date Range:",
                                 min = as.Date("2016-03-21","%Y-%m-%d"),
                                 max = as.Date(Sys.Date(),"%Y-%m-%d"),
                                 value = c(Sys.Date()-14, Sys.Date()), 
                                 timeFormat="%Y-%m-%d", step = 7),
                     actionButton(inputId = "clicks",
                                  label = "Update Timespan")),
           wellPanel(htmlOutput("data_package_info"))),
    column(8,
           plotOutput("binned_scatterplot_packageLevel", click = "click_data_package_info")),
    fluidRow(
      column(4, plotOutput("barplot_detailed_scores")),
      column(8, plotOutput("linegraph_FAIR_overview"))
    )
  )
)