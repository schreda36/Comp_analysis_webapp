#library(markdown)

shinyUI(
      navbarPage(div(style="color:#1A7ADA; font-style:italic; font-weight:bold", "Compensation Director"),
                 
                   tabPanel("EEO Analysis",
                            tags$head(
                                  tags$style(HTML("
                                          
                                          h1 {  font-family: 'Calibri', cursive;
                                                font-weight: 500;
                                                line-height: 1.1;
                                                color: #48ca3b; }
                                                  
                                          body { background-color: #ffffff; }

                                          .selectize-control { margin-top: 1px }
                                          .selectize-input { font-size: 13px; }
                                          .selectize-control.label { font-size: 20px; }
                                          .selectize-dropdown-content { font-size: 13px; }
                                          #.TblAlign {text-align:right;}
                                          .alignRight {color: #1774D4; margin-right:50px; text-align:right}
                                          #sidebar {background-color: #E1ECF7; align: center;}
                                          //tfoot {display:none;} //hide column searches
                                    "))
                            ),
                            sidebarLayout(
                                  sidebarPanel(id="sidebar",
                                       checkboxGroupInput("GroupBy", "Group people by:",
                                                     c("Job" = "Job",
                                                        "Years of Experience" = "YrsOfExperience",
                                                        "Years of School" = "YrsOfSchool",
                                                        "Prior Performance" = "PriorPerformance",
                                                        "Group X" = "GroupX"
                                                        ),selected="Job"),
                                        tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                        selectizeInput("FactorType", "Split each group by:",
                                                    c("Age"="Age",
                                                      "Disability"="Disability", 
                                                      "Ethnicity"="Ethnicity",
                                                      "Gender"="Gender", 
                                                      "Religion"="Religion",
                                                      "Veteran Status"="Veteran", 
                                                      "Factor X"="FactorX"
                                                      ),selected="Gender"
                                        ),
                                       tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                       selectizeInput("DataType", "Data to analyze:",
                                                      c("Total Compensation"="TotalCompensation",
                                                        "Salary"="Salary", 
                                                        "Merit Increase"="MeritInc",
                                                        "Promotion Increase"="PromoInc",
                                                        "Bonus"="BonusAmt",
                                                        "Stock"="StockAmt",
                                                        "Data X"="DataX"
                                                      ),selected="Salary"
                                       ),
                                       tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                       selectizeInput("FilterType", "Filter by:",
                                                    c("Country" = "Country",
                                                      "Location" = "Location",
                                                      "Organization" = "Organization"
                                                      ),selected="Country"
                                        ),
                                        selectizeInput("Filter", NULL, choices="", selected = "USA"),
                                        tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                        sliderInput("Difference", "Display differences > than +/-:",
                                                    min=0.0,max=20.0,round=TRUE,
                                                    step = .5, post="%",
                                                    value=10, width=200
                                        ),
                                        tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                        div(style="display:inline-block; margin-top: 5px",actionButton("goButton", "Analyze", style="align: center")),
                                        div(style="display:inline-block; margin-top: 5px",downloadButton('downloadData', 'Download')),
                                        tags$hr(style="height: 1px; margin: 5px;"),
                                        radioButtons("downloadType", "Download file type:",
                                                     choices = c("csv", "pdf", "text"), inline=T)
                                  ,width=3),
                                  mainPanel(
                                        tabsetPanel(type = "tabs", 
                                                    tabPanel("Table", dataTableOutput("table")),
                                                    tabPanel("Plot", 
                                                             fluidRow(column(width = 12,
                                                                        plotOutput("plot", click = "plot_click", brush=brushOpts(id="plot_brush",resetOnNew=F))
                                                             )),
                                                             fluidRow(column(width = 12,
                                                                        dataTableOutput("click_info")
                                                             ))
                                                    ),
                                                    tabPanel("Detail", 
                                                             fluidRow(column(width = 12,
                                                                        verbatimTextOutput("detail")
                                                             )),
                                                             fluidRow(column(width = 12,
                                                                        plotOutput("detailHist")
                                                             ))
                                                    )
                                                    
                                        )
                                  ,width=9)
                            )
                   )
                   
                   ,tabPanel("Comp Analysis",
                             tags$head(
                                   tags$style(HTML("
                                                   
                                                   h1 {  font-family: 'Calibri', cursive;
                                                   font-weight: 500;
                                                   line-height: 1.1;
                                                   color: #48ca3b; }
                                                   
                                                   body { background-color: #ffffff; }
                                                   
                                                   .selectize-control { margin-top: 1px }
                                                   .selectize-input { font-size: 13px; }
                                                   .selectize-control.label { font-size: 20px; }
                                                   .selectize-dropdown-content { font-size: 13px; }
                                                   #.TblAlign {text-align:right;}
                                                   .alignRight {color: #1774D4; margin-right:50px; text-align:right}
                                                   #sidebar2 {background-color: #E1ECF7; align: center;}
                                                   "))
                                   ),
                             sidebarLayout(
                                   sidebarPanel(id="sidebar2",
                                                checkboxGroupInput("GroupBy2", "Group people by:",
                                                                   c("Manager" = "ManagerID",
                                                                     "Salary Grade" = "SalaryGrade",
                                                                     "Performance Rating" = "PriorPerformance",
                                                                     "Quartile" = "YrsOfSchool",
                                                                     "Percentile" = "Percentile",
                                                                     "Group X" = "GroupX"
                                                                   ),selected="ManagerID"
                                                ),
                                                tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                                selectizeInput("FactorType2", "Split each group by:",
                                                               c("Optional"="",
                                                                 "Age"="Age",
                                                                 "Disability"="Disability", 
                                                                 "Ethnicity"="Ethnicity",
                                                                 "Gender"="Gender", 
                                                                 "Factor X"="FactorX"
                                                               )
                                                ),
                                                tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                                div(style="display:inline-block; margin-top: 5px",
                                                    selectizeInput("CompData1", "Column 1:",
                                                                   c("Total Comp"="TotalCompensation",
                                                                     "Salary"="Salary", 
                                                                     "Merit Increase"="MeritInc",
                                                                     "Promo Increase"="PromoInc",
                                                                     "Bonus"="BonusAmt",
                                                                     "Stock"="StockAmt",
                                                                     "Data X"="DataX"
                                                                   ),selected="Total Compensation"
                                                                   ,width=145)),
                                                div(style="display:inline-block; margin-top: 0px",
                                                    selectizeInput("CompEq1", "",
                                                                   c("Avg"="comp.mean",
                                                                     "Sum"="comp.sum"
                                                                   ),selected="comp.mean"
                                                                   ,width=70)),
                                                tags$hr(style="height: 0px; margin: 0px;"),
                                                div(style="display:inline-block; margin-top: 5px",
                                                    selectizeInput("CompData2", "Column 2 (optional):",
                                                                   c("Optional"="",
                                                                     "Total Comp"="TotalCompensation",
                                                                     "Salary"="Salary", 
                                                                     "Merit Increase"="MeritInc",
                                                                     "Promo Increase"="PromoInc",
                                                                     "Bonus"="BonusAmt",
                                                                     "Stock"="StockAmt",
                                                                     "Data X"="DataX"
                                                                   )
                                                                   ,width=145)),
                                                div(style="display:inline-block; margin-top: 0px",
                                                    selectizeInput("CompEq2", "",
                                                                   c("Avg"="Avg",
                                                                     "Sum"="Sum"
                                                                   ),selected="Avg"
                                                                   ,width=70)),

                                                tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                                selectizeInput("FilterType2", "Filter by:",
                                                               c("Optional"="",
                                                                 "Country" = "Country",
                                                                 "Location" = "Location",
                                                                 "Organization" = "Organization"
                                                               )
                                                ),
                                                selectizeInput("Filter2", NULL, choices="", selected = "USA"),
                                                tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                                div(style="display:inline-block; margin-top: 5px",actionButton("goButton2", "Analyze", style="align: center")),
                                                div(style="display:inline-block; margin-top: 5px",downloadButton('downloadData2', 'Download')),
                                                tags$hr(style="height: 1px; margin: 5px;"),
                                                radioButtons("downloadType2", "Download file type:",
                                                             choices = c("csv", "pdf", "text"), inline=T)
                                                ,width=3),
                                   mainPanel(
                                         tabsetPanel(type = "tabs", 
                                                     tabPanel("Table", dataTableOutput("table2")),
                                                     tabPanel("Plot", 
                                                              fluidRow(column(width = 12,
                                                                              plotOutput("plot2", click = "plot_click2", brush=brushOpts(id="plot_brush",resetOnNew=F))
                                                              )),
                                                              fluidRow(column(width = 12,
                                                                              dataTableOutput("click_info2")
                                                              ))
                                                     ),
                                                     tabPanel("Detail", 
                                                              fluidRow(column(width = 12,
                                                                              verbatimTextOutput("detail2")
                                                              )),
                                                              fluidRow(column(width = 12,
                                                                              plotOutput("detailHist2")
                                                              ))
                                                     )
                                                     
                                         )
                                         ,width=9)
                             )
                    )
                 
                   ,tabPanel("Comp Forecasting",
                            verbatimTextOutput("summary")
                   )
                 
                   # ,navbarMenu("More",
                   #            tabPanel("Table",
                   #                     verbatimTextOutput("summary")
                   #            ),
                   #            tabPanel("About",
                   #                     fluidRow(
                   #                           column(6,
                   #                                  verbatimTextOutput("summary")
                   #                                  #includeMarkdown("about.md")
                   #                           )
                   #                           ,
                   #                           column(3,
                   #                                  img(class="img-polaroid",
                   #                                      src=paste0("http://www.vayam-llc.com/wp-content/themes/Vayam%20LLC/images/vayam-logo-small.png")),
                   #                                  tags$small(
                   #                                        "Copyright 2016 ",
                   #                                        "Vayam LLC ",
                   #                                        a(href="http://www.vayam-llc.com")
                   #                                  )
                   #                           )
                   #                     )
                   #            )
                   # )
                 
))