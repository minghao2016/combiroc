# Copyright 2015 Paul Govan

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

source("scripts.r")
library(shiny)
library(shinyapps)
library(shinydashboard)  #shinydashboard makes it easy to use Shiny to create pretty dashboards
#library(googleVis)
#library(googleCharts)
#library(plotly)
library(ggplot2)
#library(shinyjs)
library(DT)              # DT: An R interface to the javaScript library DataTables library
library(rCharts)         # R package to create, customize and publish interactive javascript visualizations from R using a familiar lattice style plotting interface.
library(grDevices)
library(ROCR)            # Visualizing the Performance of Scoring Classifiers
library(pROC)            # An R package to display and analyze ROC curves
library(stringr)         # Simple, Consistent Wrappers for Common String Operations
library(cvAUC)           # Cross-Validated Area Under the ROC Curve Confidence Intervals
library(STRINGdb)        # STRINGdb (Search Tool for the Retrieval of Interacting proteins database)      
library(networkD3)       # D3 JavaScript Network Graphs from R
library(d3Network)
library(RCurl)
library(visNetwork)      # R package for network visualization, using vis.js javascript library (http://visjs.org)
library(knitr)

         #to create animated bubble charts
footer=h6("This application was created by the", a("Protein Microarray", target="_blank",href="http://www.ingm.org/en/research/facilities/protein-microarray"),
" and ", a("Bioinformatics",target="_blank",href="http://www.ingm.org/en/research/facilities/bioinformatics"),
" units at INGM. If you have questions or comments, please contact Saveria Mazzara (mazzara(at)ingm.org). This application was implemented using shiny (for web interface) and other additional R packages (for data manipulation).")

 

dashboardPage(skin="black",
              dashboardHeader(title = "CombiROC",
                              dropdownMenu(type = "messages",
                                           messageItem(
                                             from = "Support",
                                             message = "Welcome to combiROC!"
                                           )
                              )),
              dashboardSidebar(
                      tags$hr(),
              #sidebarSearchForm( textId = "searchText", buttonId = "selectInput",
#                    label = "Search...",icon = shiny::icon("search")),
#                     
               tags$head( tags$script(src = "custom.js"),tags$script(src = "custom2.js"),tags$script(src = "custom3.js"),tags$script(src = "custom4.js"),tags$script(src = "customprova.js")),   #!!!!!!!Enables linking to specific tabs

                  sidebarMenu(id="tabs",

                      menuItem("Home",tabName="main",icon=icon("home")),

                      menuItem("Data",tabName="data",icon=icon("gear"),menuSubItem("Upload",tabName="upload"),menuSubItem("Plots",tabName="plots"),menuSubItem("Pre-processing (optional)",tabName="preprocessing")),
                      menuItem("Analysis",tabName="analysis",icon=icon("spinner"),menuSubItem("Combinatorial analysis",tabName="combi"),menuSubItem("Gold combinations",tabName="gold"),menuSubItem("ROC analysis",tabName="roc")), #,menuSubItem("Interaction map",tabName="network")
                      menuItem("Download",tabName="download",icon=icon("download")),
                      menuItem("Accessories",tabName="accessories",icon=icon("paperclip"),menuSubItem("Tutorial",tabName="tutorial"),menuSubItem("Logs",tabName="history"),menuSubItem("Contacts",tabName="contact"),menuSubItem("FAQ",tabName="FAQ"))
                  ),
                  tags$hr(),
                  conditionalPanel("input.tabs=='plots' ",
                    sidebarMenu(
                     #tags$div(menuItem("Prova",icon = icon("chevron-circle-right"))),

                     tags$div( # a div to highlight "Display" and "Option" item
                     menuItem("Display",icon = icon("chevron-circle-right"), tabName="display",
                        fluidRow(
                                column(1),
                                column(10, radioButtons("display", "",c("Box plot"="1","Marker plot"="3"), #,"Correlation plot"="2"
                                selected="1"))
                              )
                              #fluidRow(
#                                column(1),
#                                column(10, checkboxInput("correlation", "Correlation plot", TRUE))
#                              )
                      ),class="my-item"

                     ),

                                        
                      tags$br(),

                      tags$div(
                       menuItem("Options",tabName="options",icon = icon("chevron-circle-right"),
                               fluidRow(
                                column(1),
                                column(10, checkboxGroupInput("data_option",NULL, c("change data options"=TRUE),FALSE))
                              ),
                              fluidRow(
                                column(1),
                                column(10, checkboxGroupInput("display_option",NULL, c("change display option"=TRUE), FALSE))
                              ),
                               fluidRow(
                                column(1),
                                column(10, checkboxGroupInput("plot_option",NULL, c("change plot labels"=TRUE), FALSE))
                              )
                      ),class="my-item")
                      
                      )
                     ),



                    conditionalPanel("input.tabs=='tutorial' ",
                    sidebarMenu(
                   

                     tags$div( # a div to highlight "Display" and "Option" item
                     menuItem("Tutorial",icon = icon("chevron-circle-right"), tabName="help",
                        fluidRow(
                            column(1),
                              column(10, a("OVERVIEW",href="#general")),hr(),
                            column(1),
                              column(10,a("DATA",href="#data")),hr(),
                            column(2),                                                                   #column(1),
                              column(10,a("Upload",href="#dataupload")),hr(),                              # column(10, a("Data-Plots",href="#dataplots")),hr(),
                            column(2),                                                                   #column(1)
                              column(10,a("Plots",href="#dataplots")),hr(),                            # column(10, a("Data-Upload",href="#dataupload")),hr(),
                            column(2),                                                                   #column(1),
                              column(10, a("Pre-processing (optional)",href="#datapreprocessing")),hr(), # column(10, a("Pre-processing",href="#datapreprocessing")),hr(),
                            column(1),
                              column(10,a("ANALYSIS",href="#analysis")),hr(),
                            column(2),                                                                    #column(1),
                              column(10, a("Combinatorial Analysis",href="#analysiscombinatorial")),hr(), #column(10, a("Analysis-Combinatorial Analysis",href="#analysiscombinatorial")),hr(),
                            column(2),                                                                    #column(1),
                              column(10, a("Gold combinations",href="#analysisgold")),hr(),               #column(10, a("Analysis-Gold combinations",href="#analysisgold")),hr(),
                            column(2),                                                                    #column(1),
                              column(10, a("ROC analysis",href="#analysisroc")),hr(),                     #column(10, a("Analysis-ROC analysis",href="#analysisroc")),hr(),
                            column(1),    
                               column(10, a("DOWNLOAD",href="#download")),hr(),
                            column(1),
                              column(10,a("ACCESSORIES",href="#accessories")),hr(),
                            column(2),                                                                   #column(1),
                              column(10, a("Tutorial",href="#tutorial")),hr(),                           #column(10, a("Accessories-Tutorial",href="#tutorial")),
                            column(2),                                                                   #column(1),
                              column(10, a("Logs",href="#logs")),hr(),                                   #column(10, a("Accessories-Logs",href="#logs")),
                            column(2),                                                                   #column(1),
                              column(10, a("Contacts",href="#contacts")),hr(),               #column(10, a("Accessories-Contacts",href="#contacts")),
                            column(2),                                                                   #column(1),
                              column(10, a("Faq",href="#faq")),hr()                          #column(10, a("Accessories-FAQ",href="#faq"))
                                #  column(1),
                                # column(10, a("Analysis-Interaction map",href="#analysisinteraction")),hr(),
                          )
                      ),class="my-item"
                     ))),


              tags$head(tags$style(type="text/css", ".my-item{color:black;background-color:ghostwhite;font-style:italic;font-weight:bold;} .skin-black") ) #tags$head(tags$style(".my-item{background-color:red;} .skin-black .sidebar .my-item{color: green;}") )
                  ),   # closing DashboardSidebar
                  
                  
            

              dashboardBody(   #tags$head(tags$link(rel = "icon", type = "image/jpeg", href = "puzzle.jpeg"),
                         # tags$title("combiROC")),
                      #useShinyjs(),
                  tags$head(
                  tags$style(type="text/css","h1,h2,h3,h4,h5{font-weight:bold;padding: 25px 25px;}"), #25px 25px;
                  #tags$style(type = "text/css", "h2 { font-size: 31.5px; }")
                  #tags$style(type="text/css", ".jslider .jslider-label{font-size: 34px;}")
                  tags$style(HTML(".shiny-output-error-validation {color: mediumvioletred;font-style:italic;font-weight:bold;font-size:15px}"))    # to color error messages with validate in combi tab
                  ), 
            #       tags$head(tags$script(src = "https://code.highcharts.com/highcharts.js"),
            # tags$script(src = "https://code.highcharts.com/highcharts-more.js"),
            # tags$script(src = "https://code.highcharts.com/modules/exporting.js"),
            # tags$script(src = "https://code.highcharts.com/modules/heatmap.js")
            # ),

                         
                tabItems(
##################################
################# Main tab
##################################
                  tabItem(tabName = "main",
                          fluidRow(
                            box(
                              title = "CombiROC", status = "primary", solidHeader = TRUE, width = 10,
                               #headerPanel(list(tags$img(src="puzzle.jpeg",style="text-align: center;", height=100,width=100),"Welcome to CombiROC!"),windowTitle="Welcome to CombiROC!"),
                              br(),br(),
                              includeHTML("prova2.html"),
                              h4("CombiROC is a web app for guided and interactive generation of multimarker panels. Click", em("Data"), "in the sidepanel to get started or", em("Accessories/Tutorial"),"for full instructions.")
                            )),
                            br(),br(),br(),
                            footer
                            ),

##################################
################# Upload tab
##################################
                tabItem(tabName = "upload",
                  box(width=NULL, status="info",solidHeader=TRUE,title="Data upload",
                    # Brief Help Wording
                    box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can upload your own data or load a demo dataset. Then you can preview them in tabular form and verify their summary statistics"
                      ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px;  text-align: justify;
                      border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
                         
                       br(),
                        fluidRow(
                            #column(width = 5,
                                   box(
                                     title = "Enter data", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 3,
                                     helpText("Select a demo data or upload your data:"),
                                     selectInput("DataInput", h5("Choose your data:"),
                                                 c("Load demo data (proteomics)"=1,
                                                   #"Load demo data (transcriptomics I)"=3,
                                                   "Load demo data (transcriptomics)"=5,
                                                   "Upload file"= 2,
                                                   #"Paste data"=3,
                                                   "Select"=4
                                                   ),selected=4),
                                     conditionalPanel(condition = "input.DataInput == 2",
                                                      p('Note: your data should be structured as a ',
                                                        a(target="_blank",href = 'http://en.wikipedia.org/wiki/Comma-separated_values', 'csv file')),
                                                      fileInput('file', strong('File Input:'),
                                                                accept = c(
                                                                  'text/csv',
                                                                  'text/comma-separated-values',
                                                                  'text/tab-separated-values',
                                                                  'text/plain',
                                                                  '.csv',
                                                                  '.tsv'
                                                                )
                                                      ),
                                      conditionalPanel(condition = "input.DataInput == 1"
                                                      ),

                                                      checkboxInput('header', 'Header', TRUE),
                                                      selectInput('sep', strong('Separator:'),
                                                                  c(Comma=',',
                                                                    Semicolon=';',
                                                                    Tab='\t'),
                                                                  ';'))
                                    ),
                                     box(
                                     title = "Table", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 9,
                                     helpText("These are the data you are going to analyze, shown in tabular form"), #The dataset you want to use is displayed below:
                                     conditionalPanel(condition = "input.DataInput == 1",tags$h4(em("Proteomics demo data"))),
                                     #conditionalPanel(condition= "input.DataInput == 3",tags$h4(em("Transcriptomics demo data"))),
                                     conditionalPanel(condition= "input.DataInput == 5",tags$h4(em("Transcriptomics demo data"))),
                                     DT::dataTableOutput("AgTable")
                                      ) 
                                    ),
                                    
                        fluidRow(
                            box(
                              title = "Details of uploaded dataset", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 5, collapsed=TRUE,
                              helpText("Data information:"),
                              #uiOutput("uploadDetails")
                              htmlOutput("uploadDetails")

                            )
                          )
                                    ),

                    br(),br(),br(),
                    footer
                       ),

##################################
################# Plot tab
##################################
                # Second version Plot tab
                tabItem(tabName="plots",
                  box(width=NULL, status="info",solidHeader=TRUE,title="Data Plots",
                    # Brief Help Wording
                    box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can preview plots and tabular statistics of your uploaded data. Use the \"Display\" submenu (on the lower left)  to change plot type and \"Option\" submenu to customize the boxplot visualization"
                    ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px; text-align: justify;
                    border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
                  br(),
                ########## Box Plot  
                conditionalPanel(condition=" input.tabs=='plots' & input.display=='1'",
                  fluidRow(
                     box(title=" Box Plot",status="primary",width=12,height="850px",

#division of layout
                       #fluidRow(column(12,downloadButton("downloadBoxPlotPDF","Download PDF-file"))),   #,downloadButton("downloadBoxPlotEPS","Download EPS-file"),downloadButton("downloadBoxPlotSVG","Download SVG-file")
                        fluidRow(
                          column(6, tags$h4("Class A")),
                          column(6, tags$h4("Class B"))
                        ),
                        fluidRow(
                         column(5,
                             plotOutput("A_boxPlot",height="40%",width="30%")  #,height="800px"
                             
                             #h4("Box plot statistics"),
                             #DT::dataTableOutput("A_boxstatistics")  
                              ),
                         column(4,
                             plotOutput("B_boxPlot",height="40%",width="20%"),column(12,downloadButton("downloadBoxPlotPDF","Download PDF-file"))  #,height="800px"
                             #h4("Box plot statistics"),
                             #DT::dataTableOutput("B_boxstatistics")
                         )
                        )

                       ) #closing box
                    ),# closing fluidRow
                   #fluidRow(column(12,downloadButton("downloadBoxPlotPDF","Download PDF-file"))),   #,downloadButton("downloadBoxPlotEPS","Download EPS-file"),downloadButton("downloadBoxPlotSVG","Download SVG-file")
                   tabItem(tabName="options",
                        ### Data options
                        conditionalPanel(condition=" input.tabs=='plots' & input.display=='1' & input.data_option !=''",
                        fluidRow(
                          h5("Data options"),
                          box(width=4, status="primary",solidHeader=FALSE,
                              selectInput("whisker","Select the whisker type:", choices=WhiskerType,selectize=useSelectize)
                          ),
                          box(width=4, status="primary",solidHeader=FALSE,
                              checkboxInput("NPoints","Show the number of data points", FALSE),
                              checkboxInput("MMeans","Show the variable means", FALSE)
                         )
                         )
                         ),
                        ### Display options
                        conditionalPanel(condition=" input.tabs=='plots' & input.display=='1' & input.display_option !=''",
                        fluidRow(
                          h5("Display options"),
                          box(width=4, status="primary",solidHeader=FALSE,

                              selectInput("colour_A","Select the color class A", choices=ColorType,"palevioletred",selectize=useSelectize),

                              selectInput("colour_B","Select the color class B", choices=ColorType,"cornflowerblue",selectize=useSelectize),
                              selectInput("addgrid","Add grid:", choices=GridType,selectize=useSelectize),
                              selectInput("orientation","Select the orientation:", choices=OrientationType,selectize=useSelectize)
                          ),
                          box(width=4, status="primary",solidHeader=FALSE,
                              checkboxInput("scale","Scale variables", FALSE),
                              br(),
                              #sliderInput("plotheight","Adjust plot height:", value=250,min=0,max=500,step=1),
                              #sliderInput("plotwidth","Adjust plot width:", value=2,min=0,max=5,step=1),
                              numericInput("plotheight","Adjust plot height",value=500,step=50),
                              numericInput("plotwidth","Adjust plot width:", value=500,step=50),
                              textInput("ylimit","Adjust Y-axis range (i.e. '0,5000')",value="")
                          )
                        )
                       ),
                       ### Plot labels
                        conditionalPanel(condition=" input.tabs=='plots' & input.display=='1' & input.plot_option !=''",
                        fluidRow(
                          h5("Label options"),
                          box(width=4, status="primary",solidHeader=FALSE,
                              selectInput("labels_title","Select labels and title:",choices=LabelOptionsType,selectize=useSelectize),
                                 conditionalPanel(condition="input.labels_title == 'x axis label'",
                                     textInput("myxlab","x axis label:", value=c(""))
                                     ),
                                 conditionalPanel(condition="input.labels_title == 'y axis label'",
                                     textInput("myylab","y axis label:", value=c(""))
                                     ),
                                 conditionalPanel(condition="input.labels_title == 'box plot title_classA'",
                                     textInput("myboxtitle_A","box plot title:", value=c(""))
                                     ),
                                 conditionalPanel(condition="input.labels_title == 'box plot title_classB'",
                                     textInput("myboxtitle_B","box plot title:", value=c(""))
                                     )

                          ),
                          box(width=4, status="primary",solidHeader=FALSE,
                              checkboxInput("rotationlabels","Rotate the x ticks", FALSE),
                              sliderInput("fontsizetitle","Font size of title", value=15,min=1,max=25,step=1),
                              sliderInput("fontsizeaxis","Font size of axis", value=9,min=1,max=25,step=1),
                              sliderInput("fontsizeaxislabel","Font size of axis label", value=9,min=1,max=25,step=1)
                              )
                        )
                       ) #closing conditionalpanel plot labels
                    ),
                    fluidRow(
                         column(6,
                             h4("Box plot statistics"),
                             DT::dataTableOutput("A_boxstatistics")  
                              ),
                         column(6,
                             h4("Box plot statistics"),
                             DT::dataTableOutput("B_boxstatistics")
                         )
                        )
                    ),#closing consitional panel boxplot
                    ########## Correlation Plot
                    conditionalPanel(condition=" input.tabs=='plots' & input.display=='2'",
                        h4("Correlation plot here")
                    ), 
                    ########## Marker Plot  
                    conditionalPanel(condition=" input.tabs=='plots' & input.display=='3'",
                        box(title="Marker Profile Plot (for each individual sample mean fluorescence intensity is displayed)",status="primary",width=12,

                           fluidRow(
                          column(7,helpText("Hover over the datapoints to display sample's details"),showOutput("myChart", "highcharts")), #showOutput("myChart", "dimple")
                          column(5, DT::dataTableOutput("Oggi"))

                            ),
                           fluidRow(
                             box(width=NULL,status="primary",solidHeader=TRUE,
                                   helpText("Select the marker to plot from drop down menu:"),
                                   uiOutput('Profilevariable'),
                                   hr(), hr(),column(4),
                                   actionButton("profileButton","plot graph"))

                            )

                      ) # closing box Marker Profile Plot
                    ) # closing conditionalPanel Marker Plot

                    ),   #closing first box

                  br(),br(),br(),
                  footer
                  ),    # closing Plot tab
##################################
################# Preprocessing tab
##################################
                tabItem(tabName = "preprocessing",
                  box(width=NULL, status="info",solidHeader=TRUE,title="Data pre-processing",
                    # Brief Help Wording
                    box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can reshape the data to facilitate data analysis and model interpretation"
                    ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px; text-align: justify;
                    border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
                    br(),
                    fluidRow(
                        box(title = "Pre-processing options", status = "danger",width=4,
                                    helpText("Select a pre-treatment options in order to transform the data into a suitable form for the analysis"),br(),
                                    selectInput("datatransf","Data transformation",choices=dataTransf,selected="none",selectize=useSelectize),
                                    br(),
                                    selectInput("datascaling","Data scaling",choices=dataScaling,selected="none",selectize=useSelectize),br(),br(),
                                     htmlOutput("Error")
                                 ),# closing box

                              box(title = "Table", status = "primary", width=8,
                               helpText("These are the data you are going to analyze, shown in tabular form"),br(),  
                               DT::dataTableOutput("preproctable"),

                          hr(),
                          hr(),
                          hr()
                                    ))
                    
                    ),
                    br(),br(),br(),
                    footer
                       ),
##################################
################# Combinatorial Analysis tab
##################################

                 tabItem(tabName = "combi",
                    box(width=NULL, status="info",solidHeader=TRUE,title="Combinatorial Analysis",
                    # Brief Help Wording
                    box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can set the parameters (threshold, cutoffs etc) to evaluate the markers combinations and choose the best one with higher response."
                      ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px;  text-align: justify;
                      border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
                    br(),
                        fluidRow( #column(width = 5,
                                   # if I want to insert the possibility to choose the combinations
                                   # box(title = "Combination enumeration", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 5,
                                   #   helpText("Select the class panel:"),
                                   #   radioButtons("panelType","",list("full"=0,"reduced"=1),inline=TRUE),
                                   #   conditionalPanel(condition="input.panelType=='0'",
                                   #      textInput("Nitems","N items from which to choose:", value=c("all")),
                                   #      textInput("kitems","k items in each combination:",value=c("k=1,2,....,N"))
                                   #   ), #closing conditionalPanel
                                   #   #selectInput("TypePanel","Select the size of the panel", choices=PanelType,selectize=useSelectize),
                                   #   #htmlOutput("nCombinations"), br(),
                                   #   checkboxInput("repetition","Allow repeats?", FALSE),
                                   #   checkboxInput("order","Order matters?", FALSE)),
                                    box(title = "Graphics", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                     helpText("Insert here the thresholds on your uploaded data"),
                                     ######Proteomics demo data
                                      conditionalPanel("input.DataInput== 1",
                                          fluidRow(column(5, numericInput("signalthr", "Set the test signal cutoff (data above this value are considered positive):", 450,min=1),
                                             numericInput("combithr", "Minimum features (the minimum number of positive markers)", 1,min=1))),
                                          fluidRow(
                                            column(1),column(8,actionButton("runButton","distributions")),br(),br(),br(),column(8,p("Click the button to update the value displayed in the plot panel.")), br(),
                                            #column(12, plotOutput('viewDistribution',height="300px",width = "100%")),
                                            column(12,showOutput("barDistribution","highcharts"))
                                           #verbatimTextOutput("provathr")
                                            )
                                        ),
                                       ######Transcriptomics demo data
                                      conditionalPanel("input.DataInput== 5",
                                          fluidRow(column(5, numericInput("signalthr_3", "Set the test signal cutoff (data above this value are considered positive):", 8,min=1),
                                             numericInput("combithr_3", "Minimum features (the minimum number of positive markers)", 1,min=1))),
                                          fluidRow(
                                            column(1),column(8,actionButton("runButton_3","distributions")),br(),br(),br(),column(8,p("Click the button to update the value displayed in the plot panel.")), br(),
                                            #column(12, plotOutput('viewDistribution',height="300px",width = "100%")),
                                            column(12,showOutput("barDistribution_3","highcharts"))
                                           #verbatimTextOutput("provathr")

                                            )
                                        ),
                                       conditionalPanel("input.DataInput !=1 & input.DataInput != 5",
                                          fluidRow(column(5, numericInput("signalthr_2", "Set the test signal cutoff (data above this value are considered positive):", NA),
                                             numericInput("combithr_2", "Minimum features (the minimum number of positive markers)", NA,min=1))),
                                          fluidRow(
                                            column(1),column(8,actionButton("runButton_2","distributions")),br(),br(),br(),column(8,p("Click the button to update the value displayed in the plot panel.")), br(),
                                            #column(12, plotOutput('viewDistribution_2',height="300px",width = "100%")),
                                            column(12,showOutput("barDistribution_2","highcharts"))
                                           # textOutput("provathr_2")
                                            )
                                        )
                                     # fluidRow(column(5,
                                     #    #conditionalPanel to set the threshold values according to the type of data frame uploaded
                                     #    conditionalPanel(condition = "input.DataInput == '1'",  
                                     #    numericInput("signalthr", "Set the threshold (data above this value are considered positive):", 450),
                                     #     numericInput("combithr", "Minimum features (the minimum number of markers shown in the plot)", 1)),
                                     #    conditionalPanel(condition = "input.DataInput != '1'",  
                                     #    numericInput("signalthr", "Set the threshold (data above this value are considered positive):",NA),
                                     #     numericInput("combithr", "Minimum features (the minimum number of markers shown in the plot)",NA)
                                     #       )  )
                                     #  #column(11
                                     # #plotOutput("distributionPlot",height="500px",width = "100%")
                                     # #)),
                                     # fluidRow(column(1),
                                     #   column(8,actionButton("runButton","distributions")),hr(),
                                     #   column(8,p("Click the button to update the value displayed in the plot panel.")), br(),
                                     #   column(12,
                                     #   plotOutput('viewDistribution',height="300px",width = "100%")))
                                    ) #closing box Graphics
                                   ), #closing fluidRow
                          br(),br(),
                         fluidRow(
                            box(
                              title = " Mathematical details", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 5,
                              helpText("Definition and Formula"),
                              # conditionalPanel(condition="input.panelType=='0'",
                               withMathJax(),
                               includeMarkdown("Mathdetails.Rmd")
                            ),
                            box(
                              title = " Combo list", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 7, height="auto",
                              helpText("All possible combinations"),
                              conditionalPanel("input.DataInput== 1", DT::dataTableOutput("SE_SPtable")),
                              conditionalPanel("input.DataInput== 5", DT::dataTableOutput("SE_SPtable_3")),
                               conditionalPanel("input.DataInput!=1",DT::dataTableOutput("SE_SPtable_2")),br(),br() #conditionalPanel(condition="input.panelType=='0'"
                            )
                          ), #closing lower fluidRow
                     br(),
#                      conditionalPanel(condition="input.DataInput==1",
#                        conditionalPanel(condition="input.runButton==0",column(8), div(HTML('<div class="row">
#                                       <div class="col-xs-6">
#                                          <div class="spin-test">
#                                            Click the distributions button <span class="fa fa-3x fa-spinner fa-spin" style="color:red"></span>
#       </div>
#     </div>
# </div>')))),
                     # Next widget : Go to gold combinations
                     conditionalPanel( condition="input.runButton!=0 | input.runButton_3!=0 | input.runButton_2!=0",fluidRow(column(8),valueBoxOutput("kpi_summary_box_1",width=3))),
                     # Warning widget : Go to gold combinations
                     conditionalPanel( condition="input.runButton==0 & input.runButton_3==0 & input.runButton_2==0",fluidRow(column(8),
                           tags$head(tags$style(HTML('#do{color:red;background-color:orange} '))), #width: 300px; 
                          actionButton("do","", style='padding-left:0px; text-align:right;',icon=HTML('<div class="row"><div class="col-sm-4"><p><font size="4">Warning</font></p>click the distributions button</div><div class="col-sm-4"></div><div class="col-sm-4"><div class="spin-test"><span class="fa fa-3x fa-spinner fa-spin" style="color:red" alt="top right"></span></div></div></div>'),width="auto") # icon("spinner")
                     ))
                     ), # closing Combinatorial Analysis box

                     #conditionalPanel(condition="input.runButton!=0 & input.signalthr=='' | input.combithr=='' ", valueBoxOutput("warning_next1")), #doesn't work
                     #alternative aesthetic for intercative
                       # conditionalPanel( condition="input.runButton!=0",fluidRow(column(8),
                       #     tags$head(tags$style(HTML('#do1{color:white;background-color:green} '))), #width: 300px; 
                       #    actionButton("do1","", style='padding-left:0px; text-align:right;',icon=HTML('<div class="row"><div class="col-sm-4"><p><h4>Next</h4></p>Gold combinations</div><div class="col-sm-4"></div><div class="col-sm-4"><div class="spin-test"><span class="fa fa-4x fa-step-forward"></span></div></div></div>'),width="auto") # icon("spinner")
                       #  )),    
                    br(),br(),br(),
                    footer
                       ),    # closing tab combi

##################################
################# Gold combinations tab
##################################
                 tabItem(tabName = "gold",
                    box(width=NULL, status="info",solidHeader=TRUE,title="Gold Combinations",
                    # Brief Help Wording
                    box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can explore the specificity and sensitivity space, in order to choose the optimal cutoffs "
                      ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px;  text-align: justify;
                      border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
                    br(),
                        fluidRow(
                                 box(title = "Explore SE and SP values", status = "danger",width=4,
                                    "Select the sensitivity and/or specificity filters:",br(),br(),br(),
                                   sliderInput("sinePhase", "Sensitivity", 0, 100, 40, step=5,animate=animationOptions(interval=1000, loop=TRUE)),
                                   sliderInput("sineAmplitude", "Specificity", 0, 100, 80, step=5,animate=animationOptions(interval=1000, loop=TRUE))
                                 ),# closing box
                                 box(title = "Gold combination bubble plot", status = "primary", width=8,height = "800px",
                                "Visualize the SE and SP space",br(),br(),showOutput("highbubble","highcharts"),  #,   plotOutput("plotbubble", height = 250)
                                 #xlim <- list(min= input$se,max = 100),
                                #googleBubbleChart("bar",  width="20%",height = "20%",hAxis= list(minValue= input$se))
                                 #htmlOutput("bar")
                                 #sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 10),
                                #plotOutput("plot4", height = 250)
                                 #sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 10),
                               #includeHTML("oggi.html"), #if I want include an HTML file
                                #lineChartOutput("mychart"), questa riga prima quando inglobavo esempio seno e mie dait con linechart
                                      #scatterChartOutput("mychart"),
                                #includeHTML("index.html"),
                                #includeHTML("prova.html"), #is valid, #graphOutput('trendPlot'),
                          hr(),hr(),hr())
                      ), #closing fluidRow

                        fluidRow( 
                                box(title="Gold table",width=12,status="primary",                                 #solidHeader=TRUE if I want a border of the box
                                    helpText("Confirm the selected thresholds:"),
                                   fluidRow(
                                      column(4,
                                         uiOutput("numericSe"),uiOutput("numericSp"),
                                        hr(), hr(),column(4),
                                         actionButton("submitButton1","Submit"),hr(),hr(),hr(),
                                         p("Click the button to confirm thresholds and visualize the corresponding gold combinations.")),
                                      column(8, DT::dataTableOutput("Goldtable1"))
                                   ) #closing inner fluidRow
                                ) #closing box "Gold table"
                      ), #closing fluidRow
                     fluidRow(column(6),valueBoxOutput("kpi_summary_box_2",width=3),conditionalPanel(condition="input.submitButton1==0",valueBoxOutput("kpi_summary_box_5",width=3)),
                      conditionalPanel(condition="input.submitButton1!=0 ",valueBoxOutput("kpi_summary_box_3",width=3))
                      ),
                    br(),br(),br()
                  
                      # HTML("<div id='linkToSelect'><button type='button' class='btn btn-block btn-primary'><span class='glyphicon glyphicon-repeat'></span> Back to Selection</button></div>"), 

            #             tags$script("$('#linkToSelect').click(function() {
            # tabs = $('.tabbable .nav.nav-tabs li a');
            # $(tabItems[0]).click();
            # })")
                        ), 
                       footer# closing Gold combinations tab
                     ),

##################################
################# ROC curves tab
##################################
#                     #first version
#                      tabItem(tabName = "roc",
#                      box(width=NULL, status="info",solidHeader=TRUE,title="ROC analysis",
#                     # Brief Help Wording
#                     box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can......."
#                       ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px;  text-align: justify;
#                       border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
#                        br(),
#                        fluidRow(
# #                            #column(width = 5,
#                                     box(title = "ROC curves", status = "primary", width = 12,
#                                      helpText("Insert the opportune thresholds:"),
#                                     fluidRow(
#                                      column(3,
#                                         numericInput("spethr", "Specificity (%):", 50),
#                                         numericInput("sethr", "Sensitivity (%):", 50),
#                                         hr(), hr(),column(4),
#                                         actionButton("submitButton","Submit"),hr(),hr(),hr(),
#                                          p("Click the button to visualize the gold combinations.")
#                                         ),
#                                      column(8,   DT::dataTableOutput("Goldtable"))  
#                                            ),
#                                     br(),
#                                     fluidRow(
#                                        column(3),
#                                        column(8, h4("Specify Variables"), checkboxInput("multi", "Check to plot multiple curves"),  conditionalPanel("input.multi == false",
#                                                    p("Select the combination/s from drop down menu."),

#                                                   #selectInput("M", "Marker", choices =c(input$Goldtable_rows_all), selected = colnames(example)[1]),
#                                                   #textInput("M", "Marker", value=""),
#                                                   uiOutput('Mvariable'),
#                                                   actionButton("rocButton","view ROC"),
#                                                   br(),
#                                                   br(),
#                                                   tabBox(title = "ROC analysis",# The id lets us use input$tabset1 on the server to find the current tab
#                                                             id = "tabset1", height = "3000px",width="400px",tabPanel("ROC curve","Here ROC curve",plotOutput("ROCcurve")),
#                                                             tabPanel("Predictions", "Here predictions",plotOutput("ClassPred")),
#                                                             tabPanel("Performance Analysis",br(),br(),
#                                                                     fluidRow(box(title="Comparison plot",width=6,solidHeader=TRUE,"plot the ROC curves here",plotOutput("ComparisonPlot",height="400px",width = "100%"),br(),br(),height=600),
#                                                                              box(title="Only CV plot",width=6,solidHeader=TRUE,"if I consider k times 10 CV then plot the 10-fold CV ROC curves",br(),br())
#                                                                              ),
#                                                                     br(),br(),br(),
#                                                                     DT::dataTableOutput("Perftable")) # if you want to print the result ,verbatimTextOutput("prova1")
#                                                                     )
#                                                   #plotOutput("ROCcurve")  #,height="400px",width = "400px"
#                                                   #textInput("text", "Enter Formula", "a=b+c"), ,plotOutput("ClassPred")
#                                                   #plotOutput("ROCcurve")
#                                                   #numericInput("alpha", "Confidence level", .05, min = .01, max = .99, step = .01),
#                                                   #textInput("ci.at", "Cutoffs for CIs (separate multiple by commas)", value = "")
#                                                ),
#                                                conditionalPanel("input.multi == true",
#                                                   selectInput("Ms", "Marker", choices = colnames(example), multiple = TRUE, selected = colnames(example)[1])
#                                                                 )
#                                                ),
#                                        br())
#                                      )
#                           ),br(),br(),br(),footer)),   # closing ROC tab
                tabItem(tabName = "roc",
                    box(width=NULL, status="info",solidHeader=TRUE,title="ROC analysis",
                    # Brief Help Wording
                    box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can perform ROC analysis according to previously set parameters."
                      ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px;  text-align: justify;
                      border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
                    br(),
                        fluidRow(
#                            #column(width = 5,
                                box(title = "Select Combos", status = "primary", width = 4,
                                    checkboxInput("multi", "Check to plot multiple curves"),
                                    conditionalPanel("input.multi == false & input.submitButton1!='0'",  p("Select the combination/s from drop down menu."), br(),uiOutput('Mvariable'), br(),column(3)), #,actionButton("rocButton","view")
                                    conditionalPanel("input.multi == false & input.submitButton1=='0'", htmlOutput('Mgoldtable')),
                                    conditionalPanel("input.multi == true & input.submitButton1!='0'",p("Select the combination/s from drop down menu."), br(),uiOutput('Mvariable2'))
                                ), #closing box "Specify variables"
                                box(title = "Results", status = "primary", width = 8,
                                                             h4(em("ROC curve")),
                                                             conditionalPanel("input.multi ==false & input.submitButton1!='0'",
                                                                fluidRow(div(class="span4",showOutput("ROCcurve2","highcharts"))),
                                                                br(),br(),
                                                                DT::dataTableOutput("ROCtable1")
                                                             ),
                                                             conditionalPanel("input.multi ==true & input.submitButton1!='0'",
                                                               showOutput("multipleROC","highcharts"),br(),br(),br(),br(),
                                                               DT::dataTableOutput("ROCmultipletable")
                                                              ),br(),br(),
                                                             conditionalPanel("input.multi ==false & input.submitButton1!='0'",
                                                              h4(em("Predictions")),plotOutput("ClassPred1"),br(),br(),br(),showOutput("pie","highcharts")),br(),br(),#showOutput("heatmap","highcharts"),
                                                             br(),br(),
                                                             conditionalPanel("input.multi ==false & input.submitButton1!='0'",
                                                              #div(class="span4", tags$style(".highcharts{ height: 100%; width: 800px;}"),showOutput("Chart2", "highcharts")),                       
                                                                   h4(em("Performance Analysis")),
                                                                   fluidRow(div(class="span4",showOutput("Chart2","highcharts"))),
                                                                    br(),br(),br(),
                                                                   DT::dataTableOutput("Perftable"),br(),br()
                                                                   )
                                ) #closing box "Results"
                                      #                      tabBox(title = "",# The id lets us use input$tabset1 on the server to find the current tab
                                      #                       id = "tabset1",side="right", height = "600px",width="400px",tabPanel("ROC curve","Here ROC curve",plotOutput("ROCcurve")),
                                      #                       tabPanel("Predictions", "Here predictions",plotOutput("ClassPred"),br(),br()),
                                      #                       tabPanel("Performance Analysis",br(),br(),
                                      #                               fluidRow(box(title="Comparison plot",width=6,solidHeader=TRUE,"plot the ROC curves here",plotOutput("ComparisonPlot",height="400px",width = "100%"),br(),br(),height=600),
                                      #                                        box(title="Only CV plot",width=6,solidHeader=TRUE,"if I consider k times 10 CV then plot the 10-fold CV ROC curves",br(),br())),
                                      #                               br(),br(),br()
                                      #                               #DT::dataTableOutput("Perftable")) # if you want to print the result ,verbatimTextOutput("prova1")))
                             ), #closing fluidRow
                        fluidRow(column(6),conditionalPanel(condition="input.submitButton1!=0",valueBoxOutput("kpi_summary_box_4",width=3))),    
           
                          br(),br(),br()),
                          footer),   # closing ROC tab

##################################
################# Network tab
##################################
     #                tabItem(tabName = "network",

     #                 box(width=NULL, status="info",solidHeader=TRUE,title="Network Graph",
     #                 # Brief Help Wording
     #                box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can........."
     #                  ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px;  text-align: justify;
     #                  border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
     #                   br(),
     #                      fluidRow(
     #                            box(title = "Network Input", status = "primary",width=4,
     #                                "Insert your marker of interest:",br(),br(),br(),
     #                                textInput("MarkerName","Marker name:", value=c("")),
     #                                column(4),
     #                                actionButton("NetworkButton","make network"),br(),br(),
     #                                     p("Click the button the interaction network.")
     #                             ),
     #                               box(title = "Network Plot", status = "primary", width=8,
     #                            "Insert the network plot here" ,htmlOutput("networkPlot"),  sliderInput("nb", "number of nodes : ", min = 2, max = 1000, value = 10),
     #                                actionButton("goButton", "Go!"),
     # checkboxInput("legend2", "legend", value = TRUE),
     # h4("input$network_selected"),verbatimTextOutput("test"),
     # h4("output$network"),
     # visNetworkOutput("network",height = "600px"), hr(),verbatimTextOutput("datatest")
  


     #                             )
     #                      ),
     #                      fluidRow(
     #                            box(title = "Network Summary", status = "primary",width=4,
                                    
     #                                htmlOutput("uploadNetworkDetails")
                                    
     #                             )

     #                      )

     #                      ),br(),br(),br(),footer),


##################################
################# Download tab
##################################
                tabItem(tabName = "download",
                  box(width=NULL, status="info",solidHeader=TRUE,title="Download",
                    # Brief Help Wording
                    box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can download the tabular file of the \"demo data\" and a printable pdf file of the tutorial."
                      ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px;  text-align: justify;
                      border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
                         
                       br(),
                        fluidRow(
                          column(1),
                          downloadButton("downloadDemoData","Download proteomics demo data as .CSV file"),br(),br(),br(),
                         
                          column(1),
                          p(a("Click here",target="_blank",href="CombiROC_tutorial_FINAL.pdf"), " to downlaod the tutorial as single PDF file.",align="justify")
                   
                                
                                    )
                                    
                
                                    ),

                    br(),br(),br(),
                    footer
                       ),

##################################
################# Tutorial tab
##################################
                tabItem(tabName = "tutorial",
                  box(width=NULL, status="info",solidHeader=TRUE,title="Tutorial",
                       h3("Overview of the CombiROC wokflow.",id="general"),
                       #h4("CombiROC workflow"),
                       p("CombiROC delivers a simple workflow to help researchers in selecting the optimal combination(s) of markers through a simple analytical method based on the introduction of a double filter scoring.",strong("Figure 1"),", left, illustrates the general workflow of the application: after uploading multi markers profiling data in text format, users are offered a choice of simple data viewing with plotting and optional data processing methods. Users can define the stringency of their test (i.e. the signal cutoff and minimum number of positive features). Then, thresholds on sensitivity (SE) and specificity (SP) can be freely explored and interactively adjusted graphically observing how many markers’ combinations survive the cutoffs; finally, the best combinations can be chosen and their ROC curves automatically generated. Users can review results and download combinatorial analyses results and ROC curves.",align="justify"),
                       p("CombiROC’s analytical approach is based on sensitivity and specificity filters, interpreted in terms of recognition frequency, optimizing the number of potentially interesting panels rising from a previous combinatorial analysis step.  CombiROC does not take for granted a default algorithm-driven marker threshold, but it allows users to interactively choose the thresholds according to their requirements: in doing so it dramatically reduces the computational burden for the subsequent analytical steps. CombiROC also makes the analysis of biomarkers panels of diverse nature easier, lowering false negative rate given by fixed thresholds.",align="justify"),
                       p("Paragraphs of this tutorial are ordered following the structure of the application's main menu (",strong("Figure 1"),", right).",align="justify"),
                       p("This tutorial can be downloaded as single PDF file",  a("here",target="_blank",href="CombiROC_tutorial_FINAL.pdf"), ".",align="justify"),
                       br(),br(),
                       tags$img(src="tutorial_fig1.png",style="text-align: center;", height=450,width=800),

                     
                  div(class = "my-class", "DATA",id="data",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #070B11;padding: 5px;  text-align: justify;border-style:none;background-color: #A3BDDC;visibility:visible;"),
                      br(),
                  div(class = "my-class", "UPLOAD",id="dataupload",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"),
                  #h3("Data",id="dataupload"),
                  #tags$h4("Upload",style="color:blue"),
                       br(),
                       p("Data can be uploaded to the CombiROC application. Before being uploaded the data in the application, data need to be correctly formatted as text files (csv, tab or semicolon separated). Make sure you are using the English locale for decimal separators (use the dot \".\" to separate decimals in numerical fields).",align="justigy"), 
                       p("You can prepare your data in your favorite spreadsheet software or application as long as it contains (see",strong("Figure 2"),"):"),
                       
                       HTML("<ul><li>in the first column, the samples IDs (i.e. patients),</li>
                        <li>in the second column, the class category (i.e. disease and healthy; marked \"A\" and \"B\")</li>
                        <li>from the third column onward, the data values (i.e. detection levels) </li></ul>"),
                       p("The first row can contain a header. An example is given in",strong("Figure 2")," and a preformatted demo file can be also downloaded",a(" here",target="_blank",href="demodata.csv"),".",align="justify"), # a("here",href="http://combiroc.ingm.ad:3838/sample-apps/combiROCdashbv6bis/")
                         HTML("<ul><li>the header of the first column must be \"Sample_ID\"</li>
                        <li>the header of the second column must be \"Class\"</li>
                        <li>the header of columns from the third onward must be \"Marker#\", where \"#\" is a progressive integer (i.e. Marker1, Marker2, Marker3,....).</li></ul>"),

                       p("In the second column, dedicated to the classes/categories of samples (i.e. healthy and diseases; treated and untreated) an arbitrary number of classes are accepted in the file but", em("the application will consider for the analyses only the first 2 classes for pairwise comparisons"),". Thus, for clarity we recommend to limit in the uploaded file the number of classes to 2, tagged with \"A\" and \"B\".",align="justify"),
                       p("Once your data are correctly formatted you can upload them using the \"Data / Upload\" link in the main menu on the left: in the \"Enter data\" widget select the \"Upload file\" option then select the file from your workstation. In case it’s not automatically recognized,  you can indicate the presence or absence of the header and specify the separator used (comma, semicolon or tab). From the very same menu you are also offered the possibility to use the pre-loaded demo data choosing the \"Load demo data\" options.",align="justify"),
                       br(),
                       tags$img(src="tutorial_fig2.png",style="text-align: center;", height=400,width=600),

                       p("Immediately after loading the data, they will be visualized in a tabular form in the \"Table\" widget on the right; if the data are correctly formatted you will be able to see the header and data as they appear in the original file. Only the first ten entries (rows) are displayed by default but you can adjust this number with the upper left selection in the Table widget. ",align="justify"),
                       p("The displayed rows can be copied, downloaded as csv, or as pdf clicking on the \"Copy\",\"CSV\" or \"PDF\" buttons respectively, on the upper right corner of the widget. Please note that only entries displayed on screen will be downloaded or printed, so if you want the entire file to be downloaded make sure to select all entries with the \"Show ALL entries\" toggle selection on the left.",align="justify"),
                       p("The widget \"Details of uploaded dataset\" will summarize some details of your data: the number of samples, markers and categories, the data value’s type and the presence of missing values. If errors are detected a warning will be displayed in this widget.",align="justify"),
                       br(),
                      
                  div(class = "my-class", "PLOTS",id="dataplots",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"),
                  #h3("Data-Plots",id="dataplots"),
                        br(),
                        p("The Plots page of CombiROC automatically displays two types of plots and statistics overview of the uploaded data. Upon clicking to the Data / Plots menu, ancillary options become active on the lower bottom of the main menu: they are \"Display\" , used to choose from two different type of plots (box plot and marker profile plot), and \"Options\" (available for the box plot only) used to change data, display and label options of the box plot. ",align="justify"), 
                        p("Among the options that can be changed for the box plot type are the whisker type, the color of boxes, the orientation, height and width of the plot. The Y-axis ranges of the two classes’ plots are scaled individually by default, according to minimum and maximum values in the dataset. If you need to have box plot panels in the same range you can adjust this range in the \"Adjust Y-axis range\" field typing it in the format \"0,1000\" (lower value - comma - upper value). Beware that adjusting Y-axis range the value extremes could be not visualized. Finally, labels can be edited with custom text and font size.",align="justify"),
                        p("The box plot is used to visualize the distribution of single markers values across the samples (i.e. patients). Upon loading the demo data two box plots are obtained colored in red-pink for Class A and blue for Class B; markers are visualized in each class and their distributions observed.",align="justify"),
                        p("In the box plot page is also visible the \"Box plot statistics\" tables for both classes: they include distribution parameters which allow the user to choose the best cutoff values for the subsequent step (combinatorial analysis).",align="justify"),
                        p("The marker plot displays the signal intensity of each single sample for each marker. Select the marker you want to display from the drop down menu on the left, click on \"plot graph\" and the profile plot will be displayed. You then can hover over data points to reveal single samples details.",align="justify"),
                        br(),

                  div(class = "my-class", "PRE-PROCESSING (OPTIONAL)",id="datapreprocessing",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"),
                  #h3("Data-Pre-processing",id="datapreprocessing"),
                        br(),
                        p("In this page data can be processed with a few transformations if they need to be reshaped. From the drop down menu you can choose among a data transformation (log2 transformation), and two methods of scaling (unit variance scaling; pareto scaling).",align="justify"),
                        p("The \"unit variance scaling\", also known as autoscaling is commonly applied and uses the standard deviation as the scaling factor; in the \"pareto scaling\" the square root of the standard deviation is used as the scaling factor instead.",align="justify"),
                        p("On the right side of the page the transformed and/or scaled data are displayed in tabular form. As for the other tables visualized in the application, only the first ten entries (rows) are displayed by default but you can adjust this number with the upper left selection in the Table widget.",align="justify"),
                        p(strong("PLEASE NOTE:"),em("CombiROC is neither a transformation nor a data visualization tool: the steps \"Plots\" and \"Preprocessing\" are not strictly necessary for the completion of the analysis. The \"Plot\" function and the optional transformation tools are meant to allow users to look their data's structure, but data themselves should be correctly formatted", strong("before"), "being uploaded to CombiROC."),align="justify"),
                        br(),br(),br(),

                  div(class = "my-class", "ANALYSIS",id="analysis",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #070B11;padding: 5px;  text-align: justify;border-style:none;background-color: #A3BDDC;visibility:visible;"),
                        br(),
                  div(class = "my-class", "COMBINATORIAL ANALYSIS",id="analysiscombinatorial",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"),
                     #h3("Analysis-Combinatorial",id="analysiscombinatorial"),
                        br(),
                        p("The Combinatorial Analysis, also called combinatorics, is a branch of mathematics concerned with the theory of enumeration, or combinations and permutations, in order to solve problems about the possibility of constructing arrangements of objects which satisfy specified conditions.",align="justify"),
                         p("In the page \"Analysis / Combinatorial Analysis\" you will find tools to obtain all possible markers combinations and choose the best one, i.e. the one with the higher response. Three main widgets \"",strong("Graphics"),"\", \"",strong("Mathematical details"),"\" and \"",strong("Combo List"),"\" are describe below.",align="justify"),
                        #h4("Graphics"),
                        br(),
                  div(class = "my-class", "GRAPHICS",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:dotted none none dotted;border-width: 1px;"),
                        br(),
                        p("In the \"Graphics\" widget you can set, according to the specific nature of your experiment (the test), the cutoff above which the features' values are considered positive (the \"",em("test's signal cutoff"),"\"); you can also insert the minimum number of positive features that need to reach the previously set cutoff.",align="justify"),
                        p("As an example, upon loading the \"Demo data (proteomics)\" provided by the application you will find the pre-set cutoff value of \"450\" (e.g. Fluorescence Intensities, representing the mean value of buffer control class plus three times the standard deviation). The minimum number of positive features is, for the demo data, pre-set to \"1\", i.e. with the minimum stringency, which means that at least 1 marker must reach the value of 450.",align="justify"),
                        p("Once you have set the proper thresholds click on the \"",em("Distribution"), "\" button to visualize in an histogram graph the distributions of",em("Sensitivity"), "and", em("Specificity"), "of the combinations satisfying the set cutoffs (",strong("Figure 3"),":",em("Sensitivity")," (SE, blue bars) is defined as the true positive rate in percentage of your sample.",em("Specificity"),"(SP, black bars) is defined as the true negative rate in control class in percentage. In x-axis is shown the number of each positive feature as frequency (left wise blue bars for SE intervals, right wise black bars for SP intervals) while in y-axis the SE and SP distributions intervals in percent. You can hover over bars to see values.",align="justify"),
                        #list(tags$img(src="SESPbarchart.png",style="text-align: center;", height=480,width=800),""),
                        list(tags$img(src="tutorial_fig3.png",style="text-align: center;", height=480,width=800),""),
                        br(),
                        p("This histogram plot helps the user to evaluate the intervals from which the best SP/SE values will be chosen, and on which markers’ \"",em("Gold Combinations"), "\" will be calculated in further steps of the analysis. In the specific \"Demo data (proteomics)\" example, to which the plot in",strong("Figure 3"), "refers, the graph shows that most markers’ combinations have sensitivity higher than 40%, with a peak of 12 combinations in the 81-90% sensitivity  range; for the specificity distributions all combinations have SP higher than 50% and a substantial number of them are above 80%. Any evaluation and choice at this point is strictly dependent on the specific nature of the experiment that generated the data and on the aim of the user: using the demo data as an example the preloaded values of sensitivity >40% and specificity >80% can be found, since they may serve the purpose of a usable tradeoff with the data at hand. ",align="justify"),
                        p("Once these \"hard\" thresholds are set, further browsing and evaluation of sensitivity and specificity values can be done in the subsequent \"Gold combinations\" section. ",align="justify"),
                        br(),
                        #h4("Mathematical details"),
                  div(class = "my-class", "MATHEMATICAL DETAILS",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:dotted none none dotted;border-width: 1px;"),
                        br(),
                        p("In this widget is shown the formula used for the combinatorial analysis:",align="justify"),
                         tags$img(src="tutorial_fig_formula.png",style="text-align: center;", height=100,width=300),
                        p("N= the total number of items; k= the desiderate number of components in the combination. For more theoretical details see:",a(" Introductory Combinatorics (5th Edition), Brualdi RA.",target="_blank",href="http://www.pearsonhighered.com/educator/product/Introductory-Combinatorics/9780136020400.page"),align="justify"),
                         br(), 
                        
                  div(class = "my-class", "COMBO LIST",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:dotted none none dotted;border-width: 1px;"),     
                   #h4("Combo List"),       
                         br(),
                         p("The table in the \"Combo List\" widget shows a numerical overview of the sensitivity and specificity of combination of markers thereof according to the thresholds set in the \"Graphic\" widget. In the \"Demo data (proteomics)\" example the table shows the sensitivity and specificity values of 31 features, a list that includes each marker and all possible combinations generated using the pre-set threshold (at least 1 feature with ≥ 450 detection, i.e. at least 1 feature with \"positive\" value).",align="justify"),
                         br(),
                      
                  div(class = "my-class", "GOLD COMBINATIONS",id="analysisgold",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"), 
                        #h3("Analysis-Gold Combinations",id="analysisgold"),
                        br(),
                        p("Once the hard thresholds (", em("i.e.")," cutoffs on detection value and minimal number of markers) have been set, the array of obtained markers’ combinations (\"Combos\") can be more deeply evaluated in order to select only the few (the Gold ones) that satisfy a minimal SP and SE. ",align="justify"),
                        br(),
                   div(class = "my-class", "EXPLORE SE AND SP VALUES / GOLD COMBINATION BUBBLE PLOT",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:dotted none none dotted;border-width: 1px;"),      
                  #h4("Explore SE and SP values / Gold Combination bubble plot"),
                        br(),
                        p("In this section two sliders are available to explore the SE and SP ranges. On the Bubble chart on the right of the page sensitivity (Y axis) and specificity (X axis) of all the marker combinations are automatically plotted; the size of the bubbles is proportional to the number of markers in the combo, the bigger the bubble, the more the markers. Combinations that do not bypass the SE & SP thresholds set with the sliders on the left are depicted as blue bubbles (the \"under the thresholds\" combos), otherwise the bubbles are yellow (the \"Gold\" combos). To start off, you can move the sensitivity and specificity sliders and observe how many bubbles (=markers combos) remain yellow at higher SE and SP values.",align="justify"),
                        br(),
                  div(class = "my-class", "GOLD TABLE",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:dotted none none dotted;border-width: 1px;"),      
                  #h4("Gold table"),
                        p("Once you reach a reasonable tradeoff between SE, SP and number of combos to go further in the analysis you need to confirm SE and SP values in the \"Gold Table\" section, in the lower half of the page (you’ll notice that SE and SP values set with the sliders are automatically reported in the \"Gold Table\" section below, for the \"Demo data (proteomics)\" values of 40% Sensitivity and 80% Specificity are suggestsed and preloaded). Click on \"Submit\" and a table detailing - and naming - the selected combinations will be displayed in the lower right of the page",strong("Figure 4"),".
                          This table lists each single marker and/or combination of markers that have been selected from the values of Specificity and Sensitivity used. Using the \"Demo data (proteomics)\", and the preloaded SP and SE values (see also discussion in the previous section \"Combinatorial Analysis\"), 14 combinations out of the 31 in the original input list are selected and consequently displayed in the gold table: this table displays the percentages of Specificity and Sensitivity corresponding to each marker or combination. As other tables in the application, this one can be copied and downloaded as a csv or pdf file.",align="justify"),
                        br(),
                        tags$img(src="tutorial_fig4.png",style="text-align: center;", height=400,width=400),
                        br(),br(),
                  
                  div(class = "my-class", "ROC ANALYSIS",id="analysisroc",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"), 
                  #h3("Analysis-ROC curves",id="analysisroc"),
                        br(),
                        p("After having set all the thresholds on your dataset and obtained a number of \"Gold combinations\" of markers, then you finally need to see how these combinations perform. Receiver operating characteristic (ROC) curves are used in medicine to determine a cutoff value for a clinical test. When creating a diagnostic test, a ROC curve helps us visualize and understand the tradeoff between high Sensitivity and high Specificity when discriminating between clinically \"normal\" and clinically \"abnormal\" laboratory values.",align="justify"),
                        br(),
                  div(class = "my-class", "SELECT COMBOS / RESULTS",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:dotted none none dotted;border-width: 1px;"),      
                  #h4("Select Combos/Results"),
                        br(),
                        p("First, you have to select from the drop down menu the single combination to visualize the corresponding ROC curves. If you want to directly compare multiple markers or combinations in the same plot thick the \"Check to plot multiple curves\" mark (see further). The names of markers and combinations in the drop down menu (\"Marker#\" if single markers or \"Combo\" followed by roman numeral) are the same as those reported in the \"Gold table\" in the previous page of the workflow.",br(),"Once you select a marker/combo from the dropdown, the ROC curve, Predictions and Performance Analysis are automatically visualized.",align="justify"),
                        br(),
                  div(class = "my-class", "ROC CURVES",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:none none solid none;border-width: 1px;"),      
                  #h4("ROC curves"),
                          p("The ROC curve (",strong("Figure 5"),") is a graph of \"sensitivity\" (y-axis) vs \"1 – specificity\" (x-axis). Large \"y\" values on the ROC curve plot correspond to higher sensitivity, while small \"x\" values correspond to higher specificity. The shape of the curve depicts the combinatorial variation of these two important parameters. Below the ROC curve you will be able to see the AUC (Area Under the Curve), SE, SP and optimal cutoff numerical values of the analyzed combo in tabular form. ",
                          br(), "The results are reported in fractions from 0 to 1. A diagonal line of identity is reported such as the point of optimal cut-off. However, you can choose whether to display it or not, just by clicking on it.", align="justify"),
                          tags$img(src="tutorial_fig5.png",style="text-align: center;", height=600,width=500),
                          br(),br(),
                          p(strong("Figure 5"), " shows the ROC curve of \"Combo II\" obtained using the \"Demo data (proteomics)\". A table for each single marker or combo that have been selected from the marker dialog, will also appear below the plot, showing the values of Area Under Curve, Sensitivity, Specificity (in percentage) and Optimal cut-off. As other tables/figures in the application, this one can be copied and downloaded as a csv or pdf file.",align="justify"),
                          br(),
                  div(class = "my-class", "PREDICTIONS",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:none none solid none;border-width: 1px;"),          
                  #h4("Predictions"),
                          br(),
                          p("The \"Predictions\" section of the page displays a violin plot and a pie chart.",br(),"The violin plot is a combination of a box plot and a kernel density plot, showing the \"probability density\" of the data at different values. Prediction probabilities are plotted for both classes (class A and B, disease/healthy, treated/untreated) according to the previously obtained optimal cutoff. The four possible categories are then visualized: False Negative (FN); False Positives (FP); True Negative (TN); True Positive (TP). This plot helps to visualize the proportion of samples falling in the four possible quadrants, especially in those of the TN and TP predicted categories, in order to evaluate the goodness of the underlying markers.",align="justify"),
                          p("The pie chart shows the very same information in a different way. In this plot can be easily visualized which fraction of false predictions (false positive or false negative) there are in each class (class A/B, disease/healthy) as opposed on how big is the fraction of true predictions and inside the total fraction of markers in each class. Obviously the smaller the false predictions fractions, the better performing is the marker or the combination.",align="justify"),
                          br(),
                div(class = "my-class", "PERFORMANCE ANALYSIS",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:none none solid none;border-width: 1px;"),          
                #h4("Performance Analysis"),
                         br(),
                         p("In the lower section of the page the same ROC curve of the selected combo is overlaid with the corresponding Cross Validation (CV) in order to evaluate its performance. The table below this last plot reports the accuracy (ACC) and error rate of the whole cohort and 10-fold CV, as well as the corresponding sensitivity (SE), specificity (SP) and Area Under the Curve value (AUC).",align="justify"),
                         br(),
                 div(class = "my-class", "PLOTTING MULTIPLE CURVES",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:none none solid none;border-width: 1px;"),          
                #h4("Plotting multiple curves"),
                         br(),
                         p("If you want to visualize and compare ROC curves of multiple markers and combinations among those selected in the Gold table you need to check the \"Check to plot multiple curves\" tick mark in the \"Select Combos\" widget at the top (upper left) of the page.",br()," In the drop down selection menu you will be able to choose, one after the other, all the single markers and/or combos in the gold table that you want to compare: a graph of overlaid ROC curves will be automatically displayed in a single plot. Below the graph SE, SP and AUC for each curve will be reported in a tabular form. As other tables/figures in the application, this one can be copied and downloaded as a csv or pdf file.",align="justify"),   
                         p("Please note that prediction and performance analyses as described before are available for single markers or combinations only, not when multiple markers/combos are compared in the same ROC curve plot.",align="justify"),
                         br(),br(),br(),
                      # h3("Analysis-Interaction map",id="analysisinteraction"),
                      #     p("Lorem ipsum dolor sit amet, consectetur adipisci elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."), 
                div(class = "my-class", "DOWNLOAD",id="download",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #070B11;padding: 5px;  text-align: justify;border-style:none;background-color: #A3BDDC;visibility:visible;"),     
                #h3("Download",id="downlaod"),
                         br(),
                         p("In this section you can download the tabular file of the \"demo data\" and a printable pdf file of the tutorial.",align="justify"),
                         br(),br(),br(),
                div(class = "my-class", "ACCESSORIES",id="accessories",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #070B11;padding: 5px;  text-align: justify;border-style:none;background-color: #A3BDDC;visibility:visible;"),         
                #h3("Accessories",id="accessories"), 
                         br(),
                 div(class = "my-class", "TUTORIAL",id="tutorial",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"),     
                #h4("Tutorial",id="tutorial"),        
                         br(),
                         p("This section describe how to use CombiROC step by step. Real dataset is used as example as \"Demo data (proteomics)\". The demo dataset is obtained from ", a("Mazzara et al. 2015, PLos One 10(9):e0137927",target="_blank",href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0137927"),"and can also be downloaded from the \"Download\" section of the application. ",align="justify"),
                         p("Other \"Demo data (transcriptomics)\" available on the CombiROC website are obtained from ",a("Baraniskin et al. 2011, Blood 117(11):3140-6.",target="_blank",href="http://www.ncbi.nlm.nih.gov/pubmed/21200023") ,align="justify"),
                         br(),
                div(class = "my-class", "LOGS",id="logs",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"),            
                #h4("Logs",id="logs"),
                        br(),
                        p("This section contains the version history of the CombiROC web application. All relevant changes and upgrades will be reported here in the future.",align="justify"),
                        br(),
                div(class = "my-class", "CONTACTS",id="contacts",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"),             
                #4("Contacts",id="contacts"),
                       br(),
                       p("This application was created by the",a("Protein Microarray",target="_blank",href="http://www.ingm.org/en/research/facilities/protein-microarray"), " and ",a("Bioinformatics",target="_blank",href="http://www.ingm.org/en/research/facilities/bioinformatics"), " units at Istituto Nazionale Genetica Molecolare \"Romeo ed Enrica Invernizzi\" (INGM), Milan, Italy. ",align="justify"),
                       p("If you have questions or comments, please contact Saveria Mazzara at",tags$a(href="mailto:mazzara@ingm.org","mazzara@ingm.org"),"This application was implemented using Shiny (for web interface) and other R packages (for data manipulation).",align="justify"),
                       br(),
                div(class = "my-class", "FAQ",id="faq",style="height: 40px;font-family: 'Calibri', cursive; font-size: 20px; line-height: 1.1; color: #4579B9;padding: 5px;  text-align: justify;border-style:solid none none solid;"),             
                #h4("FAQ",id="faq"),
                        p("In this section frequently asked questions on the application and its usage will be added as soon as they will be available.",align="justify"),
                        br(),br()
               
                        #  radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),inline = TRUE),
                        # downloadButton('downloadreport'),
                        # a("click on me",target="_blank",href="myfile.pdf"),
                        # a("click on me",target="_blank",href="links.docx"),
                  

                               
                
                                    ),

                    br(),br(),br(),
                    footer
                       ),

##################################
################# News tab
##################################
                tabItem(tabName = "history",
                  box(width=NULL, status="info",solidHeader=TRUE,title="Logs",
                    # # Brief Help Wording
                    # box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can......"
                    #   ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px;  text-align: justify;
                    #   border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
                       h4("Version History"),
                       p("Current version of CombiROC is v.1.1 (",strong("December 22nd, 2015"),"): added multiple curves plotting functionalities, few bugs fixed."), 
                       p("CombiROC v.1.0 (",strong("September 7th, 2015"),"): first web version running on Shiny server.")
                                    ),

                    br(),br(),br(),
                    footer
                       ),

##################################
################# Contacts tab
##################################
                tabItem(tabName = "contact",
                  box(width=NULL, status="info",solidHeader=TRUE,title="Contacts",
                    # Brief Help Wording
                      headerPanel(list(tags$img(src="logo_INGM.jpg",style="text-align: center;", height=90,width=280),""),windowTitle="Istituto Nazionale"), #"logo-ingm-romeo-erica-invernizzi.png"
                      h3("Istituto Nazionale Genetica Molecolare \"Romeo ed Enrica Invernizzi\""),
                      br(),br(),
                      p("This application was created by the Protein Microarray and Bioinformatics facilities at Istituto Nazionale Genetica Molecolare \"Romeo ed Enrica Invernizzi\" (INGM), Milan, Italy. "), #\"A\" and \"B\"

                       hr(),
                       h4("Bioinformatics- Facility"),
                       p("The ",a("Computational Biology and Data Analysis ",target="_blank",href="http://www.ingm.org/en/research/facilities/bioinformatics"), " (CBDA) group is a team of professional scientists focusing on study, development, and optimization of tools for the analysis of genomic data generated by INGM scientists. Our mission is to work closely with the Institute’s programs facilitating access to biological data analyses and elaborations to all research groups, while supporting translational research with standard analytical expertise and providing access to up-to-date and novel analytical methods."),
                       hr(),
                       h4("Protein Microarray- Facility"),
                       p("The ", a("Facility",target="_blank",href="http://www.ingm.org/en/research/facilities/protein-microarray"), " provides access to Protein Microarray for biomedical research. The provided services cover all the main needs of basic and advanced research by using Protein Microarray.The major objectives of the facility are:"),
                        HTML("<ul><li>Set up, maintain and develop Protein Microarray Technology for basic and advanced research.</li>
                        <li>Provide protein spotting, as such as antigens, antibodies for, extract cell for INGM researchers and external collaborators.</li>
                        <li>Directly provide sample spotting and analysis in specific applications for INGM researchers and external collaborators.</li>
                        <li>Provide on-site training and assistance to prepare biomedical researchers to the use of instrumentations and to introduce to protein microarray applications.</li>
                        <li>Perform independent and collaborative research for the development of advanced protein microarray protocols.</li>
                        <li>Perform collaborative research with INGM groups where highly specific protein microarray platform competences are required.</li></ul>"),

                       hr(),
                       # h4("Publication"),
                       # p("Mazzara S, Rossi RL, Abrignani S, Bombaci M- CombiROC: a web tool for guided and interactive generation of multimarker panels using Combinatorial Analysis and ROC curves..."),
                       # hr(),
                     
                       h4("Developers"),
                       HTML("<ul><li>Saveria Mazzara, PhD</li>
                        <li>Riccardo L. Rossi, PhD</li>
                        <li>Mauro Bombaci, PhD</li></ul>"),
                       hr(),
                        h4("Contact"),
                        p("We welcome your feedback. If you have suggestions for improvements, questions and difficulties, please contact Saveria Mazzara at", tags$a(href="mailto:mazzara@ingm.org","mazzara@ingm.org")),
                       hr(),
                         
                       br(),
                        fluidRow(
                            #column(width = 5,
                   
                                
                                    )
                                    
                
                                    ),

                    br(),br(),br(),
                    footer
                       ),

##################################
################# FAQ tab
##################################
                tabItem(tabName = "FAQ",
                  box(width=NULL, status="info",solidHeader=TRUE,title="FAQ",
                    # # Brief Help Wording
                    # box(width=13,collapsible=TRUE,div(class = "my-class", "In this page you can......"
                    #   ,style="height: 70px;font-family: 'Lobster', cursive; font-size: 15px; line-height: 1.1; color: #4d3a7d;padding: 25px;  text-align: justify;
                    #   border-style:ridge;background-color: #F0FFFF;visibility:visible;")),
                         
                      #h4("Q: What is  CombiROC?")
                      a(h4("Q: What is  CombiROC?"),href="#q1")
                                    
                
                                    ),

                    br(),br(),br(),
                    footer
                       )







                     

                  )   # closing tabItems of dashboardBody

               )

              )


