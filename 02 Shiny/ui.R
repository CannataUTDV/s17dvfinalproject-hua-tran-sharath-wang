#ui.R
library(plotly)
library(RColorBrewer)
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstab - AADR by State, Cause", tabName = "SNT1", icon = icon("dashboard")),
      menuItem("Scatter - Death rate v Education", tabName = "SNT2", icon = icon("dashboard")),
      menuItem("Barchart - Death rate by Year,Cause", tabName = "NH1", icon = icon("dashboard")),
      menuItem("SetID - BS Attainment: Female", tabName = "NH2", icon = icon("dashboard")),
      menuItem("SetID - BS Attainment: Male", tabName = "NH3", icon = icon("dashboard")),
      menuItem("Map - Death rate and Education level", tabName = "SNT3", icon = icon("dashboard")),
      menuItem("Boxplot - Amount of Deaths per Cause", tabName = "NH4", icon = icon("dashboard")),
      menuItem("Histogram - AADR", tabName = "NH5", icon = icon("dashboard"))
   
      
    )
  ),
  dashboardBody(    
    tabItems(

#Begin SNT1 Tab --------------------------------
tabItem(tabName = "SNT1",
  tabsetPanel(
    tabPanel("Data",  
             sliderInput("AADR1", "AADR_Low:", 
                         min = 0, max = 2000,  value = 1000),
             sliderInput("AADR2", "AADR_Medium:", 
                         min = 2000, max = 10000,  value = 5000),
             actionButton(inputId = "click1",  label = "To get data, click here"),
             hr(), # Add space after button.
             DT::dataTableOutput("data1")
    ),
    tabPanel("Crosstab", plotOutput("plot1", height=750))
  )
),
#End SNT1 Tab ___________________________________

#Begin SNT2 Tab --------------------------------
tabItem(tabName = "SNT2",
        tabsetPanel(
          tabPanel("Data",
                   actionButton(inputId = "click8",  label = "To get data, click here"),
                   hr(), # Add space after button.
                   DT::dataTableOutput("data8")
          ),
          tabPanel("Scatter Plot", plotOutput("plot8", height=750))
        )
),
#End SNT2 Tab ___________________________________

#Begin NH1 Tab --------------------------------
tabItem(tabName = "NH1",
        tabsetPanel(
          tabPanel("Data",
                   actionButton(inputId = "click2",  label = "To get data, click here"),
                   hr(), # Add space after button.
                   'Here is data for the "Barchart - AADR by Year,Cause" tab',
                   hr(),
                   DT::dataTableOutput("barchartData2")
          ),
          tabPanel("Full Barchart", "Black = Sum of AADR per Cause, Red = Average Sum of AADR per Year, and  Blue = (Sum of AADR per Cause - Average Sum of AADR per Year)", plotOutput("barchartPlot2_All", height=800)),
          tabPanel("Zoomed Barchart", "Black = Sum of AADR per Cause, Red = Average Sum of AADR per Year, and  Blue = (Sum of AADR per Cause - Average Sum of AADR per Year)", plotOutput("barchartPlot2_Zoom", height=2000))
        )
),
#End NH1 Tab ___________________________________
#Begin NH2 Tab --------------------------------
tabItem(tabName = "NH2",
        tabsetPanel(
                tabPanel("Data for High Level",
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "SetID - BS Attainment: Female" tab',
                         hr(),
                         DT::dataTableOutput("barchartData3")
                ),
                tabPanel("Barchart", plotOutput("barchartPlot3", height=750)),
                tabPanel("Data for Low Level",
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "SetID - BS Attainment: Female" tab',
                         hr(),
                         DT::dataTableOutput("barchartData4")
                ),
                tabPanel("Barchart", plotOutput("barchartPlot4", height=750))
        )
),
#End NH2 Tab ___________________________________
#Begin NH3 Tab --------------------------------
tabItem(tabName = "NH3",
        tabsetPanel(
                tabPanel("Data for High Level",
                         actionButton(inputId = "click5",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "SetID - BS Attainment: Male" tab',
                         hr(),
                         DT::dataTableOutput("barchartData5")
                ),
                tabPanel("Barchart", plotOutput("barchartPlot5", height=750)),
                tabPanel("Data for Low Level",
                         actionButton(inputId = "click6",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "SetID - BS Attainment: Male" tab',
                         hr(),
                         DT::dataTableOutput("barchartData6")
                ),
                tabPanel("Barchart", plotOutput("barchartPlot6", height=750))
        )
),
#End NH3 Tab ___________________________________
#Begin SNT3 Tab --------------------------------
tabItem(tabName = "SNT3",
        tabsetPanel(
          tabPanel("Data",
                   actionButton(inputId = "click7",  label = "To get data, click here"),
                   hr(), # Add space after button.
                   'Here is data for the "Map - AADR and Fraction BS" tab',
                   hr(),
                   DT::dataTableOutput("mapData7")
          ),
          tabPanel("Maps", plotlyOutput("map2", height = 400), hr(), plotlyOutput("map1", height = 400))
          )
        ),
#End SNT3 Tab ___________________________________
#Begin NH4 Tab --------------------------------
tabItem(tabName = "NH4",
        tabsetPanel(
          tabPanel("Data",
                   actionButton(inputId = "click9",  label = "To get data, click here"),
                   hr(), # Add space after button.
                   'Here is data for the "Boxplot - Amount of Deaths per Cause" tab',
                   hr(),
                   DT::dataTableOutput("boxData9")
          ),
          tabPanel("Boxplot - Amount of Deaths per Cause", plotOutput("boxplot9", height = 800))
          )
        ),
#End NH4 Tab ___________________________________
#Begin NH5 Tab ___________________________________
tabItem(tabName = "NH5",
        tabsetPanel(
                tabPanel("Data",
                         actionButton(inputId = "click10",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "Histogram - AADR" tab',
                         hr(),
                         DT::dataTableOutput("histogramData1")
                ),
                tabPanel("Histogram - AADR", plotOutput("histogramPlot1", height = 800))
        )
)
#End NH5 Tab ___________________________________
    )
  )
)

