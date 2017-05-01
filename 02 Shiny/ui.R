#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Barchart - AADR by State,Cause", tabName = "SNT1", icon = icon("dashboard")),
      menuItem("Barchart - AADR by Year,Cause", tabName = "NH1", icon = icon("dashboard")),
      menuItem("SetID - BS Attainment: Female", tabName = "NH2", icon = icon("dashboard")),
      menuItem("SetID - BS Attainment: Male", tabName = "NH3", icon = icon("dashboard"))
      
    )
  ),
  dashboardBody(    
    tabItems(
      
#Begin SNT1 Tab --------------------------------
      tabItem(tabName = "SNT1",
        tabsetPanel(
          tabPanel("Data",
             actionButton(inputId = "click1",  label = "To get data, click here"),
             hr(), # Add space after button.
             'Here is data for the "Barchart - AADR by State,Cause" tab',
             hr(),
             DT::dataTableOutput("barchartData1")
          ),
          tabPanel("Barchart", "Black = Sum of AADR per Cause, Red = Average Sum of Sales per State, and  Blue = (Sum of AADR per Cause - Average Sum of AADR per State)", plotOutput("barchartPlot1", height=10000))
        )
      ),
#End SNT1 Tab ___________________________________

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
          tabPanel("Barchart", "Black = Sum of AADR per Cause, Red = Average Sum of AADR per Year, and  Blue = (Sum of AADR per Cause - Average Sum of AADR per Year)", plotOutput("barchartPlot2", height=3000))
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
)
#End NH3 Tab ___________________________________
    )
  )
)

