fluidPage(
  #Create a dark theme layout of the page.
  theme = shinytheme("darkly"),
  # shinythemes::themeSelector(),
  #Main body of the project consisting three tabs with different search options
  navbarPage(
    "Project",
    #Creating the first tab of the Shiny Application displaying the information at UC Davis
    tabPanel("Davis Weather", (htmlOutput("text1"))),
    #Creating the second tab of the Shiny Application allowing people to search air quality by longitude and latitude
    tabPanel(
      "Search by lat&lon",
      #Textbar corresponding to the value of latitude
      textInput(inputId = "lat", label = "write a lat", value = "34.03"),
      #Textbar corresponding to the value of longitude
      textInput(inputId = "lon", label = "write a lon", value = "-118.15"),
      #Friendly reminders of what AQI numbers mean to people
      tags$h5("AQI Index"),
      tags$h5("0~50:Good"),
      tags$h5("51~100:Moderate"),
      tags$h5("101~150:Unhealthy for Sensitive Groups"),
      tags$h5("151~200:Unhealthy"),
      tags$h5("201~300:Very Unhealthy"),
      tags$h5("301~500:Hazardous"),
      
      #A button to push to display a table of air quality and a navigating map
      actionButton(inputId = "go1", label = "update"),
      tableOutput("table"),
      leafletOutput("plot1")
    ),
    #Creating the third tab allowing users to search air quality by states.
    tabPanel(
      "Search by state",
      #Sidebar to choose the state with all states included
      sidebarPanel(
        selectInput(
          inputId = "select_state",
          label = "Choose state",
          choices = c("", all_state_list),
          
        ),
        #Creating a check box allowing users to compare different variables in different cities at chosen state
        checkboxGroupInput(
          inputId = "show_num_variable",
          label = "Choose numeric variable",
          choices = "empty"
        ),
        #displaying a map comparing a certain variable among different cities on a map
        selectInput(
          inputId = "show_num_variable_map",
          label = "Choose numeric variable on map",
          choices = "empty"
        ),
      ),
      #Panel that ouput the data by the selection made from tabpanel
      mainPanel(
        #Create a panel with three tabs
        tabsetPanel(
          #First tab containing table of all the variables and cities at the chosen state
          tabPanel("Table", tableOutput("show_table")),
          #Second tab containing a histogram comparing some variables among different cities at the chosen state
          tabPanel("Variable compare", plotOutput("plot_variable")),
          #Third tab containing a map that illustrate different variables among all the cities at the chosen state
          tabPanel("Map", plotlyOutput("show_map")),
          #A friendly reminder
          h4("For the data of California, it may take up to 10 minutes to get the result, 
                  since there are too many City observations in California.")
        )
      )
    )
  )
)