library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
# Title of dashboard
  dashboardHeader(title= "Exam data visualization"),
  
# Content in sidebar  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashbaord")),
      menuItem("widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  
# Content in Body
  dashboardBody(
    tabItems(
      # Content in first tab
      tabItem(tabName = "dashboard",
        fluidRow(
          box(plotOutput("plot1", height = 250)),
          
          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 1000, 100)
          )
        )
      ),

      # Content in second tab
           tabItem(tabName = "widgets",
            h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)