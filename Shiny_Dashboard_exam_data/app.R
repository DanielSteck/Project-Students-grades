library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)



ui <- dashboardPage(
  
# Title of dashboard
  dashboardHeader(title= "Exam data visualization"),
  
# Content in sidebar  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  
# Content in Body
  dashboardBody(
    tabItems(
      # Content in first tab
      tabItem(tabName = "dashboard",
        fluidPage( #fluid page make the correct visualization based on users display
          titlePanel(h1("Data Visualization")),
          
            fluidRow(
              column(9,
                sidebarLayout(position ="right",
                sidebarPanel(
                  #box(
                    title = "Input Histogram", status="warning", solidHeader = TRUE, "Amount of students can be specified here",
                    sliderInput("slider", "Amount of students:", 1, 1000, 100)
                  #)
                ),
                mainPanel(
                  h2("Plots from exam dataset"),
                  h3("Distribution of target variable"),
                  #box(
                    title = "Histogram of average exam score", status = "primary", solidHeader = TRUE, collapsible =TRUE,
                    plotOutput("hist_plot_score", height = 250)),
                
                  #)
                )
              ),
            ),
          
            fluidRow(
              column(9,
                sidebarLayout(position="right",
                sidebarPanel(
                    title = "Input Histogram", status="warning", solidHeader = TRUE, "Variable for Boxplot can be specidied here",
                    selectInput("select", h3("Select variable for Boxplot"), 
                                choices = list("Gender" = 1, "Ethnic group" = 2,
                                               "Parent education" = 3, "Type of lunch" = 4, "Test preparation course" =5), selected = 1)
                ),
                mainPanel(
                  h3("Categorical Plots"),
                  h4("Boxplots"),
                  #box(
                    title = "Histogram of average exam score", status = "primary", solidHeader = TRUE, collapsible =TRUE,
                    plotOutput("box_plot", height = 250),
                
                  #)
                  
                )
                )
              ),
            ),
      ),

      # Content in second tab
           tabItem(tabName = "widgets",
            h2("Widgets tab content")
            )
      )
    )
  )
)

server <- function(input, output) {
# read in the data and make preprocessing  
  path <- "https://raw.githubusercontent.com/DanielSteck/Project-Students-grades/main/exams.csv"
  dataset <- read_csv(path, show_col_types = FALSE)
  dataset <- dataset %>% clean_names
  dataset <- dataset %>% mutate (avg_score = (math_score + reading_score + writing_score)/3)
 
# Histogram of average exam scores  
   output$hist_plot_score <- renderPlot({
    
    x    <- dataset$avg_score[seq_len(input$slider)]
    bins <- 10 #seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Average performance in exam",
         main = "Histogram of average score in exam")
    
  })
   
# Boxplot for target variable and filtered variable  
   #output$box_plot <- renderPlot({
    
    #x    <- dataset$avg_score[seq_len(input$slider)]
    #bins <- 10 #seq(min(x), max(x), length.out = input$bins + 1)
    
    #hist(x, breaks = bins, col = "#007bc2", border = "white",
         #xlab = "Average performance in exam",
         #main = "Histogram of average score in exam")
    
  #})
   
}

shinyApp(ui, server)