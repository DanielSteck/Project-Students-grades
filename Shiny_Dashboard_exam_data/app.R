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
              h2("Plots from exam dataset"),
              h3("Distribution of target variable")
            ),
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
                  #box(
                    title = "Histogram of average exam score", status = "primary", solidHeader = TRUE, collapsible =TRUE,
                    plotOutput("hist_plot_score", height = 250)),
                
                  #)
                )
              ),
            ),
            fluidRow(
              h3("Categorical Plots"),
              h4("Boxplots"),
            ),
            fluidRow(
              column(9,
                sidebarLayout(position="right",
                sidebarPanel(
                    title = "Input Histogram", status="warning", solidHeader = TRUE, "Variable for Boxplot can be specidied here",
                    selectInput("var", h3("Select variable for Boxplot"), 
                                choices = list("Gender",
                                               "Ethnic group" ,
                                               "Parent education" ,
                                               "Type of lunch" ,
                                               "Test preparation course" ), selected ="gender")
                ),
                mainPanel(
                  #box(
                    title = "Histogram of average exam score", status = "primary", solidHeader = TRUE, collapsible =TRUE,
                    plotOutput("box_plot", height = 250),
                    
                    textOutput("selected_var"),
                
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
   

 
   output$box_plot <- renderPlot({
     data <- switch(input$var,
                    "Gender"=dataset$gender,
                    "Ethnic group" = dataset$race_ethnicity,
                    "Parent education" = dataset$parental_level_of_education,
                    "Type of lunch" = dataset$lunch,
                    "Test preparation course" = dataset$test_preparation_course)
     boxplot(dataset$avg_score[seq_len(input$slider)] ~ data[seq_len(input$slider)], col = "#007bc2", 
            xlab = input$var,
            main = "Students performance in exams",
            ylab = "Average performance in exam")
   })
   
   output$selected_var <- renderText({ 
     paste("You have selected", input$var)
   })
   
   
}

shinyApp(ui, server)