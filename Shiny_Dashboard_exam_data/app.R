# load librarys
library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(ggplot2)


# define ui
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
              # define headers, in a separate fluid row in order to have input slider and plot on the same height
              h2("Plots from exam dataset"),
              h3("Distribution of target variable")
            ),
          # define ui for histogram and input slider
            fluidRow(
              column(9,
                sidebarLayout(position ="right", # sidebar shall be on the right
                sidebarPanel( # input slider is defined here
                  #box(
                    title = "Input Histogram", status="warning", solidHeader = TRUE, "Amount of students can be specified here",
                    sliderInput("slider", "Amount of students:", 1, 1000, 100)
                  #)
                ),
                # define the main panel, in this panel the histogram shall be plotted
                mainPanel(
                  #box(
                    title = "Histogram of average exam score", status = "primary", solidHeader = TRUE, collapsible =TRUE,
                    plotOutput("hist_plot_score", height = 450)),
                
                  #)
                )
              ),
            ),
          # define headers, in a separate fluid row in order to have input slider and plot on the same height
            fluidRow(
              h3("Categorical Plots"),
              h4("Boxplots"),
            ),
          # define ui for boxplot and input selection for boxplot
            fluidRow(
              column(9,
                sidebarLayout(position="right", # sidebar shall be on the right
                sidebarPanel( # Possible variables for the boxplot are defined here
                    title = "Input Boxplot", status="warning", solidHeader = TRUE, "Variable for Boxplot can be specidied here",
                    selectInput("var", h3("Select variable for Boxplot"), 
                                choices = list("Gender",
                                               "Ethnic group" ,
                                               "Parent education" ,
                                               "Type of lunch" ,
                                               "Test preparation course" ), selected ="gender")
                ),
                # define the main panel, in this panel the boxplot shall be plotted
                mainPanel(
                  #box(
                    title = "Histogram of average exam score", status = "primary", solidHeader = TRUE, collapsible =TRUE,
                    plotOutput("box_plot", height = 450),
                    
                    textOutput("selected_var"),
                
                  #)
                  
                )
                )
              ),
            ),
          # define headers, in a separate fluid row in order to have input slider and plot on the same height
          fluidRow(
            h4("Density Plots"),
          ),
          #define ui for density plot and input selection for density plot (color and facet wrap)
          fluidRow(
            column(9,
                sidebarLayout(position="right", # sidebar shall be on the right
                sidebarPanel( # Possible variables for the density plot are defined here. One input for the color and one input for the facet_wrap
                  title = "Input Density Plot", status="warning", solidHeader = TRUE, "Variable for Density plots can be specidied here",
                  selectInput("var2", h3("Select color for Density plot"), 
                              choices = list("Gender",
                                             "Ethnic group" ,
                                             "Parent education" ,
                                             "Type of lunch" ,
                                             "Test preparation course" ), selected ="gender"),
                  selectInput("var3", h3("Select facet wrap for Density plot"), 
                              choices = list("Gender",
                                             "Ethnic group" ,
                                             "Parent education" ,
                                             "Type of lunch" ,
                                             "Test preparation course" ), selected ="gender")
                ),
                # define the main panel, in this panel the density plot shall be plotted
                mainPanel(
                  title = "Density plot of average exam score", status = "primary", solidHeader = TRUE, collapsible =TRUE,
                  plotOutput("dens_plot", height = 450),
                  
                  textOutput("selected_var2"),
                  textOutput("selected_var3"),
                )
                )
            )
          )
      ),

      # Content in second tab. Further topics can be added here, was not achieved due to lack of time at the end.
           tabItem(tabName = "widgets",
            h2("Widgets tab content")
            )
      )
    )
  )
)

# define the server, app contains of ui and server
server <- function(input, output) {
# read in the data and make preprocessing  
  path <- "https://raw.githubusercontent.com/DanielSteck/Project-Students-grades/main/exams.csv"
  dataset <- read_csv(path, show_col_types = FALSE)
  dataset <- dataset %>% clean_names
  dataset <- dataset %>% mutate (avg_score = (math_score + reading_score + writing_score)/3)
 
# Histogram of average exam scores  
   output$hist_plot_score <- renderPlot({
    # x shall be the amount of collected students
    x    <- dataset$avg_score[seq_len(input$slider)]
    bins <- 10 
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Average performance in exam",
         main = "Histogram of average score in exam")
    
  })
 # Boxplot for selected variable
   output$box_plot <- renderPlot({
     #transform selected variable into column of dataframe
     data <- switch(input$var,
                    "Gender"=dataset$gender,
                    "Ethnic group" = dataset$race_ethnicity,
                    "Parent education" = dataset$parental_level_of_education,
                    "Type of lunch" = dataset$lunch,
                    "Test preparation course" = dataset$test_preparation_course)
     # add boxplot. Also with the input from the amount of students slider
     boxplot(dataset$avg_score[seq_len(input$slider)] ~ data[seq_len(input$slider)], col = "#007bc2", 
            xlab = input$var,
            main = "Students performance in exams",
            ylab = "Average performance in exam")
   })
   # print which variable was selected
   output$selected_var <- renderText({ 
     paste("You have selected", input$var)
   })
   # dense plot
   output$dens_plot <- renderPlot({
     #transform selected variables into column of dataframe
     data2 <- switch(input$var2,
                    "Gender"=dataset$gender,
                    "Ethnic group" = dataset$race_ethnicity,
                    "Parent education" = dataset$parental_level_of_education,
                    "Type of lunch" = dataset$lunch,
                    "Test preparation course" = dataset$test_preparation_course)
     #transform selected variables into column of dataframe
     data3 <- switch(input$var3,
                     "Gender"=dataset$gender,
                     "Ethnic group" = dataset$race_ethnicity,
                     "Parent education" = dataset$parental_level_of_education,
                     "Type of lunch" = dataset$lunch,
                     "Test preparation course" = dataset$test_preparation_course)
     # add dense plot, (Also with the input from the amount of students slider --> did not work)
     ggplot(dataset, aes(x=dataset$avg_score, fill=data2)) +
       geom_density(alpha=0.7) + 
       scale_fill_manual(input$var2, values = c("#339999", "#FFFF66", "blue", "green", "orange", "red"))+
       facet_wrap(~dataset$gender, ncol=1)+ #facet_wrap(~data3) is not working therefore as a temporary solution I will use gender always as facet_wrap
       labs(title = "Students average performance in exams", 
            x = "Average score in exams",
            y = "density") +
       theme_minimal()+
       theme (text = element_text(size=15),
              legend.key.size = unit(10, "mm"))

   })
   # Print out which variable was selected as color in the dense plot
   output$selected_var2 <- renderText({ 
     paste("You have selected", input$var2, "as color in the density plot")
   })
   # Print out which variable was selected as facet_wrap in the dense plot
   output$selected_var3 <- renderText({ 
     paste("You have selected", input$var3, "as facet wrap in the density plot")
   })

   
}

shinyApp(ui, server)