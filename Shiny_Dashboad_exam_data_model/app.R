# load librarys
library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(ggplot2)
# load the dataset and clean up the names plus add the target variable
path <- "https://raw.githubusercontent.com/DanielSteck/Project-Students-grades/main/exams.csv"
dataset <- read_csv(path, show_col_types = FALSE)
dataset <- dataset %>% clean_names
dataset <- dataset %>% mutate (avg_score = (math_score + reading_score + writing_score)/3)

#change cloumns from categorical to factor:

dataset$gender <- factor(dataset$gender)
dataset$race_ethnicity <- factor(dataset$race_ethnicity)
dataset$parental_level_of_education <- factor(dataset$parental_level_of_education)
dataset$lunch <- factor(dataset$lunch)
dataset$test_preparation_course <- factor(dataset$test_preparation_course)



# load the model
loaded_model <- readRDS(file = "exam_model.rds")

# define ui
ui <- dashboardPage(
  
# Title of dashboard
  dashboardHeader(title= "Exam data visualization"),
  
# Content in sidebar  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      #menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Model Prediction", tabName = "model", icon = icon("th"))
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
              h2("Introduction"),
              "Welcome to my first deployed application in shiny.",
              br(),
              "This small Dashboard was created within the lecture Programming Languages for Data Science.",
              br(),
              "Several visualizations are on this page. The visualized dataset contains 1000 observations of students, their attributes and their performance in their exams.",
              br(),
              "Below some plots are showing the imapct of the variables on the target variable. The target varibale is the average score in the exams. As the original dataframe contained three different exams, the mean was taken from them and a new variable was added which contains the average score in all three exams.",
              br(),
              "In a R project a linear regression was performed in which the average exam score was predicted. Thats why this variable is called target variable. Scope of this Dashboard is to make an EDA so that it is possible to see the distribution of the target variable as well as the impact of the other variables in the average exam score.",
              h2("Plots from exam dataset"),
              h3("Distribution of target variable"),
              "First of all the distribution of the target variable shall be displayed within a histogram.",
              br(),
              "The x-axis shows the average exam score and the y-axis shows the frequency of students who are within that bin.",
              br(),
              "On the right of the histogram is a slider implemented. This slider determines the amount of students who shall be displayed in the histogram and the boxplot below.",
              br()
              
            ),
          # define ui for histogram and input slider
            fluidRow(
              column(9,
                sidebarLayout(position ="right", # sidebar shall be on the right
                sidebarPanel( # input slider is defined here
                  #box(
                    title = "Input Histogram", status="warning", solidHeader = TRUE, "Amount of students can be specified here",
                    sliderInput("slider", "Amount of students:", 1, 1000, 600)
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
              "Scope of the boxplots is to display the influence of the different variables on the average exam score.",
              br(),
              "On the right you can chose the variable you want to display in the boxplot below.",
              br(),
              "The x-axis shows all levels within the categorical variable and the y-axis shows the distribution within that level.",
              br(),
              "Boxplots help for a quick understanding of the available data, as the median, the first quantil and the third quantil are beeing displayed. Moreover outliers can be detected.",
              br()
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
            "Additionally to the boxplots you can play around with the density plot and specify the variables you want to get plotted.",
            br(),
            "On the right you can specify which variable shall be filled as color and which one shall be used for a facet_wrap. Sadly the facet_wrap is not working with the drop down selection, therefore allways the gender variable is used for the facet_wrap.",
            br(),
            " The x-axis shows the average score in the exams while the y-axis shows the density within the selected variable.",
            br(),
            "for testing purposes the last plot was created with ggplot instead of the shiny visualizations."
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
           #tabItem(tabName = "widgets",
            #h2("Widgets tab content")
            #)
      # Content in second/third tab. Model prediction will be added here.
           tabItem(tabName = "model",
                   fluidPage( #fluid page make the correct visualization based on users display
                     titlePanel(h1("Exam Score Prediction")),
                     fluidRow(
                       # define headers, in a separate fluid row in order to have input slider and plot on the same height
                       h2("Introduction"),
                       "In this chapter you can make predictions regarding the average exam score which can be predicted for students with certain characteristics. The added model is the same which was created within the quarto document. The model was saved as .rds file and was loaded into this application.",
                       br(),
                       "Feel free to play around with the possible inputs on the right. Below you can find the predicttion of the average exam score. Moreover you can find as text which data was added into the model. Furthermore, the added data is displayed as a Dataframe.",
                       br(),
                       h4("Model Description"),
                       "The added model is a linerar regression model which takes all features as an Input. The output which shall be predicted is the average score in the exams. The training was made with all available data from the exam dataset.",
                       br(),
                       "The following recipe steps are made as preprocessing and are added into a recipe:",
                       br(),
                       "1. Drop missing data. The whole row will be dropped in case of missing data.",
                       br(),
                       "2. Removal of outliers.",
                       br(),
                       "3. One Hot Encoding for the categorical variables.",
                       br(),
                       "4. Removal of correlated features. For this the spearman method is used with a threshhold of 0,7.",
                       br(),
                       h2("Predict the exam score of a student"),
                       br(),

                       
                     ),
                     fluidRow(
                       column(9,
                              sidebarLayout(position="right", # sidebar shall be on the right
                                            sidebarPanel( # Choose your variables for the model prediction.
                                              title = "Data for prediction", status="warning", solidHeader = TRUE, "Chooses the data for the student",
                                              selectInput("model_input_gender", label = "gender", choices = unique(dataset$gender)),
                                              selectInput("model_input_ethnicity", label = "Ethnic group", choices = unique(dataset$race_ethnicity)),
                                              selectInput("model_input_education", label = "Parent education", choices = unique(dataset$parental_level_of_education)),
                                              selectInput("model_input_lunch", label = "Type of lunch", choices = unique(dataset$lunch)),
                                              selectInput("model_input_test_prep", label = "Test preparation course", choices = unique(dataset$test_preparation_course)),
                                            ),
                                            # define the main panel, in this panel the model prediction shall be displayed
                                            mainPanel(
                                              verbatimTextOutput(outputId = "prediction"),
                                              #title = "Histogram of predicted exam scores", status = "primary", solidHeader = TRUE, collapsible =TRUE,
                                              #plotOutput("his_prediction", height = 450),
                                              h4("Selected Values"),
                                              
                                              

                                              
                                              textOutput("selected_input_gender"),
                                              textOutput("selected_input_ethnicity"),
                                              textOutput("selected_input_education"),
                                              textOutput("selected_input_lunch"),
                                              textOutput("selected_input_test_prep"),
                                              br(),
                                              h4("Selected values as a Dataframe"),
                                              br(),
                                              tableOutput("dataframe")
                                              
                                            )
                              )
                       )

                       
                     )
                     
            )
           
      )
      )
    )
  )
)

# define the server, app contains of ui and server
server <- function(input, output) {
# read in the data and make preprocessing  
 # path <- "https://raw.githubusercontent.com/DanielSteck/Project-Students-grades/main/exams.csv"
  #dataset <- read_csv(path, show_col_types = FALSE)
  #dataset <- dataset %>% clean_names
  #dataset <- dataset %>% mutate (avg_score = (math_score + reading_score + writing_score)/3)
 
# Histogram of average exam scores  
   output$hist_plot_score <- renderPlot({
    # x shall be the amount of collected students
    x    <- dataset$avg_score[seq_len(input$slider)]
    bins <- 10 
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Average score in exam",
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
            main = "Students average score in exams",
            ylab = "Average score in exam")
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
     ggplot(dataset, aes(x=avg_score, fill=data2)) +
       geom_density(alpha=0.7) + 
       scale_fill_manual(input$var2, values = c("#339999", "#FFFF66", "blue", "green", "orange", "red"))+
       facet_wrap(~dataset$gender, ncol=1)+ #facet_wrap(~data3) is not working therefore as a temporary solution I will use gender always as facet_wrap
       labs(title = "Students average score in exams", 
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
   
 
   random_student <- reactive({
     # Create a data frame with the user's input
     new_Data <- data.frame(
       gender = factor(input$model_input_gender),
       ethnic_group = factor(input$model_input_ethnicity),
       parent_education = factor(input$model_input_education), 
       lunch = factor(input$model_input_lunch),
       test_prep = factor(input$model_input_test_prep)
     )
     
     # Return the data frame
     return(new_Data)
   })
   
   predicted_value <- reactive({
     # Get the data frame generated by random_student()
     new_Data <- random_student()
     
     # Make a prediction on the new data using the loaded model
     prediction <- predict(loaded_model, new_Data)
     
     # Return the predicted value
     return(prediction)
   })
   
   # Render the resulting dataframe
   output$dataframe <- renderTable({
     random_student()
   })
   
   output$prediction <- renderText({
     paste("The model prediction is:", round(predicted_value(), 2))
   })

   output$selected_input_gender <- renderText({ 
     paste("Selected as gender:", input$model_input_gender)
   })
   
   output$selected_input_ethnicity <- renderText({ 
     paste("Selected as Ethnic Group:", input$model_input_ethnicity)
   })
   
   output$selected_input_education <- renderText({ 
     paste("Selected as Level of parents education:", input$model_input_education)
   })
   
   output$selected_input_lunch <- renderText({ 
     paste("Selected as type of lunch:", input$model_input_lunch)
   })
   
   output$selected_input_test_prep <- renderText({ 
     paste("Selected as test preparation course:", input$model_input_test_prep)
   })
   
   


   
}

shinyApp(ui, server)