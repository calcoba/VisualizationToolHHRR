library("shiny")
library(ggplot2)
library(tidyverse)

complete_data = read.csv('data/HRDataset_v14.csv', header=TRUE, sep=',')
mean_data <- complete_data %>%
  na.omit() %>%
  group_by(MaritalDesc, Sex) %>% 
  summarise(mean_salary = mean(Salary)) 

median_data <- complete_data %>%
  na.omit() %>%
  group_by(MaritalDesc, Sex) %>% 
  summarise(median_salary = median(Salary)) 

ui <- ui <- navbarPage("My Application",
                       tabPanel("Salary",
                                fluidPage(
                                  titlePanel(title = h2("Incomes in a year by Sex for different Marital Categories", align="center")),
                                  
                                  hr(),
                                  column (2, 
                                          h4("Select the statistic to show:"),
                                          radioButtons("STATISTIC", "Select the statistic",
                                                       choices = c("Mean", "Median"),
                                                       selected = "Mean"),
                                  ),
                                  
                                  column(10,
                                         plotOutput("global"),
                                  ),
                                  
                                  hr(), 
                                  
                                  column(3,
                                         h4("Select the Marital Status to zoom in:"),
                                         radioButtons("MARITAL", "Select the Marital Status",
                                                      choices = c("Single", "Married", "Divorced", "Separated", "Widowed"),
                                                      selected = "Single"),
                                         h4("Select the Gender to display the Income distribution:"),
                                         radioButtons("GENDER", "Select the Gender",
                                                      choices = c("Male", "Female"),
                                                      selected = "Male")),
                                  
                                  column(3, offset=1,
                                         h3("Income in a year by Sex for:"),
                                         plotOutput('zoom')),
                                  
                                  column(5,
                                         h3("Income distributiion for:"),
                                         plotOutput('histogram'))
                                )
                       ),
                       tabPanel("Department",
                                titlePanel("Normalized stacked Barchart"),
                                
                                sidebarLayout(
                                  sidebarPanel(
                                    helpText("Select the variables to be included as colors"),
                                    
                                    selectInput("attribute", 
                                                label = "Choose a variable to display",
                                                choices = c("Recruitment Source", "Race",
                                                            "Sex"),
                                                selected = "Recruitment Source"),
                                    
                                    
                                    selectInput("norm", 
                                                label = "Select normalization option in y axis",
                                                choices = c("Normalized", "Stacked", "Side-by-side"),
                                                selected = "Normalized"),
                                    
                                    width = 2),
                                  
                                  mainPanel(
                                    plotOutput("stacked_bar", height = 1000),
                                    width = 7)
                                )),
                       tabPanel("Satisfaction",
                                h2("Heatmap"),
                                fluidPage(
                                  h4("Select the variables to be included as colors"),
                                  
                                  column(4,
                                  selectInput("score", 
                                              label = "Choose a variable to display",
                                              choices = c("PerfScoreID", "Satisfaction","Salary"),
                                              selected = "Performance"),
          
                                ),
                                

                                plotOutput("heatmap", height = 1000)
                                
                                     )
                       
                                )
)

# Define server logic required to draw a histogram ----
server <- function(input,output){
  reactive_data_marital = reactive({
    marital = input$MARITAL
    return(complete_data[complete_data$MaritalDesc==marital,])
    
  })
  
  reactive_data_gender = reactive({
    gender = input$GENDER
    marital = input$MARITAL
    if(gender=='Male'){gender='M '}
    else{gender='F'}
    marital_data = complete_data[complete_data$MaritalDesc==marital,]
    return(marital_data[marital_data$Sex==gender,])
    
  })
  
  
  output$global <-  renderPlot({
    if (input$STATISTIC=="Mean"){
      use_data = mean_data
      y_data = mean_data$mean_salary
    }
    
    else{
      use_data = median_data
      y_data = median_data$median_salary
    }
    ggplot(mean_data, aes(x=Sex, y=y_data, fill=Sex)) + 
      geom_col(width=0.3) +
      facet_wrap(~MaritalDesc) +
      xlab("Gender") + 
      ylab("Mean Salary") + 
      theme(axis.text = element_text(size=15),
            axis.title = element_text(size=20),
            strip.text = element_text(size = 20),
            legend.key.size = unit(1, 'cm'),
            legend.title = element_text(size=20),
            legend.text = element_text(size=15))
  })
  
  
  output$zoom <- renderPlot({
    
    our_data <- reactive_data_marital()
    
    if (input$STATISTIC=="Mean"){
      Male = mean(our_data[our_data$Sex=='M ',]$Salary)
      Female = mean(our_data[our_data$Sex=='F',]$Salary)
      Male_sd = sd(our_data[our_data$Sex=='M ',]$Salary)
      Female_sd = sd(our_data[our_data$Sex=='F',]$Salary)
    }
    
    else{
      Male = median(our_data[our_data$Sex=='M ',]$Salary)
      Female = median(our_data[our_data$Sex=='F',]$Salary)
      Male_sd = mad(our_data[our_data$Sex=='M ',]$Salary)
      Female_sd = mad(our_data[our_data$Sex=='F',]$Salary)
    }
    
    
    
    salary_data = data.frame(Gender=c('Male', 'Female'), Salary=c(Male, Female),
                             sd_error=c(Male_sd, Female_sd))
    
    ggplot(salary_data, aes(x=Gender, y=Salary, fill=Gender)) + 
      geom_col(width=0.3) +
      geom_errorbar(aes(x=Gender, ymin=Salary-sd_error, ymax=Salary+sd_error), 
                    width=0.1, colour="orange", alpha=0.9, size=1.3) +
      ggtitle(input$MARITAL) + 
      theme(axis.text = element_text(size=15),
            axis.title = element_text(size=20),
            plot.title =  element_text(size = 20),
            legend.position = "None")
  })
  
  output$histogram <- renderPlot({
    work_data <- reactive_data_gender()
    histogram_title = paste(input$MARITAL, 'and', input$GENDER)
    if (input$GENDER == 'Male'){
      color_selected = "#00BFC4"
    }
    else{
      color_selected = "#F8766D"
    }
    ggplot(work_data, aes(Salary)) +
      geom_histogram(color='black', fill = color_selected) +
      ggtitle(histogram_title) + 
      xlab("Salary") + 
      ylab("Count") + 
      theme(axis.text = element_text(size=15),
            axis.title = element_text(size=20),
            plot.title =  element_text(size = 20))
    
    
  })
  
  output$stacked_bar <- renderPlot({
    
    feature <- switch(input$attribute, 
                   "Recruitment Source" = complete_data$RecruitmentSource,
                   "Race" = complete_data$RaceDesc,
                   "Sex" = complete_data$Sex)
    
    stack_var = input$attribute
    
    position <- switch(input$norm,
                       "Normalized" = "fill", 
                       "Stacked" = "stack",
                       "Side-by-side" = "dodge")
    
    if (position=='Normalized'){count='Percentage'}
    else {count = 'Count'}
    
    ggplot(complete_data, aes(fill=feature, x=Department)) + 
      geom_bar(position=position) +
      scale_fill_brewer(palette = "Set1") +
      labs(y = count, fill=stack_var) + 
      theme(axis.text = element_text(size=15),
            axis.title = element_text(size=20),
            strip.text = element_text(size = 20),
            legend.key.size = unit(1, 'cm'),
            legend.title = element_text(size=20),
            legend.text = element_text(size=15))
  })
  
  
  output$heatmap <- renderPlot({
    
    score_data <- switch(input$score, 
                   "PerfScoreID" = complete_data$PerfScoreID,
                   "Satisfaction" = complete_data$EmpSatisfaction,
                   "Salary" = complete_data$Salary)
    
    lab_x = 'Department'
    lab_y = 'Position'
    
    ggplot(complete_data, aes(x=Department, y=Position, fill=score_data)) + 
      geom_tile() +
      labs(x = lab_x, y = lab_y, fill =input$score) +
      theme(axis.text = element_text(size=15),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.title = element_text(size=20),
            strip.text = element_text(size = 20),
            legend.key.size = unit(1, 'cm'),
            legend.title = element_text(size=20),
            legend.text = element_text(size=15))
  })
}

shinyApp(ui = ui, server = server)

