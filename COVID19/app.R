#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(Stat2Data)
cleanData3_df <- read_csv("~/Documents/analyze_COVID_delta/data_clean/cleaned2_CDC_COVID_data_20211109.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19 Graphs"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("vars1", label = "Date",
                         choices = list("GPA","HSGPA", "SATV", "SATM", "Male", "HU", "SS", "FirstGen", "White", "CollegeBound"),
                         selected = "HSGPA")
        ),
        radioButtons("vars2", label = "Variable 2:",
                     choices = list("GPA","HSGPA", "SATV", "SATM", "Male", "HU", "SS", "FirstGen", "White", "CollegeBound"),
                     selected = "HSGPA")
    ),
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("scatter")
    )),

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$scatter <- renderPlot({
        scatter <- ggplot(
            data = FirstYearGPA,
            aes(
                x = !!as.name(input$vars1),
                y = !!as.name(input$vars2)
            )) +
            geom_point(vars = input$vars)
        
        if (input$fit_line == TRUE) {
            scatter <- scatter + geom_smooth(model = "lm")
        }
        scatter
    })
}
# server <- function(input, output) {
# 
# data("FirstYearGPA")
# 
# output$scatter <- renderPlot({
#   scatter <- ggplot(
#     data = FirstYearGPA,
#     aes(
#       x = !!as.name(input$vars1),
#       y = !!as.name(input$vars2)
#     )) +
#     geom_point()
# })
# }

# Run the application 
shinyApp(ui = ui, server = server)

