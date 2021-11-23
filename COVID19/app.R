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
cleanData3_df <- read_csv("~/Documents/analyze_COVID_delta/COVID19/cleaned_CDC_COVID_data_20211114.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19 Average Cases"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        radioButtons("vars1", label = "County",
             choices = list("Miami-Dade","Broward", "Palm Beach"),
             selected = ("Miami-Dade")),
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("scatter")
    )),
# Define server logic required to draw a scatterplot
server <- function(input, output) {
    
    output$scatter <- renderPlot({
        scatter <- ggplot(
            data = cleanData3_df ,
            aes(
                x = Date,
                y = `Proportion of Positive Tests - last 7 days`,
                color = !!as.name(c"County"),
                group = !!as.name("County")
            )) +
            geom_point(vars = input$vars)
        
        if (input$fit_line == TRUE) {
            scatter <- scatter + geom_smooth(model = "lm")
        }
        scatter
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

