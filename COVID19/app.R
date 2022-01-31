library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(shinythemes)
library(shinydashboard)


cleanData_df <- read_csv("/Users/annyrodriguez/Documents/analyze_COVID_delta/COVID19/cleaned1_CDC_COVID_data_20220122.csv")
county_char <- c("Miami-Dade", "Broward", "Palm Beach")
cbbPalette <- c(
    "#081E3F", "#B6862C", "#CC0066", "#00FFFF", "#FFCC00", "#000000"
)

ui <- fluidPage(
    theme = shinytheme("yeti"),
    titlePanel("COVID-19 Informational Graphs - Miami-Dade, Broward, and Palm Beach Counties"),
    hr(style="border-color: #081E3F;"),
    sidebarLayout(
        sidebarPanel(checkboxGroupInput("vars1", label = "County",
             choices = county_char,
             selected = ("Miami-Dade"),
             width = 12)),
        
    # plot panel     
    mainPanel(

            # tab layout with click option
            tabsetPanel(
                tabPanel("Average Cases",plotOutput('AveCases', click = "plot_click"), verbatimTextOutput("info")),
                tabPanel("Proportion Positive", plotOutput('PropPos', click = "plot_click"), verbatimTextOutput("info1")),
                tabPanel("Hospitalizations", plotOutput("Hosp", click = "plot_click"), verbatimTextOutput("info2")),
                tabPanel("Fully Vaccinated Proportion", plotOutput("VaxxedTotal", click = "plot_click"), verbatimTextOutput("info3")),
                tabPanel("Eligible Vaccinated Proportion", plotOutput("VaxxedElig", click = "plot_click"), verbatimTextOutput("info4")),
                tabPanel("65+ Vaccinated Proportion", plotOutput("Vaxxed65", click = "plot_click"), verbatimTextOutput("info5")),
                tabPanel("12-17 Vaccinated Proportion", plotOutput("Vaxxed1217", click = "plot_click"), verbatimTextOutput("info6"))
                )
                         
    ))
)
server <- function(input, output){

    ## Average Cases    
    output$AveCases <- renderPlot ({
        AveCases <- 
            ggplot(
            data = cleanData_df %>% 
                filter(County == input$vars1) %>% 
                filter(Date >= ymd("20210101")) %>%
                mutate(aveCases = `Cases - last 7 days` / 7),
            aes(
                x = Date,
                y = aveCases,
                group = County,
                colour = County
            )) +
            theme_bw() +
            theme(legend.position = "bottom") +
            labs(
                title = "COVID-19 Cases",
                x = "Average No. Cases per Day: 7-Day Rolling Window",
                y = "Count of COVID-19 Cases"
            ) + 
            scale_colour_manual(values = cbbPalette) + 
            geom_line(size = 1)
        
AveCases
    })

output$info <- renderText({
    paste0("Date:", as_date(input$plot_click$x), "\nCount of Covid Cases:", input$plot_click$y)
    
    })

    ## Proportion Positive
    output$PropPos <- renderPlot({
    PropPos <- 
        ggplot(
            data = cleanData_df %>% 
                filter(County == input$vars1) %>% 
                filter(Date >= ymd("20210101")) %>% 
                rename(
                    `Proportion of Positive Tests - last 7 days` = 
                        `Positivity Rate` 
                )
        ) +
        theme_bw() +
        theme(legend.position = "bottom") +
        aes(
            x = Date,
            y = `Proportion of Positive Tests - last 7 days`,
            group = County,
            colour = County
        ) +
        labs(
            title = "Proportion of Positive COVID-19 Tests"
        ) + 
        scale_colour_manual(values = cbbPalette) + 
        geom_line(size = 1)
    
    PropPos
        })
    output$info1 <- renderText({
        paste0("Date: ", as_date(input$plot_click$x), "\nProportion of Positive Tests - last 7 days: ", input$plot_click$y)
       
    })
    
output$Hosp <- renderPlot({

    ## Hospitalizations
    Hosp <- 
    ggplot(
        data = cleanData_df %>% 
            filter(County == input$vars1) %>% 
            filter(Date >= ymd("20210101")) %>% 
            mutate(
                `COVID-19 Patients - last 7 days` = 
                    `% inpatient beds occupied by COVID-19 patient` *
                    `Total inpatient beds among hospitals reporting - last 7 days`
            )
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    aes(
        x = Date,
        y = `COVID-19 Patients - last 7 days`,
        group = County,
        colour = County
    ) +
    labs(
        title = "Count of COVID-19 Hospitalizations"
    ) + 
    scale_colour_manual(values = cbbPalette) + 
        geom_line(size = 1)

Hosp
        })
output$info2 <- renderText({
    paste0("Date: ", as_date(input$plot_click$x), "\nCOVID-19 Patients - last 7 days: ", input$plot_click$y)
    
})

## Fully Vaccinated Proportion
output$VaxxedTotal <- renderPlot({
    VaxxedTotal <- 
        ggplot(
            data = cleanData_df %>% 
                filter(County == input$vars1) %>% 
                filter(Date >= ymd("20210401")) 
        ) +
        theme_bw() +
        theme(legend.position = "bottom") +
        aes(
            x = Date,
            y = `Proportion Vaccinated, Total`,
            group = County,
            colour = County
        ) +
        labs(
            title = "Fully-Vaccinated Proportion"
        ) + 
        scale_y_continuous(limits = c(0, 0.8)) +
        scale_colour_manual(values = cbbPalette) + 
        geom_line(size = 1)
    
    VaxxedTotal
})
output$info3 <- renderText({
    paste0("Date: ", as_date(input$plot_click$x), "\nProportion Vaccinated, Total: ", input$plot_click$y)
    
})

## Vaccination of those eligible
output$VaxxedElig <- renderPlot({
    VaxxedElig <- 
        ggplot(
            data = cleanData_df %>% 
                filter(County == input$vars1) %>% 
                filter(Date >= ymd("20210401")) 
        ) +
        theme_bw() +
        theme(legend.position = "bottom") +
        aes(
            x = Date,
            y = `Proportion Vaccinated of Eligible Population`,
            group = County,
            colour = County
        ) +
        labs(
            title = "Eligible Vaccinated Proportion"
        ) + 
        scale_y_continuous(limits = c(0, 1), breaks = 0:10/10) +
        scale_colour_manual(values = cbbPalette) + 
        geom_line(size = 1)
    
    VaxxedElig
})
output$info4 <- renderText({
    paste0("Date: ", as_date(input$plot_click$x), "\nProportion Vaccinated of Eligible Population: ", input$plot_click$y)
    
})

## 65+ Vaxxed out of 65+ Population 
output$Vaxxed65 <- renderPlot({
    Vaxxed65 <- 
        ggplot(
            data = cleanData_df %>% 
                filter(County == input$vars1) %>% 
                filter(Date >= ymd("20210401")) 
        ) +
        theme_bw() +
        theme(legend.position = "bottom") +
        aes(
            x = Date,
            y = `Proportion Vaccinated, 65+`,
            group = County,
            colour = County
        ) +
        labs(
            title = "65+ Vaccinated Proportion"
        ) + 
        scale_y_continuous(limits = c(0, 1), breaks = 0:10/10) +
        scale_colour_manual(values = cbbPalette) + 
        geom_line(size = 1)
    
    Vaxxed65
})
output$info5 <- renderText({
    paste0("Date: ", as_date(input$plot_click$x), "\nProportion Vaccinated, 65+: ", input$plot_click$y)
    
})

## 12-17 Vaxxed out of 12-17 Population
output$Vaxxed1217 <- renderPlot({
    Vaxxed1217 <- 
        ggplot(
            data = cleanData_df %>% 
                filter(County == input$vars1) %>% 
                filter(Date >= ymd("20210401")) 
        ) +
        theme_bw() +
        theme(legend.position = "bottom") +
        aes(
            x = Date,
            y = `Proportion Vaccinated, 12-17`,
            group = County,
            colour = County
        ) +
        labs(
            title = "12-17 Vaccinated Proportion"
        ) + 
        scale_y_continuous(limits = c(0, 1), breaks = 0:10/10) +
        scale_colour_manual(values = cbbPalette) + 
        geom_line(size = 1)
    
    Vaxxed1217 
})
output$info6 <- renderText({
    paste0("Date: ", as_date(input$plot_click$x), "\nProportion Vaccinated, 12-17: ", input$plot_click$y)
    
})

}

shinyApp (ui = ui, server = server)

