# ui.R
source("dependencies.R")  # Ensure required packages are loaded
library(shiny)

shinyUI(
  fluidPage(
    titlePanel("US Airlines Tweet Sentiment Analysis"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV File",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        selectInput("selected_airline", "Select Airline:", choices = "All", selected = "All"),
        actionButton("update", "Update Analysis"),
        br(), br(),
        helpText("Upload a CSV file with columns 'airline' and 'text'.",
                 "This app analyzes a corpus of tweets tagging major US airlines,",
                 "providing sentiment distributions, word clouds, and business insights.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Sentiment Overview", 
                   plotOutput("sentimentPlot"),
                   br(),
                   DT::dataTableOutput("summaryTable")),
          tabPanel("Word Cloud", 
                   plotOutput("wordCloudPlot")),
          tabPanel("Insights", 
                   verbatimTextOutput("insights"))
        )
      )
    )
  )
)
