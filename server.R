# server.R
source("dependencies.R")  # Ensure required packages are loaded
library(shiny)
library(tidyverse)
library(tidytext)
library(DT)
library(wordcloud)
library(RColorBrewer)

shinyServer(function(input, output, session) {

  # Reactive function for reading the uploaded CSV file
  dataInput <- reactive({
    req(input$file)
    tryCatch({
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      NULL
    })
  })
  
  # Process the uploaded data
  tweets <- reactive({
    df <- dataInput()
    req(df)
    # Ensure the CSV file has columns 'airline' and 'text'
    if (!all(c("airline", "text") %in% names(df))) {
      stop("CSV must contain 'airline' and 'text' columns")
    }
    df <- df %>% mutate(tweet_id = row_number())
    df
  })
  
  # Tokenize tweet text into words
  tweet_words <- reactive({
    tweets() %>% unnest_tokens(word, text)
  })
  
  # Calculate sentiment values using the Bing lexicon
  tweet_sentiments <- reactive({
    tweet_words() %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      mutate(sentiment_value = ifelse(sentiment == "positive", 1, -1))
  })
  
  # Compute sentiment score for each tweet
  tweet_sentiment_scores <- reactive({
    tweet_sentiments() %>%
      group_by(tweet_id, airline) %>%
      summarise(sentiment_score = sum(sentiment_value), .groups = "drop")
  })
  
  # Merge sentiment scores back into the tweets data
  processed_tweets <- reactive({
    df <- tweets() %>% left_join(tweet_sentiment_scores(), by = c("tweet_id", "airline"))
    df$sentiment_score[is.na(df$sentiment_score)] <- 0
    df
  })
  
  # Summary table: average sentiment per airline
  airline_summary <- reactive({
    processed_tweets() %>%
      group_by(airline) %>%
      summarise(avg_sentiment = mean(sentiment_score),
                tweet_count = n(),
                .groups = "drop")
  })
  
  # Update the airline selectInput once the file is uploaded
  observe({
    req(processed_tweets())
    updateSelectInput(session, "selected_airline", 
                      choices = c("All", unique(processed_tweets()$airline)),
                      selected = "All")
  })
  
  # Reactive subset based on selected airline
  filtered_data <- reactive({
    req(processed_tweets())
    if (input$selected_airline == "All") {
      processed_tweets()
    } else {
      processed_tweets() %>% filter(airline == input$selected_airline)
    }
  })
  
  # Render sentiment histogram
  output$sentimentPlot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = sentiment_score, fill = airline)) +
      geom_histogram(bins = 20, alpha = 0.7, color = "black") +
      theme_minimal() +
      labs(title = ifelse(input$selected_airline == "All",
                          "Sentiment Distribution for All Airlines",
                          paste("Sentiment Distribution for", input$selected_airline)),
           x = "Sentiment Score", y = "Tweet Count")
  })
  
  # Render summary table of sentiment scores
  output$summaryTable <- DT::renderDataTable({
    req(processed_tweets())
    if (input$selected_airline == "All") {
      DT::datatable(airline_summary(), options = list(pageLength = 5))
    } else {
      data <- filtered_data() %>%
        summarise(avg_sentiment = mean(sentiment_score),
                  tweet_count = n())
      DT::datatable(data, options = list(pageLength = 5))
    }
  })
  
  # Render word cloud based on tweet text
  output$wordCloudPlot <- renderPlot({
    req(filtered_data())
    words <- filtered_data() %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE)
    wordcloud(words = words$word, freq = words$n, min.freq = 1,
              max.words = 100, random.order = FALSE,
              colors = brewer.pal(8, "Dark2"))
  })
  
  # Render business insights
  output$insights <- renderPrint({
    req(processed_tweets())
    if (input$selected_airline == "All") {
      cat("Business Insights - All Airlines:\n")
      cat("Average sentiment scores across airlines:\n")
      print(airline_summary())
      best_airline <- airline_summary() %>% filter(avg_sentiment == max(avg_sentiment))
      worst_airline <- airline_summary() %>% filter(avg_sentiment == min(avg_sentiment))
      cat("\nInsight:\n")
      cat("The airline with the most positive sentiment is", best_airline$airline, 
          "with an average sentiment score of", round(best_airline$avg_sentiment, 2), "\n")
      cat("The airline with the most negative sentiment is", worst_airline$airline, 
          "with an average sentiment score of", round(worst_airline$avg_sentiment, 2), "\n")
      cat("\nThese insights can guide decisions on customer service improvements and targeted marketing strategies.")
    } else {
      data <- filtered_data()
      avg_sent <- round(mean(data$sentiment_score), 2)
      cat("Business Insights for", input$selected_airline, ":\n")
      cat("Average Sentiment Score:", avg_sent, "\n")
      cat("Total Tweets Analyzed:", nrow(data), "\n")
      cat("Examine the word cloud for frequently used terms that may indicate key areas of customer concern or satisfaction.")
    }
  })
})
