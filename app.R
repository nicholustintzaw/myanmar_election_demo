################################################################################

library(tidyverse)
library(shiny)
# library(plotly)
library(ggplot2)
# library(scales)
# library(sf)
library(shinydashboard)
library(rsconnect)
library(DT)
# library(caret)
# library(textdata)
# library(caret)
# library(eList)
library(SnowballC)
library(tm)
library(topicmodels)
library(tmap)
library(reshape2)

################################################################################

# https://nicholustintzaw.shinyapps.io/shiny_dashboard/
# as we using the large dataset - need to wait some time or reload the web for a while
# when selecting different indicator - that is part of the shinyapp.io website 
# performance and we control with whatever parameter available. But, this require
# advance space of memory and need additional subscription at shinapp.io
# Therefore, for this final exercise, we just left with the current version with that limitation. 
# There is no error in local shiny app as it share memory from the local PC. 


# load data
load("df_all_pyithu_map.rda")
load("df_news_sentiment.rda")
load("df_news_final.rda")

################################################################################
### Election Data Preparation ###
################################################################################

# modify variable name for display purpose
df_all_pyithu_map <- df_all_pyithu_map %>%
  rename("Voter Turnout Rate" = "share_voting", 
         "% of lost vote" = "share_lost", 
         "% of invalid vote" = "share_invalid")

state_list <- unique(df_all_pyithu_map$ST)
state_list <- append(state_list, "All States & Regions")

indicator_list <- c("Voter Turnout Rate", "% of lost vote", "% of invalid vote")

year_list <- c(2015, 2020)

# map function 
tidy_tmmap <- function(df, report_var){
  
  tm_shape(df) + 
    tm_polygons(id = "TS",
                col = report_var,
                popup.vars = c("TS", report_var),
                alpha = 0.5) +
    tm_layout(legend.outside = TRUE)
}


################################################################################
### prepare for topic modeling ###
################################################################################

tidy_docmatrix <- function(df, input_year){
  # select subset of data
  df <- df %>% filter(year == input_year)
  # prepare corpus
  Corpus <- VCorpus(VectorSource(df$text_list))
  # text cleaning
  Corpus <- Corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(stemDocument, language="english") %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace) 
  # transform doc matrix
  dtm <- DocumentTermMatrix(Corpus)
  rowTotals <- apply(dtm , 1, sum)
  dtm.new   <- dtm[rowTotals> 0, ]
  
  return(dtm.new)
  
}

dtm.new.2015 <- tidy_docmatrix(df_news_final, 2015)
dtm.new.2020 <- tidy_docmatrix(df_news_final, 2020)

# Model function
topic_modeling <- function(input_dtm, k){
  
  lda2 <- LDA(input_dtm, k = k, method = "Gibbs", control = list(burnin = 100, iter = 1000))
  
  topics2 <- tidytext::tidy(lda2, matrix = "beta") # matrix of betas (per-topic-per-word probabilities)
  topwords2 <- topics2 %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>% # print the words with the highest beta from each topic
    mutate(k = k, 
           beta = round(beta, 2)) %>%
    select(topic, term, beta)
  
  return(topwords2)
}

################################################################################
### UI ###
################################################################################
ui <- dashboardPage(
  dashboardHeader(title = "Myanmar Election Dashboard"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Election Dashboard", tabName = "dashboard", icon = icon("dashboard")), 
      menuItem("Text Analysis", tabName = "text", icon = icon("list-alt")),
      menuItem("Topic Modeling", tabName = "topic", icon = icon("table"))
      )
    
  ), 
  dashboardBody(
    tabItems(
      
      # first tab content 
      tabItem(tabName = "dashboard", 
              fluidRow(
                column(width = 4, 
                       align = "center",
                       selectInput("year", "select the year",
                                   choices = year_list)),
                column(width = 4, 
                       align = "center", 
                       selectInput("state", "select the state", 
                                   choices = state_list, 
                                   selected = "All States & Regions")), 
                column(width = 4, 
                       align = "center",
                       selectInput("indicator", "select the indicator", 
                                   choices = indicator_list))
                ), 
              fluidRow(
                box(tmapOutput("pyithu", height = 800)), 
                box(dataTableOutput("table"))
              ), 
              fluidRow(
                HTML("<p>Data Source: <a href='https://www.uec.gov.mm/'> Union Election Commission Myanmar</a> ,  <a href='http://www.themimu.info/census-data'> Myanmar Information Management Unit</a>, and the raw-data election we used for this project was shared by Jiwon Lee (Stanford University), and it was part of her graduate study dissertation project.</p>")
                )
      ), 
      # second tab content 
      tabItem(tabName = "text", 
              fluidRow(
                box(plotOutput("sentiment", height = 500, width = 1000)
                    )
              ), 
              fluidRow(
                column(width = 8, 
                       align = "left", 
                       sliderInput("count", "Select the number of top words", 
                                   value = 15,
                                   min = 10, 
                                   max = 50))
              ), 
              fluidRow(
                box(plotOutput("topwords", height = 500, width = 1000)
                )
              )
      ), 
      # third tab content 
      tabItem(tabName = "topic", 
              fluidRow(
                column(width = 12, 
                       align = "center", 
                       sliderInput("k", "Select the number of topic", 
                                   value = 3,
                                   min = 1, 
                                   max = 10))
              ), 
              fluidRow(
                h5("This is the topic modeling session. Please select the number of topics you want to observe from the election news text data we used in the sentiment analysis. The results will be display the top ten words per each topics which have higher proberbility in association with the topic. The column `beta` represent the probability. This will not literately tell what is the topic covered in the news we used in analysis. But, user can easily guess what the topic might be based on the top ten words provided by the model."), 
                box(dataTableOutput("topic_2015")),
                box(dataTableOutput("topic_2020"))
                )
              )
      )
    )
)

################################################################################
### SERVER ###
################################################################################

server <- function(input, output){
  
  # for election results
  df <- df_all_pyithu_map
  
  df_nomap <- df %>% 
    as.data.frame() %>%
    select(year, ST, TS, num_party, share_urban_pop, share_lit_pop)

  data_all <- reactive({filter(df, year == input$year)})
  
  data_nomap_all <- reactive({filter(df_nomap, year == input$year)})
  
  data <- reactive({
    filter(df, year == input$year & ST == input$state)
  })
  
  data_table <- reactive({
    filter(df_nomap, year == input$year & ST == input$state)
  })
  

  output$pyithu <- renderTmap({
    
    if(input$state != "All States & Regions"){
      if(input$indicator == "Voter Turnout Rate"){
        
        tidy_tmmap(data(), "Voter Turnout Rate")
        
      } else if(input$indicator == "% of lost vote"){
        
        tidy_tmmap(data(), "% of lost vote")
        
      } else{
        
        tidy_tmmap(data(), "% of invalid vote")
        
      }
      
      
    } else{
      if(input$indicator == "Voter Turnout Rate"){
        
        tidy_tmmap(data_all(), "Voter Turnout Rate")
        
      } else if(input$indicator == "% of lost vote"){
        
        tidy_tmmap(data_all(), "% of lost vote")
        
      } else{
        
        tidy_tmmap(data_all(), "% of invalid vote")
        
      }
      
    }
  }
  )
  
  
  output$table <- renderDataTable({
    
    if(input$state != "All States & Regions"){
      subset_table <- data_table() %>% 
        select(TS, num_party, share_urban_pop, share_lit_pop) %>%
        rename("Township" = TS, 
               "Number of Party" = num_party, 
               "Urban Pop. Shared" = share_urban_pop, 
               "Liteate Pop. Shared" = share_lit_pop)
      
      datatable(subset_table, caption = "Township Characteristics (Selected)")
    }else{
      subset_table <- data_nomap_all() %>% 
        select(ST, num_party, share_urban_pop, share_lit_pop) %>%
        group_by(ST) %>%
        summarise(
          num_party = round(sum(num_party, na.rm = TRUE), 1), 
          share_urban_pop = round(mean(share_urban_pop, na.rm = TRUE), 1), 
          share_lit_pop = round(mean(share_lit_pop, na.rm = TRUE), 1)
        ) %>%
        rename("State/Region" = ST, 
               "Number of Party" = num_party, 
               "Average Urban Pop. Shared" = share_urban_pop, 
               "Average Liteate Pop. Shared" = share_lit_pop)
      
      datatable(subset_table, caption = "State/Region Characteristics (Selected)")
    }

  }
  )
  
  # for news_sentiment
  sentimen <- reactive({df_news_sentiment}) 
  
  output$sentiment <- renderPlot({
    sentiment_plt <-  sentimen() %>% 
      mutate(source = toupper(source)) %>%
      group_by(source, year, word_tokens, sentiment) %>% 
      count(sort = TRUE) %>% 
      mutate(bing_value = ifelse(sentiment == "positive", 1, -1)) %>%
      filter(!is.na(sentiment)) %>% 
      ggplot(aes(n * bing_value, source, fill = n * bing_value > 0))+
      geom_col(show.legend = FALSE)+
      labs(title = "Myanmar Election News Sentiment",
           x = "Sentiment Value",
           y = "News Source")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_fill_manual(values = c("red", "blue"))+
      facet_wrap(~year) +
      theme(legend.position = "none")
    
    plot(sentiment_plt)
  })
  
  output$topwords <- renderPlot({
    topwords_plt <-  sentimen() %>% 
      mutate(source = toupper(source)) %>%
      group_by(source, year, word_tokens) %>% 
      count(sort = TRUE) %>% 
      filter(n > input$count) %>%
      ggplot(aes(word_tokens, n, fill = word_tokens))+
      geom_bar(stat = "identity") +
      facet_wrap(~ source, scales = "free") +
      coord_flip() +
      labs(title = "Frequenty Reported Words by News Source",
           x = "Words",
           y = "Frequency") +
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5))
      
    
    plot(topwords_plt)
  })
  
  # for topic modeling
  dtm_topic_2015 <- reactive({
    topic_modeling(dtm.new.2015, 
                   input$k)
  })
  
  dtm_topic_2020 <- reactive({
    topic_modeling(dtm.new.2020, 
                   input$k)
  })

  
  output$topic_2015 <- renderDataTable({
    
    topic_2015 <- dtm_topic_2015()
    
    datatable(topic_2015, caption = "Top 10 words associate to topics - 2015 Election news")
    
  })
  
  output$topic_2020 <- renderDataTable({
    
    topic_2020 <- dtm_topic_2020()
    
    datatable(topic_2020, caption = "Top 10 words associate to topics - 2020 Election news")
    
  })
  
}

shinyApp(ui = ui, server = server)
