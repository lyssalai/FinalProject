library(knitr)
library(jsonlite)
library(dplyr)
library(httr)
library(tidyverse)
library(stringr)
library(xml2)
library(viridis)
library(knitr)
library(plyr)

opts_chunk$set(cache = TRUE, warning = FALSE, message = FALSE)

##Setting Up NYT Bestsellers API and Functions
api_key <- 'd79f5372049f45ee9913309ba04e9329'

books_request <- function(list_name, params = c(NA)){
  books_query <- paste0('http://api.nytimes.com/svc/books/v3/lists/', list_name, '?api-key=', api_key)
  if (length(na.exclude(params)) > 0){
    books_query <- paste0(books_query, '&', paste(params, collapse = '&'))
  }
  fromJSON(books_query)
}

##Retrieving NYT Bestsellers List
list_names <- books_request('names')$results

books_response <- books_request('combined-print-and-e-book-fiction')$results
books_df <- books_response$books
books_df <- books_df %>% select(rank, weeks_on_list, title, author, publisher, primary_isbn10)
names(books_df) <- c("Rank", "Weeks", "Title", "Author", "Publisher", "ISBN10")
pubfreq <- table(books_df$Publisher)

books_response1 <- books_request('trade-fiction-paperback')$results
books_df1 <- books_response1$books
books_df1 <- books_df1 %>% select(rank, weeks_on_list, title, author, publisher, primary_isbn10)
names(books_df1) <- c("Rank", "Weeks", "Title", "Author", "Publisher", "ISBN10")
pubfreq1 <- table(books_df1$Publisher)

books_response2 <- books_request('hardcover-fiction')$results
books_df2 <- books_response2$books
books_df2 <- books_df2 %>% select(rank, weeks_on_list, title, author, publisher, primary_isbn10)
names(books_df2) <- c("Rank", "Weeks", "Title", "Author", "Publisher", "ISBN10")
pubfreq2 <- table(books_df2$Publisher)

books_response3 <- books_request('mass-market-paperback')$results
books_df3 <- books_response3$books
books_df3 <- books_df3 %>% select(rank, weeks_on_list, title, author, publisher, primary_isbn10)
names(books_df3) <- c("Rank", "Weeks", "Title", "Author", "Publisher", "ISBN10")
pubfreq3 <- table(books_df3$Publisher)

books_response4 <- books_request('combined-print-fiction')$results
books_df4 <- books_response4$books
books_df4 <- books_df4 %>% select(rank, weeks_on_list, title, author, publisher, primary_isbn10)
names(books_df4) <- c("Rank", "Weeks", "Title", "Author", "Publisher", "ISBN10")
pubfreq4 <- table(books_df4$Publisher)

books_response5 <- books_request('audio-fiction')$results
books_df5 <- books_response5$books
books_df5 <- books_df5 %>% select(rank, weeks_on_list, title, author, publisher, primary_isbn10)
names(books_df5) <- c("Rank", "Weeks", "Title", "Author", "Publisher", "ISBN10")
pubfreq5 <- table(books_df5$Publisher)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Publishers on NYT Bestsellers List"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("list", "Lists:", 
                  choices=c("Combined Print and E-book Fiction",
                            "Paperback Trade Fiction",
                            "Hardcover Fiction",
                            "Mass Market Paperback",
                            "Combined Hardcover and Paperback Fiction",
                            "Audio Fiction")),
      hr(),
      helpText("Data from New York Times Bestseller's API")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("bookPlot")  
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$bookPlot <- renderPlot({
    
    # Render a barplot
    if(input$list == "Combined Print and E-book Fiction"){
      barplot(pubfreq, 
            main=input$list,
            horiz = TRUE,
            las = 1,
            cex.names=.4,
            ylab="Number of Books on Top 15",
            xlab="Publisher")
    }
    
    if(input$list == "Paperback Trade Fiction"){
      barplot(pubfreq1, 
              main=input$list,
              horiz = TRUE,
              las = 1,
              cex.names=.4,
              ylab="Number of Books on Top 15",
              xlab="Publisher")
    }
    
    if(input$list == "Hardcover Fiction"){
      barplot(pubfreq2, 
              main=input$list,
              horiz = TRUE,
              las = 1,
              cex.names=.4,
              ylab="Number of Books on Top 15",
              xlab="Publisher")
    }
    
    if(input$list == "Mass Market Paperback"){
      barplot(pubfreq3, 
              main=input$list,
              horiz = TRUE,
              las = 1,
              cex.names=.4,
              ylab="Number of Books on Top 15",
              xlab="Publisher")
    }
    
    if(input$list == "Combined Hardcover and Paperback Fiction"){
      barplot(pubfreq4, 
              main=input$list,
              horiz = TRUE,
              las = 1,
              cex.names=.4,
              ylab="Number of Books on Top 15",
              xlab="Publisher")
    }
    
    if(input$list == "Audio Fiction"){
      barplot(pubfreq5, 
              main=input$list,
              horiz = TRUE,
              las = 1,
              cex.names=.4,
              ylab="Number of Books on Top 15",
              xlab="Publisher")
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

