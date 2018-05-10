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
str(books_response, max.level = 1)

books_df <- books_response$books
class(books_df)

books_df <- books_df %>% select(rank, rank_last_week, weeks_on_list, title, author, publisher, primary_isbn10)
names(books_df) <- c("Rank", "Last", "Weeks", "Title", "Author", "Publisher", "ISBN10")

pubfreq <- table(books_df$Publisher)

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
                  choices=c("combined-print-and-e-book-fiction")),
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
    barplot(pubfreq, 
            main=input$list,
            horiz = TRUE,
            las = 1,
            cex.names=.4,
            ylab="Number of Books on Top 15",
            xlab="Publisher")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

