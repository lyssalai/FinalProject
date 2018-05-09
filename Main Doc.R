library(knitr)
library(jsonlite)
library(dplyr)
library(httr)
library(tidyverse)
library(stringr)
library(xml2)
library(viridis)
library(knitr)
library(ggplot)
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
barplot(pubfreq,
        horiz = TRUE,
        las = 1,
        cex.names=.4,
        main = "Publishers on Top 15 NYT Bestseller's List",
        xlab = "Frequency",
        col = c("red"))

rankweek <- as.matrix(select(books_df, Weeks, Rank))
plot(rankweek,
     main = "Rank of Title VS Number of Weeks on Bestseller's List",
     pch = 20)
dfrankweek <- as.data.frame(rankweek)
rankweekLM <- lm(Rank ~ Weeks, data = dfrankweek)
summary(rankweekLM)