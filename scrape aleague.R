library(tidyverse)


html <- xml2::read_html("https://www.odds.com.au/sport/soccer/a-league/matches/")
text <- rvest::html_text(html)


event_ids <- str_extract_all(text %>% as.character(), "(?<=firstEventId:)\\d{6}") %>% unlist()
event_url <- glue::glue("https://puntapi.com/odds/event/{event_id}?betTypes=fixed-win,tote-win,exchange-win,exchange-win-lay")


possibly_get_event <- 
  purrr::possibly(
    .f = function(x) {
    
      dfr <- 
        glue::glue("https://puntapi.com/odds/event/{x}?betTypes=fixed-win") %>% 
        httr::GET() %>%
        purrr::pluck("content") %>%
        rawToChar() %>%
        jsonlite::fromJSON() %>%
        purrr::pluck("odds") %>%
        janitor::clean_names() %>%
        arrange(selection_id) %>%
        filter(bookmaker_id %in% c("bet365")) %>%
        mutate(price = .$price$value) %>%
        select(-type, -bet_type, -market_percentage, -market_percentage_movement) %>%
        spread(bookmaker_id, price) %>%
        as_tibble() %>%
        mutate(selection_id = as.character(selection_id))
    
      return(dfr)
  
      },
    
    otherwise = NULL
    
    )

events <-
  map_dfr(
    .x = event_ids, 
    .f = function(x) possibly_get_event(x)
  )

selection_ids <- events %>% pull(selection_id)

map_dfr(
  .x = selection_ids, 
  .f = function(x) {
    
    selection_url <- 
      glue::glue("https://puntapi.com/matcher-fox/sports/a-league/selection/{x}/ids")
    
    dfr <- 
      selection_url %>% 
      httr::GET() %>% 
      purrr::pluck("content") %>% 
      rawToChar() %>% 
      jsonlite::fromJSON() %>% 
      as.data.frame()
    
    return(dfr)
    
  }
)
