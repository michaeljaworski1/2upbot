# odds api functions

get_event_ids <- function() {
  
  competitions <- c("a-league", "english-premier-league", "champions-league")
  
  event_ids <-
    map(
      .x = competitions,
      .f = function(x) {
        glue::glue("https://www.odds.com.au/sport/soccer/{x}/matches/") %>% 
          xml2::read_html() %>% 
          rvest::html_text() %>% 
          str_extract_all("(?<=firstEventId:)\\d{6}") %>% 
          unlist()
      }
    ) %>% 
    reduce(c)
  
  return(event_ids)
  
}

# event lookup
get_event <- function(event_id) {
  
  event <-
    glue::glue("https://puntapi.com/odds/event/{event_id}?betTypes=fixed-win") %>% 
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
    mutate(selection_id = as.character(selection_id)) %>% 
    as_tibble()
  
  return(event) 
  
}

possibly_get_event <-
  possibly(
    .f = get_event,
    otherwise = NULL 
  )

# selection lookup
selection_lookup <- function(event_id) { 
  
  event <-
    glue::glue("https://www.punters.com.au/api/web/public/Odds/getOddsComparisonCacheable/?allowGet=true&APIKey=65d5a3e79fcd603b3845f0dc7c2437f0&eventId={event_id}&betType=FixedWin") %>%
    httr::GET() %>% 
    purrr::pluck("content") %>% 
    rawToChar() %>% 
    jsonlite::fromJSON()
  
  selections <-
    event %>% 
    pluck("selections") %>% 
    select(selectionId, name) %>%
    mutate(
      eventDesc = event$eventDesc,
      startTime = (event$startTime %>% lubridate::as_datetime() + lubridate::hours(11)) %>% lubridate::as_date()
      ) %>% 
    janitor::clean_names() %>% 
    filter(str_detect(event_desc, "Win-Draw-Win")) %>% 
    select(-event_desc) %>% 
    as_tibble()
    
  return(selections)
  
}

# update market odds 
update_bet365_market_odds <- function(event_ids) {
  
  events <-
    map_dfr(
      .x = event_ids, 
      .f = possibly_get_event
    )
  
  selections <-
    purrr::map_dfr(
      .x = event_ids,
      .f = selection_lookup
    )
  
  bet365_market_odds <- 
    events %>% 
    left_join(selections, by = c("selection_id")) %>% 
    select(-selection_id)
  
  return(bet365_market_odds)
  
}
