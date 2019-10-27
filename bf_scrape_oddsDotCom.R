bf_scrape_oddsDotCom <- function(.code = "gre") {
  
  # ------------------------
  # Check Inputs
  # ------------------------
  
  if (.code == "gre") {
    
    sportId = 21
    eventEndpoint = "https://www.odds.com.au/api/web/public/Meetings/getDataByRangeCacheable/?filter=events&APIKey=65d5a3e79fcd603b3845f0dc7c2437f0&sportId={sportId}&regionId[]=75&regionId[]=112&rangeStart={date_start}&rangeEnd={date_end}"
    
  } else if (.code == "tho") {
    
    sportId = 1
    eventEndpoint = "https://www.odds.com.au/api/web/public/Meetings/getDataByRangeCacheable/?filter=events,regions,meetings&APIKey=65d5a3e79fcd603b3845f0dc7c2437f0&sportId={sportId}&regionId[]=1&regionId[]=22&regionId[]=24&regionId[]=25&regionId[]=26&regionId[]=27&regionId[]=28&regionId[]=29&regionId[]=30&&rangeStart={date_start}&rangeEnd={date_end}"
    
  } else {
    
    stop("Please use .code = 'tho' or 'gre'")
    
  }
  
  # ------------------------
  # Execute
  # ------------------------
  
  date_start = lubridate::today() %>% str_c("T00:00:00.000Z")
  date_end = (lubridate::today() + lubridate::days(1)) %>% str_c("T00:00:00.000Z")
  
  # Get event ids
  event_ids = 
    glue(eventEndpoint) %>%
    httr::GET() %>%
    purrr::pluck("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    purrr::pluck("events") %>%
    pull(id)
  
  extract_odds_event_id = function(event_id) {
    
    #print(event_id)
    
    # Get the full event set from a different endpoint
    full_event_set = 
      glue("https://www.punters.com.au/api/web/public/Odds/getOddsComparisonCacheable/?allowGet=true&APIKey=65d5a3e79fcd603b3845f0dc7c2437f0&eventId={event_id}&betType=FixedWin") %>%
      httr::GET() %>%
      purrr::pluck("content") %>%
      rawToChar() %>%
      fromJSON()
    
    # if (!full_event_set$isGreyhounds) {
    #     return(NULL)   
    # }
    
    # Prices
    selection_prices = 
      glue("https://puntapi.com/odds/event/{event_id}?betTypes=fixed-win") %>% 
      httr::GET() %>%
      purrr::pluck("content") %>%
      rawToChar() %>%
      fromJSON() %>%
      purrr::pluck("odds") %>%
      janitor::clean_names() %>%
      arrange(selection_id) %>%
      filter(bookmaker_id %in% c("bet365", "sportsbet", "beteasy")) %>%
      mutate(
        price = .$price$value
      ) %>%
      select(-type,-bet_type, -market_percentage, -market_percentage_movement) %>%
      spread(bookmaker_id, price) %>%
      as_tibble() %>%
      mutate(selection_id = as.character(selection_id))
    
    # Selection lookup
    selection_lookup = 
      full_event_set %>%
      purrr::pluck("selections") %>%
      select(selectionId, name) %>%
      janitor::clean_names() %>%
      as_tibble()
    
    # Add Event Metadata
    draft_output = 
      selection_lookup %>%
      inner_join(selection_prices, by = "selection_id") %>%
      mutate(
        event_id = full_event_set$eventId,
        event_name =  full_event_set$eventName,
        start_time = full_event_set$startTime
      )
    
    # Rework the data into the old format
    draft_output %>%
      mutate(
        race_number = event_name %>% str_extract("(?<=R)\\d+"),
        event_venue = event_name %>% str_remove("R\\d+") %>% str_trim(),
        runner_name = name %>% str_trim() %>% str_to_upper()
      ) %>%
      mutate(
        beteasy_odds = {if ("beteasy" %in% colnames(.)) beteasy else NA_real_},
        bet365_odds = {if ("bet365" %in% colnames(.)) bet365 else NA_real_},
        sportsbet_odds = {if ("sportsbet" %in% colnames(.)) sportsbet else NA_real_}
      ) %>%
      select(event_venue, race_number, bet365_odds, beteasy_odds, sportsbet_odds, runner_name)
    
  }
  
  
  # Run extraction in parallel
  plan(multiprocess)
  
  event_ids %>%
    future_map_dfr(~tryCatch(extract_odds_event_id(.), error = function(e) NULL))
  
}