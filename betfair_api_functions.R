# betfair api functions

get_betfair_competitions <- function() {
  
  competitions <- 
    abettor::listCompetitions(
      eventTypeIds = 1,
      toDate = (lubridate::now() + days(7)) %>%
        with_tz("UTC") %>%
        format("%Y-%m-%dT%TZ") 
      ) %>% 
    pluck("competition") %>% 
    filter(
      str_detect(name, "Australian A-League|English Premier League|English Championship") &
      !str_detect(name , "Div 2")
      )
  
  return(competitions)
  
}

get_betfair_markets <- function(competitions) {
  
  markets <- 
    abettor::listMarketCatalogue(
      eventTypeIds = 1, 
      marketTypeCodes = "MATCH_ODDS", 
      competitionIds = competitions %>% pull(id), 
      fromDate = (lubridate::now() + minutes(-5)) %>%
        with_tz("UTC") %>%
        format("%Y-%m-%dT%TZ"),
      toDate = (lubridate::now() + days(7)) %>%
        with_tz("UTC") %>%
        format("%Y-%m-%dT%TZ") 
    )
  
  return(markets)
  
}

get_betfair_odds <- function(marketId) {
  
  odds <- 
    abettor::listMarketBook(
    marketIds = marketId, 
    priceData = "EX_BEST_OFFERS"
    )
  
  home_odds <- 
    odds %>%
    pull(runners) %>%
    as.data.frame() %>%
    pull(ex) %>%
    pull(availableToLay) %>%
    nth(1) %>%
    pull(price) %>%
    first()
  
  away_odds <- 
    odds %>%
    pull(runners) %>%
    as.data.frame() %>%
    pull(ex) %>%
    pull(availableToLay) %>%
    nth(2) %>%
    pull(price) %>%
    first()
  
  odds <- 
    tibble(marketId, home_odds, away_odds)
  
  return(odds)
  
}

update_betfair_market_odds <- function(markets) {
  
  betfair_market_odds <- 
    map_dfr(
      .x = markets %>% pull(marketId), 
      .f = get_betfair_odds
      ) %>% 
    mutate(
      home_team = markets %>% pluck("runners") %>% map_chr(.f = function(x) x %>% pull(runnerName) %>% nth(1)),
      away_team = markets %>% pluck("runners") %>% map_chr(.f = function(x) x %>% pull(runnerName) %>% nth(2)),
      start_time = markets %>% pull(marketStartTime) %>% lubridate::as_date()
    ) %>% 
    select(home_team, away_team, home_odds, away_odds, start_time) %>% 
    gather(key = "location", value = "name", -start_time, -home_odds, -away_odds) %>% 
    mutate(betfair = ifelse(location == "home_team", home_odds, away_odds)) %>% 
    select(name, start_time, betfair)
  
  return(betfair_market_odds) 
  
}
