library(tidyverse) 
library(abettor)
library(lubridate)

abettor::loginBF(
  username = "michael.jaworski@betfair.com.au",
  password = keyring::key_get("BF", "michael.jaworski@betfair.com.au"),
  applicationKey =  keyring::key_get("BF", "application_key")
)

soccer <- abettor::listCompetitions(
  eventTypeIds = 1,  
  toDate = (format(Sys.time() + 86400 * 360, "%Y-%m-%dT%TZ"))
)

epl_completition_id <- 10932509

epl_markets <- abettor::listMarketCatalogue(
  eventTypeIds = 1, 
  marketTypeCodes = "MATCH_ODDS", 
  competitionIds = epl_completition_id, 
  fromDate = (lubridate::now() + minutes(-5)) %>%
    with_tz("UTC") %>%
    format("%Y-%m-%dT%TZ"),
  toDate = (lubridate::now() + days(3)) %>%
    with_tz("UTC") %>%
    format("%Y-%m-%dT%TZ") 
)


epl_market_ids <- epl_markets %>% pull(marketId)

fetch_odds <- function(market_id) {
  
  odds <- abettor::listMarketBook(
    marketIds = market_id, 
    priceData = "EX_BEST_OFFERS"
    )
  
  homeOdds = odds %>%
    pull(runners) %>%
    as.data.frame() %>%
    pull(ex) %>%
    pull(availableToBack) %>%
    first() %>%
    pull(price) %>%
    first()
  
  awayOdds = odds %>%
    pull(runners) %>%
    as.data.frame() %>%
    pull(ex) %>%
    pull(availableToBack) %>%
    nth(2) %>%
    pull(price) %>%
    first()
  
  odds <- tibble(market_id, homeOdds, awayOdds)
  
  return(odds)
}

epl_market_odds <- 
  map_dfr(
    .x = epl_market_ids, 
    .f = fetch_odds
    ) %>% 
  bind_cols(
    epl_markets %>% 
      pull(event) %>%
      select(name)
    ) %>% 
  mutate(
    HomeTeam = str_extract(name, ".*(?= v)"),
    AwayTeam = str_extract(name, "(?<=v ).*")
    ) %>%
  select(HomeTeam, AwayTeam, homeOdds, awayOdds)

betfair_epl_market_odds <-
  epl_market_odds %>% 
  gather(key = "location", value = "name", -homeOdds, -awayOdds) %>% 
  mutate(betfair = ifelse(location == "HomeTeam", homeOdds, awayOdds)) %>% 
  select(name, odds)
