
get_odds_table <- function(bet365_market_odds, betfair_market_odds) {
  
  odds_table <- 
    bet365_market_odds %>% 
    inner_join(betfair_market_odds, by = c("name", "start_time")) %>% 
    distinct() %>% 
    select(name, start_time, bet365, betfair) %>% 
    mutate(ratio = betfair/bet365) %>% 
    arrange(ratio)
  
  return(odds_table)
  
}