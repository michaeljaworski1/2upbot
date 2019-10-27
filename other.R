webhook <- "https://hooks.slack.com/services/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"
token <- "xoxp-810392033364-810855719056-810866454565-57791bb5b23a8fec293b0e12601d7c36"

slackr_setup(
  channel = "#arbing",
  username = "michael.jaworski.au@gmail.com",
  incoming_webhook_url = webhook,
  api_token = token
)

get_current <- function() {
  current <- 
    bet365_epl_market_odds %>% 
    select(-selection_id) %>% 
    filter(lubridate::now() + lubridate::hours(24) > start_time) %>% 
    inner_join(betfair_epl_market_odds, by = "name") %>% 
    mutate(ratio = betfair/bet365) %>% 
    arrange(ratio)
  
  return(current)
}
