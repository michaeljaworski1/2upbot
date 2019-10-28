library(tidyverse) 
library(lubridate)
library(abettor)
source("odds_api_functions.R")
source("betfair_api_functions.R")
source("functions.R")

abettor::loginBF(
  username = "michael.jaworski@betfair.com.au",
  password = keyring::key_get("BF", "michael.jaworski@betfair.com.au"),
  applicationKey =  keyring::key_get("BF", "application_key")
)

# setup slack
slackr::slackr_setup(
  channel = "#arbing",
  username = "michael.jaworski.au@gmail.com",
  incoming_webhook_url = "https://hooks.slack.com/services/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",
  api_token = "xoxp-810392033364-810855719056-810866454565-57791bb5b23a8fec293b0e12601d7c36"

)

# setup
betfair_competitions <- get_betfair_competitions()
betfair_markets <- get_betfair_markets(betfair_competitions)
event_ids <- get_event_ids()

bot <- function() {
  
  bet365_market_odds <- update_bet365_market_odds(event_ids) %>% mutate(name = str_replace(name, "Sydney FC", "Sydney"))
  betfair_market_odds <- update_betfair_market_odds(betfair_markets)
  odds_table <- get_odds_table(bet365_market_odds, betfair_market_odds)
  
  if (odds_table %>% pull(ratio) %>% nth(1) <= 1.00) {
    slackr::slackr(pander::pandoc.table(odds_table), channel = "#arbing")
    Sys.sleep(300)
    bot()
  } else {
    print(glue::glue("best ratio is {odds_table %>% pull(ratio) %>% nth(1)}... zzz..."))
    Sys.sleep(30)
    bot()
  }
  
}

bot()
