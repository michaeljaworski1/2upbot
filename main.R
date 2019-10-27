library(slackr)

bet_365_market_odds <- update_bet365_market_odds()
betfair_epl_market_odds <- update_epl_market_odds()
current <- get_current() %>% select(name, start_time, bet365, betfair, ratio)
slackr::slackr(pander::pandoc.table(current), channel = "#arbing")
