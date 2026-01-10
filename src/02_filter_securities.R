library(here)
source(here("config", "params.R"))  # min_quotes_threshold


raw_quotes <- readr::read_rds(here("data", "processed", "raw_quotes.rds"))
moex_tickers <- names(raw_quotes)[sapply(raw_quotes, nrow) > min_quotes_threshold]

sentiment_tickers <- names(readr::read_rds(here("data", "processed", "raw_sentiment.rds")))[-1]

selected_tickers <- intersect(moex_tickers, sentiment_tickers)
readr::write_rds(selected_tickers,
                 here("data", "processed", "selected_tickers"),
                 compress = "gz")
