library(writexl)
library(here)


get_stats <- function(asset) {
  asset_ret <- na.omit(asset$ret)

  list(
    n_obs = length(asset_ret),
    median  = median(asset_ret),
    mean    = mean(asset_ret),
    min     = min(asset_ret),
    max     = max(asset_ret),
    skewness = moments::skewness(asset_ret),
    kurtosis = moments::kurtosis(asset_ret),
    volatility = sd(asset_ret),  # sd() использует формулу с (n-1)
    spread_bbo = median(asset$spread_bbo, na.rm = TRUE),
    spread_lv10 = median(asset$spread_lv10, na.rm = TRUE),
    spread_1mio = median(asset$spread_1mio, na.rm = TRUE)
  )
}

raw_quotes_spread <- readr::read_rds(here("data", "processed", "raw_quotes_spread.rds"))
securities_stats <- purrr::map_dfr(raw_quotes_spread, get_stats, .id = "ticker")

write_xlsx(securities_stats, here("output", "tables", "Securities_stats.xlsx"))
