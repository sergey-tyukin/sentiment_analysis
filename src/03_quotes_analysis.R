library(writexl)
library(here)


get_stats <- function(asset) {
  asset_ret <- na.omit(asset$return)

  list(
    n_obs = length(asset_ret),
    median  = median(asset_ret),
    mean    = mean(asset_ret),
    min     = min(asset_ret),
    max     = max(asset_ret),
    skewness = moments::skewness(asset_ret),
    kurtosis = moments::kurtosis(asset_ret),
    volatility = sd(asset_ret)  # sd() использует формулу с (n-1)
  )
}

raw_quotes <- readr::read_rds(here("data", "processed", "raw_quotes.rds"))
securities_stats <- purrr::map_dfr(raw_quotes, get_stats, .id = "ticker")

write_xlsx(securities_stats, here("output", "tables", "Securities_stats.xlsx"))
