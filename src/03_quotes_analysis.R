library(writexl)
library(dplyr)
library(here)


get_stats <- function(asset) {
  asset_ret <- na.omit(asset$ret)

  list(
    n_obs_ret   = length(asset_ret),
    n_obs_spread = sum(!is.na(asset$spread_bbo)),
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

moex_listing <- readr::read_rds(here("data", "processed", "moex_listing_2_3.rds"))
raw_quotes_spread <- readr::read_rds(here("data", "processed", "raw_quotes_spread.rds"))
securities_stats_all <- purrr::map_dfr(raw_quotes_spread, get_stats, .id = "ticker")

securities_stats_all <- securities_stats_all |>
  left_join(select(moex_listing, ticker, listing), by = "ticker") |>
  relocate(listing, .after = ticker)

securities_stats <- securities_stats_all |>
  filter(n_obs_ret >= 50 & n_obs_spread >= 50)

print("Не включены в статистику следующие тикеры:")
print(securities_stats_all |>
        filter(n_obs_ret < 50 | n_obs_spread < 50) |>
        select(ticker, listing, n_obs_ret, n_obs_spread)
)

write_xlsx(securities_stats, here("output", "tables", "Securities_stats.xlsx"))
