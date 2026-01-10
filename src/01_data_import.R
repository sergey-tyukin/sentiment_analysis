library(readxl)
library(here)
library(dplyr)
library(tidyr)

source(here("config", "api_key.R"))
source(here("src", "functions", "algopack.R"))


# Подгрузка готовых данных по сентименту

raw_sentiment <- read_excel(here("data", "raw", "Sent_2019-2025.xlsx"))
readr::write_rds(raw_sentiment,
                 here("data", "processed", "raw_sentiment.rds"),
                 compress = "gz")


# Подгрузка уровней листинга на MOEX

raw_securities <- read_excel(here("data", "raw", "Listing.xlsx"), guess_max = 5000)
readr::write_rds(raw_securities,
                 here("data", "processed", "raw_securities.rds"),
                 compress = "gz")

moex_listing_2_3 <- raw_securities |>
  filter(SUPERTYPE == "Акции") |>
  filter(LIST_SECTION %in% c("Второй уровень", "Третий уровень")) |>
  select(TRADE_CODE, LIST_SECTION) |>
  drop_na() |>
  mutate(LIST_SECTION = case_match(LIST_SECTION,
                                   "Второй уровень" ~ 2L,
                                   "Третий уровень" ~ 3L)) |>
  rename(ticker = TRADE_CODE, listing = LIST_SECTION)

readr::write_rds(moex_listing_2_3,
                 here("data", "processed", "moex_listing_2_3.rds"),
                 compress = "gz")


# Получаем котировки с помощью MOEX Algopack

raw_quotes <- lapply(moex_listing_2_3$ticker, get_stock_data, api.moex_alogpack)
names(raw_quotes) <- moex_listing_2_3$ticker
readr::write_rds(raw_quotes,
                 here("data", "processed", "raw_quotes.rds"),
                 compress = "gz")
