library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(arrow)
library(purrr)

source(here("config", "api_key.R"))
source(here("src", "functions", "algopack.R"))


# Подгрузка готовых данных по сентименту ----

raw_sentiment <- read_excel(here("data", "raw", "Sent_2019-2025.xlsx"))
raw_sentiment$date <- as.Date(raw_sentiment$date)
readr::write_rds(raw_sentiment,
                 here("data", "processed", "raw_sentiment.rds"),
                 compress = "gz")


# Подгрузка уровней листинга на MOEX ----

raw_securities <- read_excel(here("data", "raw", "Listing.xlsx"), guess_max = 5000)
readr::write_rds(raw_securities,
                 here("data", "processed", "raw_securities.rds"),
                 compress = "gz")

moex_listing <- raw_securities |>
  filter(SUPERTYPE == "Акции") |>
  select(TRADE_CODE, LIST_SECTION) |>
  drop_na() |>
  mutate(LIST_SECTION = recode(LIST_SECTION,
                                   "Первый уровень" = 1L,
                                   "Второй уровень" = 2L,
                                   "Третий уровень" = 3L)) |>
  rename(ticker = TRADE_CODE, listing = LIST_SECTION) |>
  arrange(listing, ticker)

readr::write_rds(moex_listing,
                 here("data", "processed", "moex_listing.rds"),
                 compress = "gz")


# Получаем котировки с помощью MOEX Algopack ----

raw_quotes <- lapply(moex_listing$ticker, get_stock_data, api.moex_alogpack)
names(raw_quotes) <- moex_listing$ticker
raw_quotes <- raw_quotes[order(names(raw_quotes))]
readr::write_rds(raw_quotes,
                 here("data", "processed", "raw_quotes.rds"),
                 compress = "gz")


# Подгружаем значение спреда ----

update_ticker <- function(ticker, df_quotes) {
  file_path = here("data", "processed", "daily_spread", paste0(ticker, ".parquet"))
  
  value_cols <- c("spread_bbo", "spread_lv10", "spread_1mio")

  if (!file.exists(file_path)) {
    cat("Файл спреда для", ticker, "не найден: ", file_path, "\n")
    return(df_quotes)
  }
  
  df_spread <- read_parquet(file_path)
  
  if (!"date" %in% colnames(df_spread)) {
    cat("В файле", file_path, "отсутствует колонка 'date'\n")
    return(df_quotes)
  }
  
  # Определяем, какие колонки из value_cols существуют в файле
  existing_cols <- intersect(value_cols, colnames(df_spread))
  missing_cols <- setdiff(value_cols, existing_cols)
  
  df_spread <- df_spread |> 
    select(date, any_of(existing_cols))
  
  # Добавляем недостающие колонки как NA
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      df_spread[[col]] <- NA
    }
    cat("В файле", file_path, "отсутствует колонки:", paste(missing_cols, collapse = ", "), "\n")
  }

  df_merged <- df_quotes |>
    select(-any_of(value_cols)) |>
    left_join(df_spread,by = "date") |>
    arrange(date)
  
  cat("В", ticker, "добавлен спред | строк:", nrow(df_merged), "\n")
  return(df_merged)
}

raw_quotes_spread <- Map(update_ticker, names(raw_quotes), raw_quotes)

readr::write_rds(raw_quotes_spread,
                 here("data", "processed", "raw_quotes_spread.rds"),
                 compress = "gz")
