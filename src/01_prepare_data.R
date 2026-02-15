library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(arrow)
library(purrr)

source(here("config", "api_key.R"))
source(here("src", "functions", "algopack.R"))


# Подгрузка готовых данных по сентименту ----

sentiment <- read_excel(here("data", "raw", "Sent_2019-2025.xlsx"))
sentiment$date <- as.Date(sentiment$date)
readr::write_rds(sentiment,
                 here("data", "processed", "sentiment.rds"),
                 compress = "gz")


# Подгрузка уровней листинга на MOEX ----

securities_info <- read_excel(here("data", "raw", "Listing.xlsx"), guess_max = 5000)
readr::write_rds(securities_info,
                 here("data", "raw", "securities_info.rds"),
                 compress = "gz")

securities_listing <- securities_info |>
  filter(SUPERTYPE == "Акции") |>
  select(TRADE_CODE, LIST_SECTION) |>
  drop_na() |>
  mutate(LIST_SECTION = recode(LIST_SECTION,
                                   "Первый уровень" = 1L,
                                   "Второй уровень" = 2L,
                                   "Третий уровень" = 3L)) |>
  rename(ticker = TRADE_CODE, listing = LIST_SECTION) |>
  arrange(listing, ticker)

readr::write_rds(securities_listing,
                 here("data", "processed", "securities_listing.rds"),
                 compress = "gz")


# Получаем котировки с помощью MOEX Algopack ----

raw_quotes <- lapply(securities_listing$ticker, get_stock_data, api.moex_alogpack)
names(raw_quotes) <- securities_listing$ticker
raw_quotes <- raw_quotes[order(names(raw_quotes))]


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

quotes_spread <- Map(update_ticker, names(raw_quotes), raw_quotes)

readr::write_rds(quotes_spread,
                 here("data", "processed", "quotes_spread.rds"),
                 compress = "gz")


# Получаемы рыночные значения ----

## Получаем доходности индексов ----

imoex_quotes <- read_parquet(here("data", "raw", "imoex_quotes_raw.parquet"))
moexbmi_quotes <- read_parquet(here("data", "raw", "moexbmi_quotes_raw.parquet"))

moexbmi_quotes <- moexbmi_quotes |>
  rename(price = close) |>
  mutate(
    ret_moexbmi = log(price / lag(price)),
    date = as.Date(end)
  ) |>
  select(date, ret_moexbmi)

imoex_quotes <- imoex_quotes |>
  rename(price = close) |>
  mutate(
    ret_imoex = log(price / lag(price)),
    date = as.Date(end)
  ) |>
  select(date, ret_imoex)


## Считаем объем рыночных торгов ----

total_value <- quotes_spread |>
  bind_rows() |>
  group_by(date) |>
  summarise(
    total_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(date)

market_params <- total_value |>
  full_join(imoex_quotes, by = "date") |>
  full_join(moexbmi_quotes, by = "date")

readr::write_rds(market_params,
                 here("data", "processed", "market_params.rds"),
                 compress = "gz")
