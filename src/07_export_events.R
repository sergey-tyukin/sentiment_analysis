library(here)
library(dplyr)

# Выгрузка отдельных новостей для анализа

ar_results <- readr::read_rds(here("data", "processed", "ar_results.rds"))

# Таблица событий дня +1 (реакция)
events_day1 <- ar_results |>
  filter(rel_day == 1, AR > 0.02) |>
  select(event_id, ticker, date_reaction = date, AR)

# Таблица событий дня 0 (новость)
events_day0 <- ar_results |>
  filter(rel_day == 0) |>
  select(event_id, date_news = date, sentiment_index)

# Объединяем
news_to_review <- events_day1 |>
  left_join(events_day0, by = "event_id") |>
  filter(!is.na(date_news)) |>
  select(
    ticker,
    date_news,
    date_reaction,
    sentiment_index,
    AR = AR
  ) |>
  arrange(desc(AR)) |>
  head(30)

print(news_to_review, n = 30)

write.csv(news_to_review, here("output", "tables", "news_to_review_manual.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("Сохранено", nrow(news_to_review), "кейсов.\n")
