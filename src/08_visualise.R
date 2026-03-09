library(dplyr)
library(ggplot2)
library(here)
library(readr)

# Загрузка данных
sentiment <- read_rds(here("data", "processed", "sentiment2.rds"))
quotes_spread <- read_rds(here("data", "processed", "quotes_spread.rds"))

# Настройка кейса
CASE_TICKER <- "ABIO"
CASE_NEWS_DATE <- as.Date("2024-01-17")
WINDOW_DAYS <- c(-2, 7)

# Собираем данные для тикера
quotes_df <- quotes_spread[[CASE_TICKER]] |>
  arrange(date) |>
  mutate(
    cum_return = cumsum(ret),
    value_norm = value / median(value, na.rm = TRUE)
  )

# Вырезаем окно вокруг новости из котировок
plot_data <- quotes_df |>
  filter(date >= (CASE_NEWS_DATE + WINDOW_DAYS[1]),
         date <= (CASE_NEWS_DATE + WINDOW_DAYS[2]))

# Берем сентимент только для нужного тикера (колонка с именем тикера)
sentiment_case <- sentiment |>
  select(date, sentiment_index = all_of(CASE_TICKER))

# Объединяем по дате
plot_data <- plot_data |>
  left_join(sentiment_case, by = "date")

# Простой график
ggplot(plot_data, aes(x = date)) +
  geom_line(aes(y = cum_return * 100), color = "steelblue", linewidth = 1) +
  geom_col(aes(y = value_norm * 2), fill = "gray70", alpha = 0.4) +
  geom_point(
    data = plot_data |> filter(!is.na(sentiment_index)),
    aes(y = sentiment_index * 3 + 2),  # сдвигаем вверх
    color = "red", size = 2
  ) +
  geom_vline(xintercept = as.numeric(CASE_NEWS_DATE), 
             linetype = "dashed", color = "darkred", linewidth = 0.8) +
  labs(
    title = paste(CASE_TICKER, "— реакция на новость"),
    subtitle = paste("Дата новости:", CASE_NEWS_DATE),
    x = "Дата",
    y = "Накопленная доходность, %",
    caption = "Серые столбцы — объем в рублях; красные точки — сентимент"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
