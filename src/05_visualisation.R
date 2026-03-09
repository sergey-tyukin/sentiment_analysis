library(here)
library(dplyr)
library(ggplot2)


regression <- readr::read_rds(here("data", "processed", "regression.rds"))

heatmap_data <- regression |>
  group_by(lag, model_type, listing) |>
  summarise(
    pct_significant = mean(p_value_linear < 0.05, na.rm = TRUE) * 100,
    n_significant = sum(p_value_linear < 0.05, na.rm = TRUE),
    n_total = n(),
    .groups = "drop"
  )

ggplot(heatmap_data, aes(x = model_type, y = factor(lag), fill = pct_significant)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct_significant), "%")), 
            color = "white", size = 3) +
  scale_fill_gradient(low = "#fff7fb", high = "#88419d", name = "Значимых, %") +
  labs(
    title = "Доля значимых результатов по лагам и уровню листинга",
    x = "Тип модели",
    y = "Лаг"
  ) +
  facet_wrap(~ listing, ncol = 2) +  # <-- Разбивка по листингу
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold")  # Жирные заголовки фасетов
  )