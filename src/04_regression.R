library(writexl)
library(here)
library(purrr)
library(dplyr)


# Функция сведения данных для регрессионного анализа ----
prepare_regression_data <- function(ticker, lag){
  common_dates <- intersect(raw_sentiment$date, raw_quotes[[ticker]]$date)
  
  regression_data <- data.frame(
    date = common_dates,
    sentiment = raw_sentiment[[ticker]][raw_sentiment$date %in% common_dates],
    ret = raw_quotes[[ticker]]$ret[raw_quotes[[ticker]]$date %in% common_dates]
  )

  if(lag < 0){
    regression_data$sentiment_lag <- lead(regression_data$sentiment, -lag)
  } else {
    regression_data$sentiment_lag <- lag(regression_data$sentiment, lag)
  }
  
  regression_data <- na.omit(regression_data)
  regression_data
}


# Линейная регрессия ----
regression_linear <- function(ticker, lag=0){
  df <- prepare_regression_data(ticker, lag)
  
  model <- lm(ret ~ sentiment_lag, data = df)
  smry <- summary(model)
  
  current_result <- list(
    ticker = ticker,
    lag = lag,
    model_type = "linear",
    coef_sentiment = coef(model)["sentiment_lag"],
    p_value_linear = smry$coefficients["sentiment_lag", "Pr(>|t|)"],
    r_squared = smry$r.squared,
    adj_r_squared = smry$adj.r.squared,
    f_statistic = smry$fstatistic[1],
    n_obs = nrow(df)
  )
  
  current_result
}


# Квадратичная регрессия ----
regression_quadratic <- function(ticker, lag = 0){
  df <- prepare_regression_data(ticker, lag)
  
  model <- lm(ret ~ sentiment_lag + I(sentiment_lag^2), data = df)
  smry <- summary(model)
  
  current_result <- list(
    ticker = ticker,
    lag = lag,
    model_type = "quadratic",  # <-- ВАЖНО: "quadratic" вместо "linear"
    coef_sentiment = coef(model)["sentiment_lag"],
    coef_sentiment_sq = coef(model)["I(sentiment_lag^2)"],  # <-- ДОБАВЛЕНО: квадратичный коэффициент
    p_value_linear = smry$coefficients["sentiment_lag", "Pr(>|t|)"],
    p_value_quadratic = smry$coefficients["I(sentiment_lag^2)", "Pr(>|t|)"],  # <-- ДОБАВЛЕНО: p-value квадратичного члена
    r_squared = smry$r.squared,
    adj_r_squared = smry$adj.r.squared,
    f_statistic = smry$fstatistic[1],
    n_obs = nrow(df)
  )
  
  current_result
}

raw_quotes <- readr::read_rds(here("data", "processed", "raw_quotes.rds"))
raw_sentiment <- readr::read_rds(here("data", "processed", "raw_sentiment.rds"))

df <- prepare_regression_data('AKRN', 0)

lags <- -2:7
results_df <- map_dfr(lags, function(lag_val) {
  linear_results <- map_dfr(selected_tickers, regression_linear, lag = lag_val)
  quadratic_results <- map_dfr(selected_tickers, regression_quadratic, lag = lag_val)
  bind_rows(linear_results, quadratic_results)
})

write_xlsx(results_df, here("output", "tables", "Regression.xlsx"))
