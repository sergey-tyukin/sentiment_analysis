library(writexl)
library(here)
library(purrr)
library(dplyr)


# Функция сведения данных для регрессионного анализа ----

prepare_regression_data <- function(ticker, raw_sentiment, lag){
  common_dates <- intersect(raw_sentiment$date, quotes_spread[[ticker]]$date)
  
  regression_data <- data.frame(
    date = common_dates,
    sentiment = raw_sentiment[[ticker]][raw_sentiment$date %in% common_dates],
    ret = quotes_spread[[ticker]]$ret[quotes_spread[[ticker]]$date %in% common_dates]
  )
  
  regression_data$ret_moexbmi <- market_params$ret_moexbmi[
    match(regression_data$date, market_params$date)
  ]

  if(lag < 0){  # отрицательный лаг - котировки вперёд новостей
    regression_data$sentiment_lag <- lead(regression_data$sentiment, -lag)
  } else {
    regression_data$sentiment_lag <- lag(regression_data$sentiment, lag)
  }
  
  regression_data <- na.omit(regression_data)
  regression_data
}


# Функция для линейной регрессии ----

regression_linear <- function(ticker, raw_sentiment, lag=0){
  df <- prepare_regression_data(ticker, raw_sentiment, lag)
  
  model <- lm(ret ~ sentiment_lag + ret_moexbmi, data = df)
  # model <- lm(ret ~ sentiment_lag, data = df)
  
  smry <- summary(model)
  
  f_pvalue <- pf(smry$fstatistic[1], 
                 smry$fstatistic[2], 
                 smry$fstatistic[3], 
                 lower.tail = FALSE)
  
  current_result <- list(
    ticker = ticker,
    lag = lag,
    model_type = "linear",
    coef_sentiment = coef(model)["sentiment_lag"],
    coef_market = coef(model)["ret_moexbmi"],
    p_value_linear = smry$coefficients["sentiment_lag", "Pr(>|t|)"],
    p_value_market = smry$coefficients["ret_moexbmi", "Pr(>|t|)"],
    p_value_f_test = f_pvalue,
    r_squared = smry$r.squared,
    adj_r_squared = smry$adj.r.squared,
    f_statistic = smry$fstatistic[1],
    n_obs = nrow(df)
  )
  
  current_result
}


# Функция для квадратичной регрессии ----

regression_quadratic <- function(ticker, raw_sentiment, lag = 0){
  df <- prepare_regression_data(ticker, raw_sentiment, lag)
  
  model <- lm(ret ~ sentiment_lag + I(sentiment_lag^2) + ret_moexbmi, data = df)
  # model <- lm(ret ~ sentiment_lag + I(sentiment_lag^2), data = df)
  
  smry <- summary(model)
  
  f_pvalue <- pf(smry$fstatistic[1], 
                 smry$fstatistic[2], 
                 smry$fstatistic[3], 
                 lower.tail = FALSE)
  
  current_result <- list(
    ticker = ticker,
    lag = lag,
    model_type = "quadratic",
    coef_sentiment = coef(model)["sentiment_lag"],
    coef_sentiment_sq = coef(model)["I(sentiment_lag^2)"],
    coef_market = coef(model)["ret_moexbmi"],
    p_value_linear = smry$coefficients["sentiment_lag", "Pr(>|t|)"],
    p_value_quadratic = smry$coefficients["I(sentiment_lag^2)", "Pr(>|t|)"],
    p_value_market = smry$coefficients["ret_moexbmi", "Pr(>|t|)"],
    p_value_f_test = f_pvalue,
    r_squared = smry$r.squared,
    adj_r_squared = smry$adj.r.squared,
    f_statistic = smry$fstatistic[1],
    n_obs = nrow(df)
  )
  
  current_result
}


# Регрессионный анализ ----

quotes_spread <- readr::read_rds(here("data", "processed", "quotes_spread.rds"))
sentiment <- readr::read_rds(here("data", "processed", "sentiment.rds"))
selected_tickers <- readr::read_rds(here("data", "processed", "selected_tickers.rds"))
market_params <- readr::read_rds(here("data", "processed", "market_params.rds"))
securities_listing <- readr::read_rds(here("data", "processed", "securities_listing.rds"))

lags <- -2:7
regression <- map_dfr(lags, function(lag_val) {
    linear_results <- map_dfr(selected_tickers, regression_linear, sentiment, lag = lag_val)
    quadratic_results <- map_dfr(selected_tickers, regression_quadratic, sentiment, lag = lag_val)
    bind_rows(linear_results, quadratic_results)
  }) |>
  left_join(securities_listing, by = "ticker") |>
  relocate(listing, .after = ticker)

readr::write_rds(regression,
                 here("data", "processed", "regression.rds"),
                 compress = "gz")
write_xlsx(regression, here("output", "tables", "regression.xlsx"))
