get_prices <- function(ticker, from, to, api_key) {
  base_url <- paste0("https://apim.moex.com/iss/engines/stock/markets/shares/boards/tqbr/securities/", ticker, "/candles.json?")
  params <- list(
    from = format(from, "%Y-%m-%d"),
    till = format(to, "%Y-%m-%d"),
    interval = 24
  )

  url <- httr::modify_url(base_url, query = params)

  response <- httr::GET(
    url,
    httr::add_headers(
      "Authorization" = paste("Bearer", api_key)
    )
  )

  stopifnot(response[["status_code"]] == 200)
  raw_data <- httr::content(response)
  if (length(raw_data$candles$data) != 0) {
    data <- as.data.frame(do.call(rbind, raw_data$candles$data))
    colnames(data) <- raw_data$candles$columns
    data$begin <- lubridate::ymd_hms(data$begin)
    data$end <- lubridate::ymd_hms(data$end)
  } else {
    data = NULL
  }
  return(data)
}


get_prices_all <- function(ticker, api_key) {
  message("Обработка тикера: ", ticker)
  years <- 2019:2026
  date_ranges <- lapply(years, function(y) {
    start <- as.Date(paste0(y, "-01-01"))
    end   <- as.Date(paste0(y, "-12-31"))
    get_prices(ticker, start, end, api_key)
  })
  do.call(rbind, date_ranges)
}


get_stock_data <- function(ticker, api_key) {
  prices <- get_prices_all(ticker, api_key)

  result <- prices |>
    mutate(
      date = as.Date(begin),
      price = as.numeric(close),
      volume = as.numeric(volume),
      ret = log(price / lag(price))
    ) |>
    select(date, price, volume, ret) |>
    arrange(date)

  return(result)
}
