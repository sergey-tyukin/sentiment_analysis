library(here)


# 1. Подготовка данных ----
# - сентимент (data/processed/sentiment.rds)
# - уровни листинга (data/processed/securities_listing.rds)
# - котировки, спред акций и объем торгов (data/processed/quotes_spread.rds)
# - рыночный объем торгов и доходность (data/processed/market_params.rds)
# Дополнительно:
# - все данные по акциям (data/raw/securities_info.rds)

source(here('src', '01_prepare_data.R'))


# 2. Выбор акций для оценки ----
# - выбранные акции для анализа (data/processed/selected_tickers.rds)

source(here('src', '02_filter_securities.R'))


# 3. Анализ акций ----
# - таблица с параметрами акций (output/tables/securities_stats.xlsx)
source(here('src', '03_quotes_analysis.R'))


# 4. Линейная регрессия ----
# - результаты регрессии (data/processed/regression.rds)
# - таблица с результатами регрессионного анализа (output/tables/regression.xlsx)
source(here('src', '04_regression.R'))
