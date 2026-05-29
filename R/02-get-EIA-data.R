# Program: 02-get-EIA-data.R
# Purpose: Get data on production and price of oil from the US Energy
# Information Administration or EIA (https://www.eia.gov/).
# 
# Two series are restrieved and two files are saved on:
#   1. production
#   2. price
# 
# A useful reference to for retrieving the data is:
# https://www.eia.gov/opendata/documentation.php
# 
# The code retrieves dynamically from the internet. The output is saved as a
# .csv file that is stamped with a date. These data are archived in the project
# and not included in the targets pipeline.
# 
# Updating the project requires re-running this code and changing the name of
# the file in _targets.R. The change will reflect the current date.
# 
# Date Started: 2024-09-24
# Date Revised: 2025-08-10

library(httr)
library(jsonlite)

# File parameters ---------------------------------------------------------

key <- Sys.getenv("EIA_KEY")

file_prg <- "02-get-EIA-data"

# Production --------------------------------------------------------------

# This function comes from https://github.com/richryan/jcre-oil/
get_EIA_data_production <- function(api_key) {
  # Retrieve data using API from US Energy Information Administration: PRODUCTION
  series_start <- "https://api.eia.gov/v2/international/data/?api_key="
  series_end <- "frequency=monthly&data[0]=value&facets[activityId][]=1&facets[productId][]=57&facets[countryRegionId][]=WORL&facets[unit][]=TBPD&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
  series <- paste0(series_start, api_key, "&", series_end)
  
  res <- httr::GET(series)
  print(res)
  # JSON stands for JavaScript Object Notation
  data <- jsonlite::fromJSON(rawToChar(res$content))
  
  print(data$response$total)
  print(data$response$dateFormat)
  print(data$response$description)
  
  dat_raw <- as_tibble(data$response$data)
  
  dat <- dat_raw |> 
    mutate(year = as.integer(str_sub(period, start = 1, end = 4)),
           mnth = as.integer(str_sub(period, start = 6, end = 8)),
           date = ymd(paste(year, mnth, "01", sep = "-")),
           crude_prod_millions_barrels_day_nsa = as.numeric(value),
           crude_prod_millions_barrels_day = seasonally_adjust_monthly(crude_prod_millions_barrels_day_nsa, date)) |> 
    select(date, crude_prod_millions_barrels_day, crude_prod_millions_barrels_day_nsa)
  
  return(dat)
}

dat_EIA_production_raw <- get_EIA_data_production(Sys.getenv("EIA_KEY"))

ggplot(data = dat_EIA_production_raw) +
  geom_line(mapping = aes(x = date, y = crude_prod_millions_barrels_day_nsa), color = "blue") +
  geom_line(mapping = aes(x = date, y = crude_prod_millions_barrels_day), color = "green", linetype = "solid") +
  # geom_line(mapping = aes(x = date, y = crude_prod_millions_barrels_day), color = "red") +  
  labs(title = "Global production of\ncrude oil including lease condensate",
       x = "", y = "Millions of barrels per day",
       caption = "The green line represents seasonal adjustment.")

# series_start <- "https://api.eia.gov/v2/international/data/?api_key="
# series_end <- "frequency=monthly&data[0]=value&facets[activityId][]=1&facets[productId][]=57&facets[countryRegionId][]=WORL&facets[unit][]=TBPD&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
# series <- paste0(series_start, key, "&", series_end)
# 
# res <- GET(series)
# print(res)
# # JSON stands for JavaScript Object Notation
# data <- fromJSON(rawToChar(res$content))
# 
# print(data$response$total)
# print(data$response$dateFormat)
# print(data$response$description)
# 
# dat_raw <- as_tibble(data$response$data)
# 
# dat <- dat_raw |> 
#   mutate(year = as.integer(str_sub(period, start = 1, end = 4)),
#          mnth = as.integer(str_sub(period, start = 6, end = 8)),
#          date = ymd(paste(year, mnth, "01", sep = "-")),
#          crude_prod_millions_barrels_day = as.numeric(value)) |> 
#   select(date, crude_prod_millions_barrels_day)
# 
# ggplot(data = dat_EIA_production_raw) +
#   geom_line(mapping = aes(x = date, y = crude_prod_millions_barrels_day))

fout_prod <- here("dta", "src", paste0("dat_", file_prg, "_oil-production", "_", today(), ".csv"))
write_csv(dat_EIA_production_raw, fout_prod)

# Price -------------------------------------------------------------------

# This function comes from https://github.com/richryan/jcre-oil/
get_EIA_data_prices <- function(api_key) {
  # Retrieve data using API from US Energy Information Administration: PRICES
  price_series_start <- "https://api.eia.gov/v2/total-energy/data/?api_key="
  price_series_end <- "frequency=monthly&data[0]=value&facets[msn][]=RAIMUUS&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
  series <- paste0(price_series_start, api_key, "&", price_series_end)
  
  res <- httr::GET(series)
  print(res)
  
  data <- jsonlite::fromJSON(rawToChar(res$content))
  
  print(data$response$total)
  print(data$response$dateFormat)
  print(data$response$description)
  
  dat_raw <- as_tibble(data$response$data)
  
  dat <- dat_raw |> 
    mutate(price_per_barrel_nsa = as.numeric(value),
           year = as.integer(str_sub(period, start = 1, end = 4)),
           mnth = as.integer(str_sub(period, start = 6, end = 8)),
           date = ymd(paste(year, mnth, "01", sep = "-")),
           price_per_barrel = seasonally_adjust_monthly(price_per_barrel_nsa, date)) |> 
    select(date, price_per_barrel, price_per_barrel_nsa)
  
  ggplot(data = dat) +
    geom_line(mapping = aes(x = date, y = price_per_barrel))
  
  return(dat)
}

dat_EIA_prices_raw <- get_EIA_data_prices(Sys.getenv("EIA_KEY"))

# price_series_start <- "https://api.eia.gov/v2/petroleum/pri/rac2/data/?api_key="
# price_series_end <- "frequency=monthly&data[0]=value&facets[series][]=R0000____3&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
# series <- paste0(price_series_start, key, "&", price_series_end)
# 
# res <- GET(series)
# print(res)
# 
# data <- fromJSON(rawToChar(res$content))
# 
# print(data$response$total)
# print(data$response$dateFormat)
# print(data$response$description)
# 
# dat_raw <- as_tibble(data$response$data)
# 
# dat <- dat_raw |> 
#   mutate(price_per_barrel = as.numeric(value),
#          year = as.integer(str_sub(period, start = 1, end = 4)),
#          mnth = as.integer(str_sub(period, start = 6, end = 8)),
#          date = ymd(paste(year, mnth, "01", sep = "-"))) |> 
#   select(date, price_per_barrel)
# 
# ggplot(data = dat) +
#   geom_line(mapping = aes(x = date, y = price_per_barrel))

ggplot(data = dat_EIA_prices_raw) +
  geom_line(mapping = aes(x = date, y = price_per_barrel_nsa), color = "blue") +
  geom_line(mapping = aes(x = date, y = price_per_barrel), color = "green", linetype = "solid") +
  # geom_line(mapping = aes(x = date, y = crude_prod_millions_barrels_day), color = "red") +  
  labs(title = "Price",
       x = "", y = "Amount",
       caption = "The green line represents seasonal adjustment.")

fout_price <- here("dta", "src", paste0("dat_", file_prg, "_oil-price", "_", today(), ".csv"))
write_csv(dat_EIA_prices_raw, fout_price)
