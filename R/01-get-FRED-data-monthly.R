# Program: 01-get-FRED-data-monthly.R
# Purpose: Get monthly data from FRED.
# 
# The code retrieves monthly data from FRED. The output is saved as a .csv file
# that is stamped with a date. These data are archived in the project and not
# included in the targets pipeline. 
# 
# Updating the project requires re-running this code and changing the name of
# the file in _targets.R. The change will reflect the current date.
# 
# Date Started: 2024-09-24
# Date Revised: 2024-09-24

# FRED variables:
# 
# Index of Global Real Economic Activity (IGREA)
# Consumer Price Index for All Urban Consumers: All Items in U.S. City Average (CPIAUCSL)
# All Employees, Total Nonfarm (PAYEMS)
# Employed Persons in Kern County, CA (LAUCN060290000000005)
# Employed Persons in McKenzie County, ND (LAUCN380530000000005)
# Employed Persons in Karnes County, TX (LAUCN482550000000005)
# Employed Persons in Eddy County, NM (LAUCN350150000000005)
# Employed Persons in Weld County, CO (LAUCN081230000000005)

# File to save retrieved data timestamped with today's date for reproducibility
file_prg <- "01-get-FRED-data-monthly"
fred_api_key <- Sys.getenv("FRED_API_KEY")
date_fred_monthly <- today()
fout_fred_monthly <- here("dta", "src", paste0("dat_", file_prg, "_" , date_fred_monthly, ".csv"))

fred_dictionary <- tribble(
  ~fred_series, ~series_name,
  "IGREA",                "aggdemand",
  "CPIAUCSL",             "cpi",
  "PAYEMS",               "payems",
  "LAUCN060290000000005", "empl_kern_nsa",
  "LAUCN380530000000005", "empl_mckenzie_nsa",
  "LAUCN482550000000005", "empl_karnes_nsa",
  "LAUCN350150000000005", "empl_eddy_nsa",
  "LAUCN081230000000005", "empl_weld_nsa"
  )


# Code using fredr package ------------------------------------------------

# Get all available data using extreme dates
dat_fred_raw <-  list_rbind(
  map(fred_dictionary$fred_series, fredr, observation_start = ymd("1776-07-04"), observation_end = ymd("9999-12-31"))
) |> 
  select(-starts_with("realtime_"))

dat_fred <- dat_fred_raw |>
  left_join(fred_dictionary, by = join_by(series_id == fred_series)) |>
  pivot_wider(id_cols = date,
              names_from = series_name,
              values_from = value) |> 
  arrange(date)

write_csv(dat_fred, file = fout_fred_monthly)

# Code using tidyquant package and tq_get that doesn't work ---------------

# dat_fred_raw <- tq_get(fred_dictionary$fred_series,
#                        get = "economic.data",
#                        from = "1900-01-01")
# 
# dat_fred <- dat_fred_raw |>
#   left_join(fred_dictionary, by = join_by(symbol == fred_series)) |> 
#   pivot_wider(id_cols = date,
#               names_from = series_name,
#               values_from = price) 
# 
# write_csv(dat_fred, file = fout_fred_monthly)