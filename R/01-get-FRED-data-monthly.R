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


# Recession dates ---------------------------------------------------------

fredrr <- function(vars, from) {
  # Make compatible with 
  if (missing(from)) {
    from <- ymd("1776-07-04")
  }
  dat <- list_rbind(
    map(vars, fredr::fredr, observation_start = from, observation_end = ymd("9999-12-31"))
  ) |> 
    select(-starts_with("realtime_"))
  
  dat <- dat |> 
    rename(price = value,
           symbol = series_id)
}

get_dat_recess <- function() {
  recess <- fredrr("USRECM") # tq_get("USRECM", get = "economic.data", from = "1800-01-01")
  recess_dat <- recess |> 
    arrange(date) |> 
    mutate(same = 1 - (price == lag(price))) |> 
    # Remove first row, an NA, for cumulative sum
    filter(date > min(recess$date)) |> 
    mutate(era = cumsum(same)) |> 
    # Filter only recessions
    filter(near(price, 1))
  
  recess_dat <- recess_dat |> 
    group_by(era) |> 
    # Unncessary, but to be sure...
    arrange(date) |>  
    filter(row_number() == 1 | row_number() == n())
  
  # Now reshape the data wide.
  # Each row will contain the start and end dates of a recession.
  recess_dat <- recess_dat |> 
    mutate(junk = row_number()) |> 
    mutate(begin_end = case_when(
      junk == 1 ~ "begin",
      junk == 2 ~ "end"
    ))
  
  recess_wide <- recess_dat |> 
    ungroup() |> 
    select(symbol, price, date, era, begin_end) |> 
    pivot_wider(names_from = begin_end, values_from = date)
  
  return(recess_wide)
}

dat_recess <- get_dat_recess()
fout_fred_monthly_recess <- here("dta", "src", paste0("dat_", file_prg, "_recession_" , date_fred_monthly, ".csv"))
write_csv(dat_recess, file = fout_fred_monthly_recess)

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