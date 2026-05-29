clean_qcew_data_file <- function(fin, naics_codes_keep) {
  df <- read_csv(fin,
                 col_types = cols(.default = col_character()))

  df <- df |>
    filter(industry_code %in% naics_codes_keep) |>
    filter(industry_code > 10 | own_code == 0)
  
  # Confirm industry codes > 10 are in Private 
  df_check <- df |> 
    # 10 = Total, all industries
    filter(industry_code > 10) |> 
    # 5 = Private
    verify(own_code == 5)
  
  return(df)
}

clean_qcew_data <- function(fin) {
  
  naics_codes_keep <- c(10, 21, 211, 212, 213)
  dat_all_list <- map(fin, clean_qcew_data_file, naics_codes_keep = naics_codes_keep)
  dat_all <- list_rbind(dat_all_list)
  
  # Clean dat_all
  dat_all <- dat_all |>
    select(year, qtr, area_fips, area_title, industry_code, industry_title, agglvl_code,
           qtrly_estabs_count, total_qtrly_wages,
           avg_wkly_wage,
           month1_emplvl, month2_emplvl, month3_emplvl) |>
    mutate(year = as.numeric(year),
           qtr = as.numeric(qtr),
           qtrly_estabs_count = as.numeric(qtrly_estabs_count),
           month1_emplvl = as.numeric(month1_emplvl), 
           month2_emplvl = as.numeric(month2_emplvl), 
           month3_emplvl = as.numeric(month3_emplvl),
           industry_code = as.numeric(industry_code),
           total_qtrly_wages = as.numeric(total_qtrly_wages),
           my_industry_title = case_when(
             industry_code == 10  ~ "All industries",
             industry_code == 21  ~ "Mining and\nOil and Gas Extraction",
             industry_code == 211 ~ "   Oil and Gas Extraction",
             industry_code == 212 ~ "   Mining",
             industry_code == 213 ~ "   Support Activities"),
           my_industry_title = fct_reorder(my_industry_title, industry_code))
  
  return(dat_all)
}

get_dat_estabs_count <- function(dat_all_qcew) {
  dat_estabs_count <- dat_all_qcew |>
    select(-starts_with("month"), -total_qtrly_wages) |> 
    mutate(date = yq(paste(year, qtr, sep = "-")))
  return(dat_estabs_count)
}

get_dat_qtrly_wages <- function(dat_all_qcew) {
  
  dat_wages <- dat_all_qcew |>
    select(-starts_with("month"), -all_of(c("qtrly_estabs_count"))) |> 
    mutate(date = yq(paste(year, qtr, sep = "-"))) 
  
  # Merge in total wages
  dat_wages_total <- dat_wages |>
    filter(industry_code == 10) |>
    select(all_of(c("area_fips", "date", "total_qtrly_wages"))) |>
    rename(total_qtrly_wages10 = total_qtrly_wages)

  dat_wages <- dat_wages |>
    left_join(dat_wages_total, by = join_by(area_fips, date)) |>
    mutate(share_ind_wages = 100 * total_qtrly_wages / total_qtrly_wages10) |>
    select(-total_qtrly_wages10)

  # Check all industries have 100 percent of wages
  dat_wages |> filter(industry_code == 10) |> verify(near(share_ind_wages, 100))
  
  return(dat_wages)
}

get_dat_empl_qcew_cps <- function(dat_all_qcew, dat_fred_monthly) {
  
  dat_cps <- dat_fred_monthly |> 
    drop_na(empl_kern_nsa) |> 
    select(date, starts_with("empl_")) |> 
    pivot_longer(cols = starts_with("empl_"), values_to = "empl_cps") |> 
    mutate(my_county1 = str_split_i(name, "_", 2))
  
  dat_empl <- dat_all_qcew |>
    select(-qtrly_estabs_count) |>
    pivot_longer(cols = starts_with("month"),
                 names_to = "series", values_to = "empl") |>
    mutate(month_in_quarter = as.numeric(str_sub(series, start = 6, end = 6)),
           month = (qtr - 1) * 3 + month_in_quarter,
           date = ymd(paste(year, month, "01", sep = "-"))) |>
    select(-month_in_quarter, -series, -year, -month, -qtr, -total_qtrly_wages, -agglvl_code) |> 
    filter(industry_code == 10) |> 
    mutate(my_county1 = tolower(str_split_i(area_title, " ", 1))) |> 
    left_join(dat_cps, by = join_by(my_county1, date)) |> 
    rename(empl_nsa = empl,
           empl_cps_nsa = empl_cps) |> 
    group_by(area_title) |> 
    mutate(empl = kilianr::seasonally_adjust_monthly(empl_nsa, date),
           empl_cps = kilianr::seasonally_adjust_monthly(empl_cps_nsa, date),
           pchange_empl = 100 * (empl / lag(empl, n = 1, order_by = date) - 1),
           pchange_empl0 = kilianr::detrendcl(pchange_empl, tt = "constant"),
           change_log_empl = 100 * (log(empl) - lag(log(empl), n = 1, order_by = date)),
           change_log_empl0 = kilianr::detrendcl(change_log_empl, tt = "constant")) |> 
    ungroup()
           
  return(dat_empl)
}

get_dat_empl_qcew_ind <- function(dat_all_qcew, dat_empl_qcew_cps) {
  
  dat_empl_oil_gas <- dat_all_qcew |> 
    filter(industry_code != 10) 

  dat_empl_oil_gas <- dat_empl_oil_gas |> 
    select(area_fips, area_title, industry_code, ends_with("industry_title"), starts_with("month"), year, qtr) |> 
    pivot_longer(cols = starts_with("month"),
                 names_to = "series", values_to = "empl_nsa") |>
    mutate(month_in_quarter = as.numeric(str_sub(series, start = 6, end = 6)),
           month = (qtr - 1) * 3 + month_in_quarter,
           date = ymd(paste(year, month, "01", sep = "-"))) |> 
    select(-month_in_quarter, -month, - year, -qtr) 
  
  # Merge total employment
  empl_total <- dat_empl_qcew_cps |> 
    select(area_fips, date, empl_nsa) |> 
    rename(empl_nsa_total = empl_nsa)
  
  dat_empl_oil_gas <- dat_empl_oil_gas |> 
    left_join(empl_total, by = join_by(area_fips, date)) |> 
    mutate(empl_share_nsa = 100 * empl_nsa / empl_nsa_total)
  
  return(dat_empl_oil_gas)
}

fred_api_key <- Sys.getenv("FRED_API_KEY")
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
  recess_dat <- recess %>% 
    arrange(date) %>% 
    mutate(same = 1 - (price == lag(price))) %>% 
    # Remove first row, an NA, for cumulative sum
    filter(date > min(recess$date)) %>% 
    mutate(era = cumsum(same)) %>% 
    # Filter only recessions
    filter(price == 1)
  
  recess_dat <- recess_dat %>% 
    group_by(era) %>% 
    # Unncessary, but to be sure...
    arrange(date) %>% 
    filter(row_number() == 1 | row_number() == n())
  
  # Now reshape the data wide.
  # Each row will contain the start and end dates of a recession.
  recess_dat <- recess_dat %>% 
    mutate(junk = row_number()) %>% 
    mutate(begin_end = case_when(
      junk == 1 ~ "begin",
      junk == 2 ~ "end"
    ))
  
  recess_wide <- recess_dat %>%
    ungroup() %>% 
    select(symbol, price, date, era, begin_end) %>% 
    pivot_wider(names_from = begin_end, values_from = date)
  
  return(recess_wide)
}

get_dat_var <- function(dat_empl_qcew_cps, dat_fred_monthly, dat_oil_production, dat_oil_price) {
  employment_variables <- c("empl", "pchange_empl", "pchange_empl0", "change_log_empl", "change_log_empl0")
  dat_empl <- dat_empl_qcew_cps |> 
    select(all_of(c("area_fips", "area_title", "date", employment_variables)))
  
  dat_fred_2keep <- dat_fred_monthly |> 
    select(date, aggdemand, cpi, payems) |> 
    mutate(dempl_payems = 100 * (log(payems) - lag(log(payems), n = 1, order_by = date)))
  
  dat_oil <- dat_oil_production |> 
    left_join(dat_oil_price, join_by(date))
  
  dat_tmp <- dat_fred_2keep |> 
    left_join(dat_oil, join_by(date)) |> 
    mutate(
      oilsupply = 100 * (log(crude_prod_millions_barrels_day) - lag(log(crude_prod_millions_barrels_day), n = 1, order_by = date)),
      rpoil_nodetrend = 100 * log(price_per_barrel / cpi),
      rpoil = detrendcl(rpoil_nodetrend, tt = "constant")
    ) 
  
  dat <- dat_empl |> 
    left_join(dat_tmp, join_by(date)) |> 
    select(all_of(c("area_fips", "area_title", "date", "oilsupply", "aggdemand", "rpoil", employment_variables))) |> 
    arrange(area_fips, date)

  return(dat)
}



get_dat_wage2 <- function(dat_qtrly_wages, dat_fred_monthly, my_area_title) {
  
  dat_fred_qtrly <- dat_fred_monthly |>
    mutate(year = year(date), quarter = quarter(date)) |>
    group_by(year, quarter) |>
    summarise(cpi = mean(cpi), date = first(date))
  
  cpi_factor <- dat_fred_monthly |>
    drop_na(cpi) |>
    filter(date == max(date)) |> pull(cpi)
  
  dat <- dat_qtrly_wages |>
    filter(industry_code == 10) |>
    filter(area_title == my_area_title) |>
    # filter(area_title == "Kern County, California") |> 
    assertr::verify(
      industry_title == "Total, all industries" |
        industry_title == "10 Total, all industries"
    ) |>
    mutate(avg_wkly_wage = as.numeric(avg_wkly_wage)) |>
    ungroup() |>
    left_join(dat_fred_qtrly, by = join_by(year == year, qtr == quarter, date == date)) |>
    mutate(
      avg_wkly_wage_nominal_nsa = avg_wkly_wage,
      avg_wkly_wage_nsa = cpi_factor * avg_wkly_wage_nominal_nsa / cpi,
      avg_wkly_wage_nominal = seasonally_adjust(avg_wkly_wage_nominal_nsa, date, 4),
      avg_wkly_wage = seasonally_adjust(avg_wkly_wage_nsa, date, 4)
    )
  
  plt <- ggplot(data = dat) +
    geom_line(
      mapping = aes(x = date, y = avg_wkly_wage_nsa),
      color = "black",
      linetype = "dashed"
    ) +
    geom_line(
      mapping = aes(x = date, y = avg_wkly_wage),
      color = "black",
      linetype = "solid"
    ) +
    geom_line(
      mapping = aes(x = date, y = avg_wkly_wage_nominal_nsa),
      color = "red",
      linetype = "dashed"
    ) +
    geom_line(
      mapping = aes(x = date, y = avg_wkly_wage_nominal),
      color = "red",
      linetype = "solid"
    )
  
  ret <- list(plt_check_seasonal_adj = plt, 
              area_title = my_area_title,
              dat = dat)
  return(ret)
}

# tar_load(dat_qtrly_wages) 
# tar_load(dat_fred_monthly)
# get_dat_wage2(dat_qtrly_wages, dat_fred_monthly, my_area_title = "Kern County, California")
# my_area_title

# Employment by NAICS supersector -----------------------------------------

get_qcew_naics_supersector <- function(county_name) {
  # Read in all the data
  all_qcew_files <- here("dta", "src", list.files(path = here("dta", "src"), pattern = "^[0-9]{4}.*\\.csv", recursive = TRUE))
  qcew_files <- get_qcew_files[str_detect(get_qcew_files, county_name)]
  dat_all_list <- map(qcew_files, read_in_qcew_file)
  dat_all <- list_rbind(dat_all_list)
  
  dat <- clean_naics_supersector(dat_all)
  return(dat)
}

read_in_qcew_file <- function(fin) {
  df <- read_csv(fin,
                 col_types = cols(.default = col_character()))
}

clean_naics_supersector <- function(df) {
  df <- df |> 
    # 73 = County, by Supersector -- by ownership sector
    filter(agglvl_code == 73) |> 
    # Verify ownership code
    # 1 	Federal Government  
    # 2 	State Government
    # 3 	Local Government  
    # 4 	International Government
    # 5 	Private
    verify(own_code == 1 | own_code == 2 | own_code == 3 | own_code == 4 | own_code == 5) |> 
    # Collapse across ownership type
    group_by(year, qtr, industry_code) |> 
    summarise(industry_code = first(industry_code),
              industry_title = first(industry_title),
              month1_emplvl = sum(as.numeric(month1_emplvl)),
              month2_emplvl = sum(as.numeric(month2_emplvl)),
              month3_emplvl = sum(as.numeric(month3_emplvl))) |> 
    mutate(year = as.numeric(year),
           qtr = as.numeric(qtr)) |> 
    pivot_longer(cols = starts_with("month"),
                 names_to = "series", values_to = "empl") |>
    mutate(month_in_quarter = as.numeric(str_sub(series, start = 6, end = 6)),
           month = (qtr - 1) * 3 + month_in_quarter,
           date = ymd(paste(year, month, "01", sep = "-")))
  
  
  # === Start cleaning the data more methodically
  # Standardize NAICS supersector codes
  df <- df |> 
    mutate(supersector_title_tmp = str_replace_all(industry_title, "[0123456789]", ""),
           supersector_title = str_replace(supersector_title_tmp, "^\\s", "")) |> 
    select(-supersector_title_tmp)
  
  # Drop unclassified employment
  df_check_date <- df |> 
    group_by(supersector_title) |> 
    summarise(start = min(date),
              end = max(date)) |> 
    verify(end == max(end)) |> 
    filter(start != ymd("1990-01-01")) |> 
    pull(supersector_title)
  
  df <- df |> 
    filter(supersector_title != df_check_date)
  
  # Seasonally adjust
  df <- df |> 
    rename(empl_nsa = empl) |> 
    group_by(supersector_title) |> 
    mutate(empl = kilianr::seasonally_adjust_monthly(empl_nsa, date))
  
  # Compute shares
  df <- df |> 
    group_by(date) |> 
    mutate(empl_total = sum(empl)) |> 
    arrange(date) |> 
    ungroup() |> 
    mutate(empl_share = 100 * empl / empl_total)
  
  return(df)
}

