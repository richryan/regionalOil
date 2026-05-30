# Kern County information -------------------------------------------------

plot_pcpi <- function(bea_data, color_kern, my_binwidth = 5000) {
  dat_raw <- read_csv(bea_data, skip = 3)
  
  dat <- dat_raw |>
    rename(pcpi = `2024`) |>
    mutate(state = str_sub(GeoFIPS, start = 1, end = 2)) |>
    filter(state == "06") |>
    mutate(pcpi = as.numeric(pcpi)) |> 
    arrange(desc(pcpi)) |>
    mutate(rank = row_number()) |>
    drop_na(pcpi)
  
  pcpi_kern <- dat |> filter(GeoName == "Kern, CA") |> pull(pcpi)
  rank_kern <- dat |> filter(GeoName == "Kern, CA") |> pull(rank)
  
  my_caption <- paste0(
    "Kern County (\\$",
    formatC(pcpi_kern, big.mark = ",", format = "f", digits = 0),
    ") ranks ", rank_kern, " out of ", nrow(dat), " California counties."
  )
  
  plt <- ggplot(data = dat) +
    geom_histogram(mapping = aes(x = pcpi), binwidth = my_binwidth) +
    geom_vline(xintercept = pcpi_kern, color = color_kern) +
    scale_y_continuous(breaks = seq(1, 12, 1)) +
    scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0.05, 0.12))) +
    labs(x = "Per-capita personal income (2024 dollars)", y = "Count", caption = my_caption) +
    theme_minimal()
  
  return(list(plt = plt, dat = dat))
}

# dat_raw <- read_csv(here("dta", "src", "2026-02-22_per-capita-personal-income-by-county.csv"), skip = 3)
# 
# dat <- dat_raw |> 
#   rename(pcpi = `2024`) |> 
#   mutate(pcpi = as.numeric(pcpi),
#          state = str_sub(GeoFIPS, start = 1, end = 2)) |> 
#   filter(state == "06") |> 
#   arrange(pcpi) |> 
#   mutate(rank = row_number()) |> 
#   drop_na(pcpi)
# 
# pcpi_kern <- dat |> filter(GeoName == "Kern, CA") |> pull(pcpi)
# rank_kern <- dat |> filter(GeoName == "Kern, CA") |> pull(rank)
# 
# my_caption <- paste0("Kern County, ($", 
#                      formatC(pcpi_kern, big.mark = ",", format = "f", digits = 0), 
#                      ") ranks ", rank_kern, " out of ", nrow(dat), " California counties.")
# ggplot(data = dat) +
#   geom_histogram(mapping = aes(x = pcpi), binwidth = 5000) +
#   geom_vline(xintercept = pcpi_kern, color = csub_blue) +
#   scale_y_continuous(breaks = seq(1, 12, 1)) +
#   scale_x_continuous(labels = scales::label_comma()) +
#   labs(x = "Per-capita personal income in 2024", y = "Count",
#        caption = my_caption) +
#   theme_minimal()

plot_educational_attainment <- function(acs_data, color_kern, nbins = 16) {
  dat_raw <- read_csv(acs_data, show_col_types = FALSE)

  # Keep only California county estimate columns (drops non-CA geographies like Maryland)
  dat_wide <- dat_raw |>
    select(`Label (Grouping)`, matches("County, California!!Estimate$")) |>
    rename(category = `Label (Grouping)`) |>
    mutate(
      # remove indentation / non-breaking spaces
      category = str_squish(str_replace_all(category, "\u00a0", ""))
    )

  dat_long <- dat_wide |>
    pivot_longer(cols = -category,
                 names_to = "county",
                 values_to = "estimate") |>
    mutate(
      county = str_remove(county, "!!Estimate$")
      # estimate = parse_number(estimate)  # handles commas like "1,192,437"
    )

  college_categories <- c(
    # "Associate's degree",
    "Bachelor's degree",
    "Master's degree",
    "Professional school degree",
    "Doctorate degree"
  )

  dat_out <- dat_long |> 
    group_by(county) |> 
    summarize(
      total_25plus = sum(estimate[category == "Total:"], na.rm = TRUE),
      college_deg  = sum(estimate[category %in% college_categories], na.rm = TRUE),
      share_college_deg = college_deg / total_25plus,
      .groups = "drop"
    ) |> 
    arrange(desc(share_college_deg)) |>
    mutate(rank = row_number())
  
  # (Optional) sanity check: should be 58 CA counties
  stopifnot(n_distinct(dat_out$county) == 58)

  share_college_deg_kern <- dat_out |> filter(county == "Kern County, California") |> pull(share_college_deg)
  rank_kern <- dat_out |> filter(county == "Kern County, California") |> pull(rank)  

  my_caption <- paste0(
    "Kern County (",
    formatC(100 * share_college_deg_kern, big.mark = ",", format = "f", digits = 0),
    " percent) ranks ", rank_kern, " out of ", n_distinct(dat_out$county), " California counties."
  )  
  
  ggplot(data = dat_out) +
    geom_histogram(mapping = aes(x = share_college_deg), bins = nbins) +
    geom_vline(xintercept = share_college_deg_kern, color = color_kern) +
    scale_y_continuous(breaks = seq(1, 10, 1)) +
    labs(x = "Share of people who have earned a college degree", y = "Count",
         caption = my_caption) +
    theme_minimal()
}

# f <- here("dta", "ACSDT5Y2024.B15002-2026-02-22T231022.csv")
# 
# dat_raw <- read_csv(f, show_col_types = FALSE)
# 
# # Keep only California county estimate columns (drops non-CA geographies like Maryland)
# dat_wide <- dat_raw |> 
#   select(`Label (Grouping)`, matches("County, California!!Estimate$")) |> 
#   rename(category = `Label (Grouping)`) |> 
#   mutate(
#     # remove indentation / non-breaking spaces
#     category = str_squish(str_replace_all(category, "\u00a0", ""))
#   )
# 
# dat_long <- dat_wide |> 
#   pivot_longer(
#     cols = -category,
#     names_to = "county",
#     values_to = "estimate"
#   ) |> 
#   mutate(
#     county = str_remove(county, "!!Estimate$")
#     # estimate = parse_number(estimate)  # handles commas like "1,192,437"
#   )
# 
# college_categories <- c(
#   # "Associate's degree",
#   "Bachelor's degree",
#   "Master's degree",
#   "Professional school degree",
#   "Doctorate degree"
# )
# 
# dat_out <- dat_long %>%
#   group_by(county) %>%
#   summarize(
#     total_25plus = sum(estimate[category == "Total:"], na.rm = TRUE),
#     college_deg  = sum(estimate[category %in% college_categories], na.rm = TRUE),
#     share_college_deg = college_deg / total_25plus,
#     .groups = "drop"
#   ) %>%
#   arrange(desc(share_college_deg)) |> 
#   mutate(rank = row_number())
# 
# # (Optional) sanity check: should be 58 CA counties
# stopifnot(n_distinct(dat_out$county) == 58)
# 
# share_college_deg_kern <- dat_out |> filter(county == "Kern County, California") |> pull(share_college_deg)
# 
# ggplot(data = dat_out) +
#   geom_histogram(mapping = aes(x = share_college_deg), bins = 16) +
#   geom_vline(xintercept = share_college_deg_kern) +
#   scale_y_continuous(breaks = seq(1, 10, 1)) +
#   labs(x = "Share of people who have earned a college degree", y = "Count") +
#   theme_minimal()  