tar_load(dat_supersector_kern)

df <- dat_supersector_kern |> 
  arrange(supersector_title, date) |> 
  group_by(industry_title) |> 
  mutate(empl_lag1 = lag(empl, n = 1, order_by = date),
         d_logempl = log(empl) - log(empl_lag1))

plt_county_analysis <- plt_kern_analysis

sol <- plt_county_analysis$var_sol
dat_var <- plt_county_analysis$var_dat

tt <- nrow(sol$y)
pp <- sol$p

B0inv <- t(chol(sol$SIGMAhat))
B0 <- solve(B0inv)

Ehat <- B0 %*% sol$Uhat

var_names <- rownames(Ehat)
vars_select <- paste0("shock_", var_names)

dat2 <- as_tibble(t(Ehat)) |>
  rename_with(.fn = function(x) paste0("shock_", x), .cols = everything()) |>
  mutate(date = dat_var$date[(pp+1):tt],
         year = year(date),
         month = month(date))

df <- df |> 
  left_join(dat2, by = join_by(date, year, month))

df2 <- df |> 
  # filter(supersector_title == "Construction") |> 
  drop_na(d_logempl, shock_rpoil) |> 
  group_by(supersector_title) |> 
  nest()

nrep <- 25
dat_irf2 <- stage2irf(y = df2$d_logempl, x = df2$shock_rpoil, 
                                  p = 15, block_length = 12, 
                                  nrep = nrep, 
                                  standard_deviation_factor = 1, 
                                  boot_seed = 676, cumeffect = TRUE) |> 
  mutate(
    irfstd = irf2 - irf2_lo,
    irf2_lolo = irf2 - 2 * irfstd,
    irf2_hihi = irf2 + 2 * irfstd
  )

colors_irf <- colorspace::sequential_hcl(4, palette = "DarkMint")
color_irf <- colors_irf[1]
color_ci <- colors_irf[3]

linewidth_irf <- 1.0
linewidth_ci <- 0.5

linetype_irf <- "solid"
linetype_ci <- "solid"

alpha_ci <- 0.4
alpha_ci1 <- 0.8

plt <- ggplot(data = dat_irf2) +
  geom_hline(yintercept = 0.0) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
              alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
              alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
  geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
  labs(x = "Months", y = "Percent", title = "Crude-oil supply shock") +
  # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
  # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
  theme_minimal()
(plt)

plot_irf2_dempl <- function(plt_county_analysis, dat_fred_monthly, nrep = 1000) {
  dat_fred <- dat_fred_monthly |> 
    mutate(dempl_payems = 100 * (log(payems) - lag(log(payems), n = 1, order_by = date))) |> 
    select(date, dempl_payems)
  
  sol <- plt_county_analysis$var_sol
  dat_var <- plt_county_analysis$var_dat
  
  tt <- nrow(sol$y)
  pp <- sol$p
  
  B0inv <- t(chol(sol$SIGMAhat))
  B0 <- solve(B0inv)
  
  Ehat <- B0 %*% sol$Uhat
  
  var_names <- rownames(Ehat)
  vars_select <- paste0("shock_", var_names)
  
  dat2 <- as_tibble(t(Ehat)) |>
    rename_with(.fn = function(x) paste0("shock_", x), .cols = everything()) |>
    mutate(date = dat_var$date[(pp+1):tt],
           year = year(date),
           month = month(date)) 
  
  dat2 <- dat2 |> 
    left_join(dat_fred, by = join_by(date)) |> 
    filter(date < ymd("2020-01-01"))
  
  dat_payems_oilsupply <- stage2irf(y = dat2$dempl_payems, x = -dat2$shock_oilsupply, 
                                    p = 15, block_length = 12, 
                                    nrep = nrep, 
                                    standard_deviation_factor = 1, 
                                    boot_seed = 676, cumeffect = TRUE) |> 
    mutate(
      irfstd = irf2 - irf2_lo,
      irf2_lolo = irf2 - 2 * irfstd,
      irf2_hihi = irf2 + 2 * irfstd
    )
  
  dat_payems_aggdemand <- stage2irf(y = dat2$dempl_payems, x = dat2$shock_aggdemand, 
                                    p = 15, block_length = 12, 
                                    nrep = nrep, 
                                    standard_deviation_factor = 1, 
                                    boot_seed = 676, cumeffect = TRUE) |> 
    mutate(
      irfstd = irf2 - irf2_lo,
      irf2_lolo = irf2 - 2 * irfstd,
      irf2_hihi = irf2 + 2 * irfstd
    )
  
  dat_payems_rpoil <- stage2irf(y = dat2$dempl_payems, x = dat2$shock_rpoil, 
                                p = 15, block_length = 12, 
                                nrep = nrep, 
                                standard_deviation_factor = 1, 
                                boot_seed = 676, cumeffect = TRUE) |> 
    mutate(
      irfstd = irf2 - irf2_lo,
      irf2_lolo = irf2 - 2 * irfstd,
      irf2_hihi = irf2 + 2 * irfstd
    )
  
  colors_irf <- colorspace::sequential_hcl(4, palette = "Sunset")
  color_irf <- colors_irf[1]
  color_ci <- colors_irf[3]
  
  linewidth_irf <- 1.0
  linewidth_ci <- 0.5
  
  linetype_irf <- "solid"
  linetype_ci <- "solid"
  
  alpha_ci <- 0.4
  alpha_ci1 <- 0.8
  
  plt_payems_oilsupply <- ggplot(data = dat_payems_oilsupply) +
    geom_hline(yintercept = 0.0) +
    geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
                alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
    geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
                alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
    geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
    labs(x = "Months", y = "Percent", title = "Crude-oil supply shock") +
    # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
    # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
    theme_minimal()
  (plt_payems_oilsupply)
  
  plt_payems_aggdemand <- ggplot(data = dat_payems_aggdemand) +
    geom_hline(yintercept = 0.0) +
    geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
                alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
    geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
                alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
    geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
    labs(x = "Months", y = "Percent", title = "Aggregate demand shock") +
    # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
    # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
    theme_minimal()
  
  plt_payems_rpoil <- ggplot(data = dat_payems_rpoil) +
    geom_hline(yintercept = 0.0) +
    geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
                alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
    geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
                alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
    geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
    labs(x = "Months", y = "Percent", title = "Oil-specific demand shock") +
    # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
    # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
    theme_minimal()
  
  (plt_payems_oilsupply / plt_payems_aggdemand / plt_payems_rpoil) &
    scale_x_continuous(expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15)) &
    theme_minimal() 
  
}

# Pre 2025-08-27 ----------------------------------------------------------

tar_load(dat_var)

junk <- dat_var |> 
  filter(area_fips == "06029")

ggplot(data = junk) +
  geom_line(mapping = aes(x = date, y = dempl)) +
  geom_line(mapping = aes(x = date, y = dempl_payems), color = "red") 

stopifnot(33==12)

tar_load(plt_kern_analysis)
plt_analysis <- plt_kern_analysis
sol <- plt_analysis$var_sol

var_order <- c("oilsupply", "aggdemand", "rpoil", "dempl")

dat_hd <- plt_analysis$var_dat |> 
  mutate(cum_empl = cumsum(dempl))

date4hd <- dat_hd$date[(sol$p+1):length(dat_hd$date)]

# hd <- compute_hist_decomp(plt_analysis$var_sol, date = plt_analysis$va$date, var_order = var_order) |>
#   mutate(date = date4hd)

hd <- compute_hist_decomp(plt_analysis$var_sol, date = date4hd, var_order = var_order) |>
  mutate(date = date4hd)

dat_hd <- dat_hd |>
  left_join(hd, by = join_by(date))  

date_start <- ymd("1995-02-01")

my_series_levels <- c("Employment growth",  
                      "Less oil-supply shocks",
                      "Less aggregate demand shocks",
                      "Less oil-specific demand shocks",
                      "Less region-specific demand shocks")


junk <- dat_hd |> 
  filter(date >= date_start) |> 
  mutate(check_dempl = hd_series_shock_dempl_dempl + 
           hd_series_shock_dempl_oilsupply + 
           hd_series_shock_dempl_aggdemand +
           hd_series_shock_dempl_rpoil,
         hd_series_shock_dempl_dempl0 = hd_series_shock_dempl_dempl - mean(hd_series_shock_dempl_dempl),
         hd_series_shock_dempl_oilsupply0 = hd_series_shock_dempl_oilsupply - mean(hd_series_shock_dempl_oilsupply),
         hd_series_shock_dempl_aggdemand0 = hd_series_shock_dempl_aggdemand - mean(hd_series_shock_dempl_aggdemand),
         hd_series_shock_dempl_rpoil0 = hd_series_shock_dempl_rpoil - mean(hd_series_shock_dempl_rpoil),
         check_dempl0 = hd_series_shock_dempl_dempl0 + 
           hd_series_shock_dempl_oilsupply0 + 
           hd_series_shock_dempl_aggdemand0 +
           hd_series_shock_dempl_rpoil0,         
         junk = mean(dempl),
         check1 = cumsum(dempl),
         check2 = cumsum(check_dempl),
         check3 = cumsum(dempl) - cumsum(check_dempl),
         rnum = row_number())

mtrend <- lm(dempl ~ rnum, data = junk)

ggplot(data = junk) +
  geom_line(mapping = aes(x = date, y = dempl)) +
  geom_line(mapping = aes(x = date, y = detrendcl(dempl, tt = "linear")), color = "red")

ggplot(data = dat_hd) +
  # geom_line(mapping = aes(x = date, y = dempl), color ="magenta") + 
  # geom_line(mapping = aes(x = date, y = dempl - sol$Vhat[[4]]), color ="red") + 
  geom_line(mapping = aes(x = date, y = detrendcl(dempl, tt = "constant")), alpha = 0.8) + 
  geom_line(mapping = aes(x = date, y = hd_series_shock_dempl_dempl + 
                            hd_series_shock_dempl_oilsupply + 
                            hd_series_shock_dempl_aggdemand + hd_series_shock_dempl_rpoil), color = "blue", alpha = 0.4) 


junk2 <- dat_hd |> 
  filter(date >= ymd("2000-01-01")) |>
  mutate(check_true = detrendcl(dempl, tt = "constant"),
         check_sum = detrendcl(hd_series_shock_dempl_dempl + 
                            hd_series_shock_dempl_oilsupply + 
                            hd_series_shock_dempl_aggdemand + hd_series_shock_dempl_rpoil, tt = "constant"))

ggplot(data = junk2) +
  geom_line(mapping = aes(x = date, y = cumsum(check_true))) +
  geom_line(mapping = aes(x = date, y = cumsum(check_sum)), color = "green")
  # geom_line(mapping = aes(x = date, y = check_true - check_sum), color = "black")
  # geom_line(mapping = aes(x = date, y = check_sum), color = "blue", alpha = 0.4) 

stopifnot(33==12)

ggplot(data = drop_na(dat_hd)) +
  # geom_line(mapping = aes(x = date, y = cumsum(dempl) - rnum * sol$Vhat[[4]]), color = "red") +
  geom_line(mapping = aes(x = date, y = cumsum(detrendcl(dempl, tt = "constant"))), color = "black") + 
  # geom_line(data = junk,
  #           mapping = aes(x = date, y = cumsum(detrendcl(dempl, tt = "constant"))), color = "magenta") + 
  # geom_line(data = junk, mapping = aes(x = date, y = cumsum(hd_series_shock_dempl_dempl + 
  #                           hd_series_shock_dempl_oilsupply + 
  #                           hd_series_shock_dempl_aggdemand + hd_series_shock_dempl_rpoil)), color = "pink") +  
  geom_line(mapping = aes(x = date, y = cumsum(hd_series_shock_dempl_dempl +
                            hd_series_shock_dempl_oilsupply +
                            hd_series_shock_dempl_aggdemand + hd_series_shock_dempl_rpoil)), color = "blue") +
  geom_line(mapping = aes(x = date, y = cumsum(dempl - sol$Vhat[[4]])), color = "green") + 
  ylim(-12, 10)
  

m <- lm(check3 ~ rnum, data = junk)

View(select(junk, date, dempl, check_dempl, check1, check2))

ggplot(data = junk) +
  geom_line(mapping = aes(x = date, y = cumsum(dempl)), color = "black", linewidth = 1.0) + 
  geom_line(mapping = aes(x = date, y = cumsum(check_dempl0 + mean(dempl))), color = "blue", linewidth = 1.0) +
  geom_line(mapping = aes(x = date, y = cumsum(hd_series_shock_dempl_dempl0 + 
                                                 hd_series_shock_dempl_oilsupply0 + 
                                                 hd_series_shock_dempl_aggdemand0 + 
                                                 hd_series_shock_dempl_rpoil0 + mean(dempl))), color = "red", linewidth = 1.0, alpha = 0.6) +
  geom_line(mapping = aes(x = date, y = cumsum(hd_series_shock_dempl_oilsupply0 + 
                                                 hd_series_shock_dempl_aggdemand0 + 
                                                 hd_series_shock_dempl_rpoil0 + mean(dempl))), color = "maroon", linewidth = 1.0, alpha = 0.6) +  
geom_line(mapping = aes(x = date, y = cumsum(hd_series_shock_dempl_dempl0 + 
                                                 hd_series_shock_dempl_oilsupply0 + 
                                                 hd_series_shock_dempl_aggdemand0 + 
                                               mean(dempl))), color = "purple", linewidth = 1.0, alpha = 0.6) +  
  geom_line(mapping = aes(x = date, y = cumsum(junk)), color = "green", linewidth = 1.0) 
  # geom_line(mapping = aes(x = date, y = cumsum(dempl) - cumsum(check_dempl)), color = "green") + 
  # geom_line(mapping = aes(x = date, y = cumsum(dempl + mean(dempl))), color = "green") +
  # geom_line(mapping = aes(x = date, y = cumsum(check_dempl - mean(check_dempl) + mean(dempl))), color = "blue") +
  # geom_line(mapping = aes(x = date, y = cumsum(check_dempl - mean(check_dempl) + mean(dempl)) - cumsum(hd_series_shock_dempl_rpoil)), color = "pink", linewidth = 2.0) 
  # geom_line(mapping = aes(x = date, y = cumsum(check_dempl + mean(dempl))), color = "red") 
  # geom_line(mapping = aes(x = date, y = cumsum(dempl) - cumsum(check_dempl)), color = "red", linetype = "dotted") 

+ 
  geom_vline(xintercept = min(junk$date))

dat_long <- dat_hd |> 
  pivot_longer(cols = -date, names_to = "series", values_to = "dempl") |> 
  mutate(series_dempl = str_split_i(series, "_", 4)) |> 
  filter(series == "dempl" | series_dempl == "dempl") |> 
  arrange(series, date) |> 
  mutate(series_shock = str_split_i(series, "_", 5),
         my_series = case_when(
           series == "dempl" ~ "Employment growth",
           series_shock == "oilsupply" ~ "Less oil-supply shocks",
           series_shock == "aggdemand" ~ "Less aggregate demand shocks",
           series_shock == "rpoil" ~ "Less oil-specific demand shocks",
           series_shock == "dempl" ~ "Less region-specific demand shocks"
         )) |> 
  drop_na(dempl) |> 
  filter(date >= date_start) |> 
  group_by(series) |> 
  mutate(cum_empl_growth = cumsum(dempl))

dat_long_no_shocks <- dat_long |> 
  filter(my_series == "Employment growth") |> 
  ungroup() |> 
  rename(cum_empl_growth_no_shocks = cum_empl_growth) |> 
  select(all_of(c("date", "cum_empl_growth_no_shocks")))

dat_long <- dat_long |> 
  left_join(dat_long_no_shocks, by = join_by(date)) |> 
  mutate(ceg = case_when(
    my_series == "Employment growth" ~ cum_empl_growth,
    .default = cum_empl_growth_no_shocks - cum_empl_growth
  ),
  my_series = factor(my_series, levels = my_series_levels)) 

# Colors
col5 <- sequential_hcl(5, palette = "Mako")

dat_long_lbl <- dat_long |> 
  filter(my_series == "Less oil-specific demand shocks" & date == ymd("2018-01-01"))

ggplot(data = dat_long) +
  geom_line(mapping = aes(x = date, y = ceg, color = my_series, linetype = my_series), linewidth = 1.5, alpha = 0.6) +
  labs(x = "", y = "Percent", color = "", linetype = "", title = plt_analysis$county_name) +
  geom_text_repel(data = dat_long_lbl, mapping = aes(x = date, y = ceg, label = my_series), color = col5[[2]],
                  nudge_y = -10, max.overlaps = Inf) +
  # scale_color_discrete_sequential(palette = "Mako") +
  scale_color_manual(values = c("Employment growth" = "deeppink",
                                "Less oil-supply shocks" = col5[[4]],
                                "Less aggregate demand shocks" = col5[[3]],
                                "Less oil-specific demand shocks" = col5[[2]],
                                "Less region-specific demand shocks" = col5[[1]])) +
  theme_minimal() + theme(legend.position = "inside", legend.position.inside = c(0.2, 0.85),
                          legend.key.width = unit(3, "line"))

# END WORK ON CUMSUM ----------------------------------------------------------------

stopifnot(33==12)
# Merge in data on oil production -----------------------------------------

dat_fred_2keep <- dat_FRED_monthly |> 
  select(date, aggdemand, cpi, payems) |> 
  mutate(dempl_payems = 100 * (log(payems) - lag(log(payems), n = 1, order_by = date)))

dat_empl_wide <- dat_empl_long |> 
  ungroup() |> 
  mutate(county = tolower(str_split_i(area_title, " ", 1))) |> 
  select(-area_title) |> 
  pivot_wider(id_cols = date, names_from = county, values_from = dempl, names_prefix = "dempl_") |> 
  # Join oil production and price
  left_join(dat_fred_2keep, by = join_by(date)) |> 
  left_join(dat_oil_production, by = join_by(date)) |> 
  left_join(dat_oil_price, by = join_by(date)) |> 
  # Generate variables
  mutate(
    oilsupply = 100 * (log(crude_prod_millions_barrels_day) - lag(log(crude_prod_millions_barrels_day), n = 1, order_by = date)),
    rpoil_nodetrend = 100 * log(price_per_barrel / cpi),
    rpoil = detrendcl(rpoil_nodetrend, tt = "constant")
  ) 



# VAR analysis ------------------------------------------------------------

dat_var <- dat_empl_wide |> 
  select(date, oilsupply, aggdemand, rpoil, payems, starts_with("dempl")) |> 
  drop_na() 

plt_oilsupply <- ggplot(data = dat_var) +
  geom_line(mapping = aes(x = date, y = oilsupply))

plt_aggdemand <- ggplot(data = dat_var) +
  geom_line(mapping = aes(x = date, y = aggdemand))

plt_rpoil <- ggplot(data = dat_var) +
  geom_line(mapping = aes(x = date, y = rpoil))

plt_dempl <- ggplot(data = dat_var) +
  geom_line(mapping = aes(x = date, y = dempl_payems))

(plt_oilsupply / plt_aggdemand / plt_rpoil / plt_dempl)


y <- dat_var |> 
  select(oilsupply, aggdemand, rpoil, dempl_kern) |> 
  rename(dempl = dempl_kern) |> 
  as.matrix()

sol <- olsvarc(y, p = 12)

# Parameters for the SVAR model:
var_order <- c("oilsupply", "aggdemand", "rpoil", "dempl")
var_cumsum <- c(
  # Oil-supply responses
  "response_shock_oilsupply_oilsupply",
  "response_shock_oilsupply_aggdemand",
  "response_shock_oilsupply_rpoil",
  "response_shock_oilsupply_dempl",
  # Employment responses
  "response_shock_dempl_oilsupply",
  "response_shock_dempl_aggdemand",
  "response_shock_dempl_rpoil",  
  "response_shock_dempl_dempl"  
)
negative_shocks <- c("oilsupply")

irf <- irfvar(sol$Ahat, 
              p = sol$p,
              B0inv = t(chol(sol$SIGMAhat)), 
              var_order = var_order, negative_shocks = negative_shocks, var_cumsum = var_cumsum, 
              h = 15)

nrep <- 1000
block_length <- 24
dat_irf_ci <- kilianr::bootstrap_mbb(
  olsobj = sol,
  irfobj = irf,
  nrep = nrep,
  blen = block_length,    
  standard_factor = 2.0,
  bootstrap_seed = 676,
  display_progress_bar = TRUE
)

dat_irf_ci1 <- kilianr::bootstrap_mbb(
  olsobj = sol,
  irfobj = irf,
  nrep = nrep,
  blen = block_length,    
  standard_factor = 1.0,
  bootstrap_seed = 676,
  display_progress_bar = TRUE
)  

colors_irf <- colorspace::sequential_hcl(4, palette = "Teal")
color_irf <- colors_irf[1]
color_ci <- colors_irf[3]

linewidth_irf <- 1.0
linewidth_ci <- 0.5

linetype_irf <- "solid"
linetype_ci <- "solid"

alpha_ci <- 0.4
alpha_ci1 <- 0.8

plot_irf_response_shock <- function(response, shock, my_title, my_ytitle, dat_irf, dat_ci, dat_ci1, yintercept_value = 0, ...) {
  
  response_shock_var <- paste("response_shock", response, shock, sep = "_")
  ci_lo <- paste(response_shock_var, "lo", sep = "_")
  ci_hi <- paste(response_shock_var, "hi", sep = "_")
  
  plt <- ggplot(data = dat_irf) +
    geom_hline(yintercept = yintercept_value) +  
    geom_ribbon(data = dat_irf_ci, mapping = aes(x = horizon, ymin = .data[[ci_lo]], ymax = .data[[ci_hi]]),
                alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
    geom_ribbon(data = dat_irf_ci1, mapping = aes(x = horizon, ymin = .data[[ci_lo]], ymax = .data[[ci_hi]]),
                alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
    geom_line(mapping = aes(x = .data[["horizon"]], y = .data[[response_shock_var]]), color = color_irf) +
    labs(title = my_title, y = my_ytitle)   
  
  if (length(list(...)) > 0) {
    plt <- plt + scale_y_continuous(limits = my_ylim, breaks = my_ybreaks)
  }
  
  return(plt)
}

my_ytitle_oilsupply <- "Oil production"
my_ytitle_aggdemand <- "Real economic activity"
my_ytitle_rpoil <- "Real price of oil"
# my_ytitle_dempl <- "Employment in oil-dependent counties"
my_ytitle_dempl <- "Employment"

# Negative oil-supply shock
plt_oilsupply_oilsupply <- plot_irf_response_shock(response = "oilsupply", shock = "oilsupply", 
                                                   my_title = "Oil supply shock", my_ytitle = my_ytitle_oilsupply, 
                                                   dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                   yintercept_value = 0, my_ylim = c(-2.0, 0.5), my_ybreaks = seq(-2.0, 0.5, 0.5)) 

plt_aggdemand_oilsupply <- plot_irf_response_shock(response = "aggdemand", shock = "oilsupply", 
                                                   my_title = "Oil supply shock", my_ytitle = my_ytitle_aggdemand, 
                                                   dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                   yintercept_value = 0) 

plt_rpoil_oilsupply <- plot_irf_response_shock(response = "rpoil", shock = "oilsupply", 
                                               my_title = "Oil supply shock", my_ytitle = my_ytitle_rpoil, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_dempl_oilsupply <- plot_irf_response_shock(response = "dempl", shock = "oilsupply", 
                                               my_title = "Oil supply shock", my_ytitle = my_ytitle_dempl, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 
# Aggregate-demand shock
my_title_aggdemand <- "Aggregate demand shock"
plt_oilsupply_aggdemand <- plot_irf_shock_response(response = "oilsupply", shock = "aggdemand", 
                                                   my_title = my_title_aggdemand, my_ytitle = my_ytitle_oilsupply, 
                                                   dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                   yintercept_value = 0) 

plt_aggdemand_aggdemand <- plot_irf_response_shock(response = "aggdemand", shock = "aggdemand", 
                                                   my_title = my_title_aggdemand, my_ytitle = my_ytitle_aggdemand, 
                                                   dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                   yintercept_value = 0) 

plt_rpoil_aggdemand <- plot_irf_response_shock(response = "rpoil", shock = "aggdemand", 
                                               my_title = my_title_aggdemand, my_ytitle = my_ytitle_rpoil, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_dempl_aggdemand <- plot_irf_response_shock("dempl", "aggdemand", 
                                               my_title = my_title_aggdemand, my_ytitle = my_ytitle_dempl, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

# Real-price-of-oil shock
my_title_rpoil <- "Oil-specific demand shock"
plt_oilsupply_rpoil <- plot_irf_shock_response(response = "oilsupply", shock = "rpoil", 
                                               my_title = my_title_rpoil, my_ytitle = my_ytitle_oilsupply, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_aggdemand_rpoil <- plot_irf_response_shock(response = "aggdemand", shock = "rpoil", 
                                               my_title = my_title_rpoil, my_ytitle = my_ytitle_aggdemand, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_rpoil_rpoil <- plot_irf_response_shock(response = "rpoil", shock = "rpoil", 
                                           my_title = my_title_rpoil, my_ytitle = my_ytitle_rpoil, 
                                           dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                           yintercept_value = 0) 

plt_dempl_rpoil <- plot_irf_response_shock("dempl", "rpoil", 
                                           my_title = my_title_rpoil, my_ytitle = my_ytitle_dempl, 
                                           dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                           yintercept_value = 0) 


# Employment-demand shock
my_title_dempl <- "Regional demand shock"
plt_oilsupply_dempl <- plot_irf_shock_response(response = "oilsupply", shock = "dempl", 
                                               my_title = my_title_dempl, my_ytitle = my_ytitle_oilsupply, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_aggdemand_dempl <- plot_irf_response_shock(response = "aggdemand", shock = "dempl", 
                                               my_title = my_title_dempl, my_ytitle = my_ytitle_aggdemand, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_rpoil_dempl <- plot_irf_response_shock(response = "rpoil", shock = "dempl", 
                                           my_title = my_title_dempl, my_ytitle = my_ytitle_rpoil, 
                                           dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                           yintercept_value = 0) 

plt_dempl_dempl <- plot_irf_response_shock("dempl", "dempl", 
                                           my_title = my_title_dempl, my_ytitle = my_ytitle_dempl, 
                                           dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                           yintercept_value = 0) 


(plt_oilsupply_oilsupply | plt_aggdemand_oilsupply | plt_rpoil_oilsupply | plt_dempl_oilsupply) /
  (plt_oilsupply_aggdemand | plt_aggdemand_aggdemand | plt_rpoil_aggdemand | plt_dempl_aggdemand) /
  (plt_oilsupply_rpoil | plt_aggdemand_rpoil | plt_rpoil_rpoil | plt_dempl_rpoil) /
  (plt_oilsupply_dempl | plt_aggdemand_dempl | plt_rpoil_dempl | plt_dempl_dempl) &  
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15)) &
  theme_minimal() &
  theme(
    panel.grid.minor.x = element_blank()
  )


ggsave(here("out", "fig_irf-kern.pdf"), width = mywidth, height = myheight)


# Second-stage IRF --------------------------------------------------------

tar_load(dat_fred_monthly)
tar_load(dat_empl_qcew_cps)

dat_fred <- dat_fred_monthly |> 
  mutate(dempl_payems = 100 * (log(payems) - lag(log(payems), n = 1, order_by = date))) |> 
  select(date, dempl_payems)

tar_load(plt_kern_analysis)

sol <- plt_kern_analysis$var_sol
var_dat <- plt_kern_analysis$var_dat

tt <- nrow(sol$y)
pp <- sol$p

B0inv <- t(chol(sol$SIGMAhat))
B0 <- solve(B0inv)

Ehat <- B0 %*% sol$Uhat

var_names <- rownames(Ehat)
vars_select <- paste0("shock_", var_names)

dat2 <- as_tibble(t(Ehat)) |>
  rename_with(.fn = function(x) paste0("shock_", x), .cols = everything()) |>
  mutate(date = dat_var$date[(pp+1):tt],
         year = year(date),
         month = month(date)) 

dat2 <- dat2 |> 
  left_join(dat_fred, by = join_by(date))


dat_payems_oilsupply <- stage2irf(y = dat2$dempl_payems, x = -dat2$shock_oilsupply, 
                                  p = 15, block_length = 12, 
                                  nrep = 1000, 
                                  standard_deviation_factor = 1, 
                                  boot_seed = 676, cumeffect = TRUE) |> 
  mutate(
    irfstd = irf2 - irf2_lo,
    irf2_lolo = irf2 - 2 * irfstd,
    irf2_hihi = irf2 + 2 * irfstd
  )

dat_payems_aggdemand <- stage2irf(y = dat2$dempl_payems, x = -dat2$shock_aggdemand, 
                                  p = 15, block_length = 12, 
                                  nrep = 1000, 
                                  standard_deviation_factor = 1, 
                                  boot_seed = 676, cumeffect = TRUE) |> 
  mutate(
    irfstd = irf2 - irf2_lo,
    irf2_lolo = irf2 - 2 * irfstd,
    irf2_hihi = irf2 + 2 * irfstd
  )

dat_payems_rpoil <- stage2irf(y = dat2$dempl_payems, x = -dat2$shock_rpoil, 
                              p = 15, block_length = 12, 
                              nrep = 1000, 
                              standard_deviation_factor = 1, 
                              boot_seed = 676, cumeffect = TRUE) |> 
  mutate(
    irfstd = irf2 - irf2_lo,
    irf2_lolo = irf2 - 2 * irfstd,
    irf2_hihi = irf2 + 2 * irfstd
  )


colors_irf <- colorspace::sequential_hcl(4, palette = "Sunset")
color_irf <- colors_irf[1]
color_ci <- colors_irf[3]

linewidth_irf <- 1.0
linewidth_ci <- 0.5

linetype_irf <- "solid"
linetype_ci <- "solid"

alpha_ci <- 0.4
alpha_ci1 <- 0.8

plt_payems_oilsupply <- ggplot(data = dat_payems_oilsupply) +
  geom_hline(yintercept = 0.0) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
              alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
              alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
  geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
  labs(x = "Months", y = "Percent", title = "Crude-oil supply shock") +
  # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
  # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
  theme_minimal()
(plt_payems_oilsupply)

plt_payems_aggdemand <- ggplot(data = dat_payems_aggdemand) +
  geom_hline(yintercept = 0.0) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
              alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
              alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
  geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
  labs(x = "Months", y = "Percent", title = "Aggregate demand shock") +
  # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
  # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
  theme_minimal()

plt_payems_rpoil <- ggplot(data = dat_payems_rpoil) +
  geom_hline(yintercept = 0.0) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
              alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
              alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
  geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
  labs(x = "Months", y = "Percent", title = "Oil-specific demand shock") +
  # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
  # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
  theme_minimal()

(plt_payems_oilsupply / plt_payems_aggdemand / plt_payems_rpoil) &
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15)) &
  theme_minimal() 

stopifnot(33==12)

# /
#   (plt_oilsupply_aggdemand | plt_aggdemand_aggdemand | plt_rpoil_aggdemand) /
#   (plt_oilsupply_rpoil | plt_aggdemand_rpoil | plt_rpoil_rpoil) &
#   scale_x_continuous(expand = c(0, 0)) &
#   theme_minimal() &
#   theme(
#     plot.title = element_text(size = 18),
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),      
#     axis.text.y = element_text(size = 10),
#     axis.text.x = element_text(size = 10)
#   )

stopifnot(33==12)

# Plot seasonal adjustment ------------------------------------------------

ggplot(data = dat) +
  geom_line(mapping = aes(x = date, y = log_empl_nsa), color = "black") +
  geom_line(mapping = aes(x = date, y = log_empl), color = "red") 


ggplot(data = dat) +
  geom_line(mapping = aes(x = date, y = rpoil)) +
  geom_line(mapping = aes(x = date, y = rpoil_nodetrend), color =  "red") 

ggplot(data = dat) +
  geom_line(mapping = aes(x = date, y = aggdemand)) +
  geom_line(mapping = aes(x = date, y = detrendcl(aggdemand, tt = "linear")), color = "red")
=======
# Merge in data on oil production -----------------------------------------

dat_fred_2keep <- dat_FRED_monthly |> 
  select(date, aggdemand, cpi, payems) |> 
  mutate(dempl_payems = 100 * (log(payems) - lag(log(payems), n = 1, order_by = date)))

dat_empl_wide <- dat_empl_long |> 
  ungroup() |> 
  mutate(county = tolower(str_split_i(area_title, " ", 1))) |> 
  select(-area_title) |> 
  pivot_wider(id_cols = date, names_from = county, values_from = dempl, names_prefix = "dempl_") |> 
  # Join oil production and price
  left_join(dat_fred_2keep, by = join_by(date)) |> 
  left_join(dat_oil_production, by = join_by(date)) |> 
  left_join(dat_oil_price, by = join_by(date)) |> 
  # Generate variables
  mutate(
    oilsupply = 100 * (log(crude_prod_millions_barrels_day) - lag(log(crude_prod_millions_barrels_day), n = 1, order_by = date)),
    rpoil_nodetrend = 100 * log(price_per_barrel / cpi),
    rpoil = detrendcl(rpoil_nodetrend, tt = "constant")
  ) 



# VAR analysis ------------------------------------------------------------

dat_var <- dat_empl_wide |> 
  select(date, oilsupply, aggdemand, rpoil, payems, starts_with("dempl")) |> 
  drop_na() 

plt_oilsupply <- ggplot(data = dat_var) +
  geom_line(mapping = aes(x = date, y = oilsupply))

plt_aggdemand <- ggplot(data = dat_var) +
  geom_line(mapping = aes(x = date, y = aggdemand))

plt_rpoil <- ggplot(data = dat_var) +
  geom_line(mapping = aes(x = date, y = rpoil))

plt_dempl <- ggplot(data = dat_var) +
  geom_line(mapping = aes(x = date, y = dempl_payems))

(plt_oilsupply / plt_aggdemand / plt_rpoil / plt_dempl)


y <- dat_var |> 
  select(oilsupply, aggdemand, rpoil, dempl_kern) |> 
  rename(dempl = dempl_kern) |> 
  as.matrix()

sol <- olsvarc(y, p = 12)

# Parameters for the SVAR model:
var_order <- c("oilsupply", "aggdemand", "rpoil", "dempl")
var_cumsum <- c(
  # Oil-supply responses
  "response_shock_oilsupply_oilsupply",
  "response_shock_oilsupply_aggdemand",
  "response_shock_oilsupply_rpoil",
  "response_shock_oilsupply_dempl",
  # Employment responses
  "response_shock_dempl_oilsupply",
  "response_shock_dempl_aggdemand",
  "response_shock_dempl_rpoil",  
  "response_shock_dempl_dempl"  
)
negative_shocks <- c("oilsupply")

irf <- irfvar(sol$Ahat, 
              p = sol$p,
              B0inv = t(chol(sol$SIGMAhat)), 
              var_order = var_order, negative_shocks = negative_shocks, var_cumsum = var_cumsum, 
              h = 15)

nrep <- 1000
block_length <- 24
dat_irf_ci <- kilianr::bootstrap_mbb(
  olsobj = sol,
  irfobj = irf,
  nrep = nrep,
  blen = block_length,    
  standard_factor = 2.0,
  bootstrap_seed = 676,
  display_progress_bar = TRUE
)

dat_irf_ci1 <- kilianr::bootstrap_mbb(
  olsobj = sol,
  irfobj = irf,
  nrep = nrep,
  blen = block_length,    
  standard_factor = 1.0,
  bootstrap_seed = 676,
  display_progress_bar = TRUE
)  

colors_irf <- colorspace::sequential_hcl(4, palette = "Teal")
color_irf <- colors_irf[1]
color_ci <- colors_irf[3]

linewidth_irf <- 1.0
linewidth_ci <- 0.5

linetype_irf <- "solid"
linetype_ci <- "solid"

alpha_ci <- 0.4
alpha_ci1 <- 0.8

plot_irf_response_shock <- function(response, shock, my_title, my_ytitle, dat_irf, dat_ci, dat_ci1, yintercept_value = 0, ...) {
  
  response_shock_var <- paste("response_shock", response, shock, sep = "_")
  ci_lo <- paste(response_shock_var, "lo", sep = "_")
  ci_hi <- paste(response_shock_var, "hi", sep = "_")
  
  plt <- ggplot(data = dat_irf) +
    geom_hline(yintercept = yintercept_value) +  
    geom_ribbon(data = dat_irf_ci, mapping = aes(x = horizon, ymin = .data[[ci_lo]], ymax = .data[[ci_hi]]),
                alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
    geom_ribbon(data = dat_irf_ci1, mapping = aes(x = horizon, ymin = .data[[ci_lo]], ymax = .data[[ci_hi]]),
                alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
    geom_line(mapping = aes(x = .data[["horizon"]], y = .data[[response_shock_var]]), color = color_irf) +
    labs(title = my_title, y = my_ytitle)   
  
  if (length(list(...)) > 0) {
    plt <- plt + scale_y_continuous(limits = my_ylim, breaks = my_ybreaks)
  }
  
  return(plt)
}

my_ytitle_oilsupply <- "Oil production"
my_ytitle_aggdemand <- "Real economic activity"
my_ytitle_rpoil <- "Real price of oil"
# my_ytitle_dempl <- "Employment in oil-dependent counties"
my_ytitle_dempl <- "Employment"

# Negative oil-supply shock
plt_oilsupply_oilsupply <- plot_irf_response_shock(response = "oilsupply", shock = "oilsupply", 
                                                   my_title = "Oil supply shock", my_ytitle = my_ytitle_oilsupply, 
                                                   dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                   yintercept_value = 0, my_ylim = c(-2.0, 0.5), my_ybreaks = seq(-2.0, 0.5, 0.5)) 

plt_aggdemand_oilsupply <- plot_irf_response_shock(response = "aggdemand", shock = "oilsupply", 
                                                   my_title = "Oil supply shock", my_ytitle = my_ytitle_aggdemand, 
                                                   dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                   yintercept_value = 0) 

plt_rpoil_oilsupply <- plot_irf_response_shock(response = "rpoil", shock = "oilsupply", 
                                               my_title = "Oil supply shock", my_ytitle = my_ytitle_rpoil, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_dempl_oilsupply <- plot_irf_response_shock(response = "dempl", shock = "oilsupply", 
                                               my_title = "Oil supply shock", my_ytitle = my_ytitle_dempl, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 
# Aggregate-demand shock
my_title_aggdemand <- "Aggregate demand shock"
plt_oilsupply_aggdemand <- plot_irf_shock_response(response = "oilsupply", shock = "aggdemand", 
                                                   my_title = my_title_aggdemand, my_ytitle = my_ytitle_oilsupply, 
                                                   dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                   yintercept_value = 0) 

plt_aggdemand_aggdemand <- plot_irf_response_shock(response = "aggdemand", shock = "aggdemand", 
                                                   my_title = my_title_aggdemand, my_ytitle = my_ytitle_aggdemand, 
                                                   dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                   yintercept_value = 0) 

plt_rpoil_aggdemand <- plot_irf_response_shock(response = "rpoil", shock = "aggdemand", 
                                               my_title = my_title_aggdemand, my_ytitle = my_ytitle_rpoil, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_dempl_aggdemand <- plot_irf_response_shock("dempl", "aggdemand", 
                                               my_title = my_title_aggdemand, my_ytitle = my_ytitle_dempl, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

# Real-price-of-oil shock
my_title_rpoil <- "Oil-specific demand shock"
plt_oilsupply_rpoil <- plot_irf_shock_response(response = "oilsupply", shock = "rpoil", 
                                               my_title = my_title_rpoil, my_ytitle = my_ytitle_oilsupply, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_aggdemand_rpoil <- plot_irf_response_shock(response = "aggdemand", shock = "rpoil", 
                                               my_title = my_title_rpoil, my_ytitle = my_ytitle_aggdemand, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_rpoil_rpoil <- plot_irf_response_shock(response = "rpoil", shock = "rpoil", 
                                           my_title = my_title_rpoil, my_ytitle = my_ytitle_rpoil, 
                                           dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                           yintercept_value = 0) 

plt_dempl_rpoil <- plot_irf_response_shock("dempl", "rpoil", 
                                           my_title = my_title_rpoil, my_ytitle = my_ytitle_dempl, 
                                           dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                           yintercept_value = 0) 


# Employment-demand shock
my_title_dempl <- "Regional demand shock"
plt_oilsupply_dempl <- plot_irf_shock_response(response = "oilsupply", shock = "dempl", 
                                               my_title = my_title_dempl, my_ytitle = my_ytitle_oilsupply, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_aggdemand_dempl <- plot_irf_response_shock(response = "aggdemand", shock = "dempl", 
                                               my_title = my_title_dempl, my_ytitle = my_ytitle_aggdemand, 
                                               dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                               yintercept_value = 0) 

plt_rpoil_dempl <- plot_irf_response_shock(response = "rpoil", shock = "dempl", 
                                           my_title = my_title_dempl, my_ytitle = my_ytitle_rpoil, 
                                           dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                           yintercept_value = 0) 

plt_dempl_dempl <- plot_irf_response_shock("dempl", "dempl", 
                                           my_title = my_title_dempl, my_ytitle = my_ytitle_dempl, 
                                           dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                           yintercept_value = 0) 


(plt_oilsupply_oilsupply | plt_aggdemand_oilsupply | plt_rpoil_oilsupply | plt_dempl_oilsupply) /
  (plt_oilsupply_aggdemand | plt_aggdemand_aggdemand | plt_rpoil_aggdemand | plt_dempl_aggdemand) /
  (plt_oilsupply_rpoil | plt_aggdemand_rpoil | plt_rpoil_rpoil | plt_dempl_rpoil) /
  (plt_oilsupply_dempl | plt_aggdemand_dempl | plt_rpoil_dempl | plt_dempl_dempl) &  
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15)) &
  theme_minimal() &
  theme(
    panel.grid.minor.x = element_blank()
  )


ggsave(here("out", "fig_irf-kern.pdf"), width = mywidth, height = myheight)


# Second-stage IRF --------------------------------------------------------

tar_load(dat_fred_monthly)
tar_load(dat_empl_qcew_cps)

dat_fred <- dat_fred_monthly |> 
  mutate(dempl_payems = 100 * (log(payems) - lag(log(payems), n = 1, order_by = date))) |> 
  select(date, dempl_payems)

tar_load(plt_kern_analysis)

sol <- plt_kern_analysis$var_sol
var_dat <- plt_kern_analysis$var_dat

tt <- nrow(sol$y)
pp <- sol$p

B0inv <- t(chol(sol$SIGMAhat))
B0 <- solve(B0inv)

Ehat <- B0 %*% sol$Uhat

var_names <- rownames(Ehat)
vars_select <- paste0("shock_", var_names)

dat2 <- as_tibble(t(Ehat)) |>
  rename_with(.fn = function(x) paste0("shock_", x), .cols = everything()) |>
  mutate(date = dat_var$date[(pp+1):tt],
         year = year(date),
         month = month(date)) 

dat2 <- dat2 |> 
  left_join(dat_fred, by = join_by(date))


dat_payems_oilsupply <- stage2irf(y = dat2$dempl_payems, x = -dat2$shock_oilsupply, 
                                  p = 15, block_length = 12, 
                                  nrep = 1000, 
                                  standard_deviation_factor = 1, 
                                  boot_seed = 676, cumeffect = TRUE) |> 
  mutate(
    irfstd = irf2 - irf2_lo,
    irf2_lolo = irf2 - 2 * irfstd,
    irf2_hihi = irf2 + 2 * irfstd
  )

dat_payems_aggdemand <- stage2irf(y = dat2$dempl_payems, x = -dat2$shock_aggdemand, 
                                  p = 15, block_length = 12, 
                                  nrep = 1000, 
                                  standard_deviation_factor = 1, 
                                  boot_seed = 676, cumeffect = TRUE) |> 
  mutate(
    irfstd = irf2 - irf2_lo,
    irf2_lolo = irf2 - 2 * irfstd,
    irf2_hihi = irf2 + 2 * irfstd
  )

dat_payems_rpoil <- stage2irf(y = dat2$dempl_payems, x = -dat2$shock_rpoil, 
                              p = 15, block_length = 12, 
                              nrep = 1000, 
                              standard_deviation_factor = 1, 
                              boot_seed = 676, cumeffect = TRUE) |> 
  mutate(
    irfstd = irf2 - irf2_lo,
    irf2_lolo = irf2 - 2 * irfstd,
    irf2_hihi = irf2 + 2 * irfstd
  )


colors_irf <- colorspace::sequential_hcl(4, palette = "Sunset")
color_irf <- colors_irf[1]
color_ci <- colors_irf[3]

linewidth_irf <- 1.0
linewidth_ci <- 0.5

linetype_irf <- "solid"
linetype_ci <- "solid"

alpha_ci <- 0.4
alpha_ci1 <- 0.8

plt_payems_oilsupply <- ggplot(data = dat_payems_oilsupply) +
  geom_hline(yintercept = 0.0) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
              alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
              alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
  geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
  labs(x = "Months", y = "Percent", title = "Crude-oil supply shock") +
  # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
  # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
  theme_minimal()
(plt_payems_oilsupply)

plt_payems_aggdemand <- ggplot(data = dat_payems_aggdemand) +
  geom_hline(yintercept = 0.0) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
              alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
              alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
  geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
  labs(x = "Months", y = "Percent", title = "Aggregate demand shock") +
  # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
  # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
  theme_minimal()

plt_payems_rpoil <- ggplot(data = dat_payems_rpoil) +
  geom_hline(yintercept = 0.0) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
              alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
  geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
              alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +  
  geom_line(mapping = aes(x = horizon, irf2), color = color_irf) +  
  labs(x = "Months", y = "Percent", title = "Oil-specific demand shock") +
  # scale_x_continuous(breaks = seq(0, 12, by = 2), expand = c(0, 0)) +
  # scale_y_continuous(breaks = seq(-20, 30, by = 10)) +    
  theme_minimal()

(plt_payems_oilsupply / plt_payems_aggdemand / plt_payems_rpoil) &
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15)) &
  theme_minimal() 

stopifnot(33==12)

# /
#   (plt_oilsupply_aggdemand | plt_aggdemand_aggdemand | plt_rpoil_aggdemand) /
#   (plt_oilsupply_rpoil | plt_aggdemand_rpoil | plt_rpoil_rpoil) &
#   scale_x_continuous(expand = c(0, 0)) &
#   theme_minimal() &
#   theme(
#     plot.title = element_text(size = 18),
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),      
#     axis.text.y = element_text(size = 10),
#     axis.text.x = element_text(size = 10)
#   )

stopifnot(33==12)

# Plot seasonal adjustment ------------------------------------------------

ggplot(data = dat) +
  geom_line(mapping = aes(x = date, y = log_empl_nsa), color = "black") +
  geom_line(mapping = aes(x = date, y = log_empl), color = "red") 


ggplot(data = dat) +
  geom_line(mapping = aes(x = date, y = rpoil)) +
  geom_line(mapping = aes(x = date, y = rpoil_nodetrend), color =  "red") 

ggplot(data = dat) +
  geom_line(mapping = aes(x = date, y = aggdemand)) +
  geom_line(mapping = aes(x = date, y = detrendcl(aggdemand, tt = "linear")), color = "red")
>>>>>>> a9c30120494184278bbeaddab08b704c796e4388
