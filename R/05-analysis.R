plot_supersector <- function(df, date_label) {
  # df = Data frame with employment level and shares
  # date_label = date to late the series using geom_label_repel
  
  df_label <- df |> 
    filter(date == date_label)
  
  plt_lvl <- ggplot(data = df) +
    geom_line(mapping = aes(x = date, y = empl, color = supersector_title, linetype = supersector_title), linewidth = 1.0) +
    geom_label_repel(data = df_label, mapping = aes(x = date, y = empl, label = supersector_title, color = supersector_title)) +
    labs(x = "", y = "Employment") +
    scale_y_continuous(label = scales::comma) +
    colorspace::scale_color_discrete_qualitative() +
    guides(color = "none", linetype = "none") +
    theme_minimal()
  
  plt_share <- ggplot(data = df) +
    geom_line(mapping = aes(x = date, y = empl_share, color = supersector_title, linetype = supersector_title), linewidth = 1.0) +
    geom_label_repel(data = df_label, mapping = aes(x = date, y = empl_share, label = supersector_title, color = supersector_title)) +
    labs(x = "", y = "Employment") +
    scale_y_continuous(label = scales::comma) +
    colorspace::scale_color_discrete_qualitative() +
    guides(color = "none", linetype = "none") +
    theme_minimal()
  
  return(list(plt_lvl = plt_lvl, plt_share = plt_share))
}

plot_employment <- function(dat_empl_qcew_cps, dat_recess) {
  dat_lbl <- dat_empl_qcew_cps |> 
    group_by(area_title) |> 
    filter(date == max(date) | date == min(date))
  
  ggplot(data = dat_empl_qcew_cps) +
    geom_rect(
      data = filter(dat_recess,
                    begin >= min(dat_empl_qcew_cps$date),
                    begin <= max(dat_empl_qcew_cps$date)),
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(mapping = aes(x = date, y = empl / 1000, color = area_title)) +
    geom_line(mapping = aes(x = date, y = empl_cps / 1000), color = "black") +
    geom_text_repel(data = dat_lbl,
                    mapping = aes(x = date, y = empl / 1000, 
                                  label = paste0(format(round(empl / 1000, 0), big.mark = ",")),
                                  color = area_title)) +
    facet_wrap(area_title ~ ., scales = "free_y", ncol = 1) +
    theme_minimal() +
    xlab("") + ylab("Thousands of employed persons") + labs(caption = "Black lines show employment measured by the CPS") +
    scale_color_discrete_qualitative(palette = "Dark 3") +
    guides(color = "none")   
}

plot_employment_ind <- function(dat_empl_qcew_ind, dat_recess) {
  dat_lbl <- dat_empl_qcew_ind |> 
    filter(industry_code == 21) |> 
    group_by(area_title, my_industry_title) |> 
    filter(date == max(date))
  
  ggplot(data = dat_empl_qcew_ind) +
    geom_rect(
      data = filter(dat_recess,
                    begin >= min(dat_empl_qcew_ind$date),
                    begin <= max(dat_empl_qcew_ind$date)),
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(mapping = aes(x = date, y = empl_nsa, 
                            # linetype = my_industry_title,
                            color = my_industry_title),
              linewidth = 1.0) +
    geom_text_repel(data = dat_lbl,
                    mapping = aes(x = date, y = empl_nsa,
                                  label = paste0(format(round(empl_nsa, 0), big.mark = ","))),
                    color = "black") +
    facet_wrap(area_title ~ ., scales = "free_y", ncol = 1) +
    scale_y_continuous(label = scales::comma) +
    theme_minimal() +
    xlab("") + ylab("Employed persons") + 
    scale_color_discrete_sequential(palette = "Plasma") +
    guides(color = guide_legend(title = "NAICS title"),
           linetype = guide_legend(title = "NAICS title"))
}

plot_employment_share <- function(dat_empl_qcew_ind, dat_recess) {
  dat_lbl <- dat_empl_qcew_ind |> 
    filter(industry_code == 21) |> 
    group_by(area_title, my_industry_title) |> 
    filter(date == max(date))
  
  plt_all <- ggplot(data = dat_empl_qcew_ind) +
    geom_rect(
      data = filter(dat_recess,
                    begin >= min(dat_empl_qcew_ind$date),
                    begin <= max(dat_empl_qcew_ind$date)),
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(mapping = aes(x = date, y = empl_share_nsa, 
                            # linetype = my_industry_title,
                            color = my_industry_title),
              linewidth = 1.0) +
    geom_text_repel(data = dat_lbl,
                    mapping = aes(x = date, y = empl_share_nsa,
                                  label = paste0(format(round(empl_share_nsa, 0)))),
                    color = "black") +
    facet_wrap(area_title ~ ., scales = "free_y", ncol = 1) +
    scale_y_continuous(label = scales::comma) +
    theme_minimal() +
    xlab("") + ylab("Share of total employment") + 
    scale_color_discrete_sequential(palette = "Plasma") +
    guides(color = guide_legend(title = "NAICS title"),
           linetype = guide_legend(title = "NAICS title"))
  
  # Kern County, California
  dat_lbl_kern <- dat_empl_qcew_ind |> 
    filter(area_title == "Kern County, California") |> 
    group_by(my_industry_title) |> 
    filter(date == max(date) | date == min(date))
  
  dat_empl_qcew_ind_kern <- dat_empl_qcew_ind |> 
    filter(area_title == "Kern County, California")
  
  plt_kern <- ggplot(data = dat_empl_qcew_ind_kern) +
    geom_rect(
      data = filter(dat_recess,
                    begin >= min(dat_empl_qcew_ind$date),
                    begin <= max(dat_empl_qcew_ind$date)),
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(mapping = aes(x = date, y = empl_share_nsa, 
                            # linetype = my_industry_title,
                            color = my_industry_title),
              linewidth = 1.0) +
    geom_text_repel(data = dat_lbl_kern,
                    mapping = aes(x = date, y = empl_share_nsa,
                                  label = paste0(format(round(empl_share_nsa, 2)))),
                    color = "black") +
    scale_y_continuous(label = scales::comma) +
    theme_minimal() +
    xlab("") + ylab("Share of total employment") + 
    scale_color_discrete_sequential(palette = "Plasma") +
    guides(color = guide_legend(title = "NAICS title", position = "inside"),
           linetype = guide_legend(title = "NAICS title", position = "inside")) +
    theme(legend.justification.inside = c(0.9, 0.9))
  
  return(list(plt_all = plt_all, plt_kern = plt_kern))  
}

plot_establishments_total <- function(dat_estabs_count, dat_recess) {
  
  dat_estabs_count <- dat_estabs_count |> 
    filter(industry_code == 10)
  
  dat_lbl <- dat_estabs_count |> 
    group_by(area_title) |> 
    filter(date == max(date) | date == min(date))
  
  ggplot(data = dat_estabs_count) +
    geom_rect(
      data = filter(dat_recess,
                    begin >= min(dat_estabs_count$date),
                    begin <= max(dat_estabs_count$date)),
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(mapping = aes(x = date, y = qtrly_estabs_count, color = area_title)) +
    geom_text_repel(data = dat_lbl,
                    mapping = aes(x = date, y = qtrly_estabs_count, 
                                  label = paste0(format(round(qtrly_estabs_count, 0), big.mark = ",")))) +
    scale_y_continuous(label = scales::comma) +
    facet_wrap(area_title ~ ., scales = "free_y", ncol = 1) +
    theme_minimal() +
    xlab("") + ylab("Establishments") +
    scale_color_discrete_qualitative(palette = "Dark 3") +
    guides(color = "none") 
}

plot_establishments_ind <- function(dat_estabs_count, dat_recess) {
  
  dat_estab_oil_gas <- dat_estabs_count |> 
    filter(industry_code != 10) 
  
  dat_lbl <- dat_estab_oil_gas |> 
    filter(industry_code == max(industry_code)) |> 
    group_by(area_title) |> 
    filter(date == max(date))
  
  ggplot(data = dat_estab_oil_gas) +
    geom_rect(
      data = filter(dat_recess,
                    begin >= min(dat_estab_oil_gas$date),
                    begin <= max(dat_estab_oil_gas$date)),
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(mapping = aes(x = date, y = qtrly_estabs_count, color = my_industry_title)) +
    geom_text_repel(data = dat_lbl,
                    mapping = aes(x = date, y = qtrly_estabs_count, 
                                  label = paste0(format(round(qtrly_estabs_count, 0), big.mark = ",")))) +
    scale_y_continuous(label = scales::comma) +
    facet_wrap(area_title ~ ., scales = "free_y", ncol = 1) +
    theme_minimal() +
    xlab("") + ylab("Establishments") +
    scale_color_discrete_qualitative(palette = "Dark 3") +
    guides(color = "none") 
}

plot_dempl <- function(dat_empl_qcew_cps, dat_recess) {
  ggplot(data = dat_empl_qcew_cps) +
    geom_rect(
      data = filter(dat_recess,
                    begin >= min(dat_empl_qcew_cps$date),
                    begin <= max(dat_empl_qcew_cps$date)),
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(mapping = aes(x = date, y = dempl, color = area_title)) +
    facet_wrap(area_title ~ ., scales = "free_y", ncol = 1) +
    theme_minimal() +
    xlab("") + ylab("Thousands of employed persons") +
    scale_color_discrete_qualitative(palette = "Dark 3") +
    scale_x_date(expand = c(0, 0)) +
    guides(color = "none")   
}

plot_wage_share <- function(dat_qtrly_wages, dat_recess) {
  
  dat_wages_ind <- dat_qtrly_wages |> 
    filter(industry_code > 10)
  
  dat_lbl <- dat_wages_ind |> 
    filter(industry_code == 21) |> 
    group_by(area_title) |> 
    filter(date == max(date))
  
  ggplot(data = dat_wages_ind) +
    geom_rect(
      data = filter(dat_recess,
                    begin >= min(dat_wages_ind$date),
                    begin <= max(dat_wages_ind$date)),
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(mapping = aes(x = date, y = share_ind_wages, color = my_industry_title)) +
    geom_text_repel(data = dat_lbl,
                    mapping = aes(x = date, y = share_ind_wages,
                                  label = paste0(format(round(share_ind_wages, 0)))),
                    color = "black") +
    facet_wrap(area_title ~ ., scales = "free_y", ncol = 1) +
    theme_minimal() +
    xlab("") + ylab("Percent") +
    scale_color_discrete_sequential(palette = "Plasma") +
    guides(color = guide_legend(title = "NAICS title"))
}

plot_irf_response_shock <- function(response, shock, my_title, my_ytitle, dat_irf, dat_ci, dat_ci1, alpha_ci, alpha_ci1, color_ci, linetype_ci, linewidth_ci, color_irf, my_xtitle = "", yintercept_value = 0, my_ylim = NULL, my_ybreaks = NULL) {
  
  response_shock_var <- paste("response_shock", response, shock, sep = "_")
  ci_lo <- paste(response_shock_var, "lo", sep = "_")
  ci_hi <- paste(response_shock_var, "hi", sep = "_")
  
  plt <- ggplot(data = dat_irf) +
    geom_hline(yintercept = yintercept_value) +  
    geom_ribbon(data = dat_ci, mapping = aes(x = horizon, ymin = .data[[ci_lo]], ymax = .data[[ci_hi]]),
                alpha = alpha_ci, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
    geom_ribbon(data = dat_ci1, mapping = aes(x = horizon, ymin = .data[[ci_lo]], ymax = .data[[ci_hi]]),
                alpha = alpha_ci1, fill = color_ci, linetype = linetype_ci, linewidth = linewidth_ci) +
    geom_line(mapping = aes(x = .data[["horizon"]], y = .data[[response_shock_var]]), color = color_irf) +
    labs(title = my_title, y = my_ytitle, x = my_xtitle)   
  
  if (!is.null(my_ylim)) {
    plt <- plt + scale_y_continuous(limits = my_ylim)
  }
  if (!is.null(my_ybreaks)) {
    plt <- plt + scale_y_continuous(breaks = my_ybreaks)
  }
  
  return(plt)
}

plot_irf <- function(county_name, dat_var, nrep, block_length) {
  dat_ret <- dat_var |> 
    filter(area_title == county_name) |>
    # filter(area_title == "Kern County, California") |>
    # Choose how to encode percent change in employment
    mutate(dempl = change_log_empl) |> 
    select(date, oilsupply, aggdemand, rpoil, dempl, empl, change_log_empl, change_log_empl0)
  
  dat <- dat_ret |> 
    select(date, oilsupply, aggdemand, rpoil, dempl) |> 
    drop_na()
  
  y <- dat |> 
    select(-date) |> 
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
  
  nrep <- nrep
  block_length <- block_length
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
  
  my_ytitle_oilsupply <- "Oil production"
  my_ytitle_aggdemand <- "Real economic activity"
  my_ytitle_rpoil <- "Real price of oil"
  # my_ytitle_dempl <- "Employment in oil-dependent counties"
  my_ytitle_dempl <- "Employment"
  my_xtitle <- "Months"
  
  theme_irf <- function() {
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(size = 8),
      axis.title.x = element_text(size = 8), 
      axis.title.y = element_text(size = 8), 
      axis.text.y = element_text(size = 6), 
      axis.text.x = element_text(size = 6)      
    )    
  }
  
  # Negative oil-supply shock
  my_shock_title_oilsupply <- "Oil-supply shock"
  plt_oilsupply_oilsupply <- plot_irf_response_shock(response = "oilsupply", shock = "oilsupply", 
                                                     my_title = my_shock_title_oilsupply, my_ytitle = my_ytitle_oilsupply, 
                                                     dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                     yintercept_value = 0, 
                                                     alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                     color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci, 
                                                     color_irf = color_irf,
                                                     my_ybreaks = seq(-2.0, 0.5, 0.5), my_ylim = c(-2.0, 0.5)) +
    theme_irf()
  
  plt_aggdemand_oilsupply <- plot_irf_response_shock(response = "aggdemand", shock = "oilsupply", 
                                                     my_title = my_shock_title_oilsupply, my_ytitle = my_ytitle_aggdemand, 
                                                     dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                     yintercept_value = 0,
                                                     alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                     color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                                     color_irf = color_irf) +
    theme_irf()
  
  plt_rpoil_oilsupply <- plot_irf_response_shock(response = "rpoil", shock = "oilsupply", 
                                                 my_title = my_shock_title_oilsupply, my_ytitle = my_ytitle_rpoil, 
                                                 dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                 yintercept_value = 0,
                                                 alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                 color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                                 color_irf = color_irf) +
    theme_irf()
  
  plt_dempl_oilsupply <- plot_irf_response_shock(response = "dempl", shock = "oilsupply", 
                                                 my_title = my_shock_title_oilsupply, my_ytitle = my_ytitle_dempl, 
                                                 dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                 yintercept_value = 0,
                                                 alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                 color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                                 color_irf = color_irf) +
    theme_irf()
  
  # Aggregate-demand shock
  my_title_aggdemand <- "Aggregate-demand shock"
  plt_oilsupply_aggdemand <- plot_irf_response_shock(response = "oilsupply", shock = "aggdemand", 
                                                     my_title = my_title_aggdemand, my_ytitle = my_ytitle_oilsupply, 
                                                     dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                     yintercept_value = 0,
                                                     alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                     color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                                     color_irf = color_irf) + theme_irf()
  
  plt_aggdemand_aggdemand <- plot_irf_response_shock(response = "aggdemand", shock = "aggdemand", 
                                                     my_title = my_title_aggdemand, my_ytitle = my_ytitle_aggdemand, 
                                                     dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                     yintercept_value = 0,
                                                     alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                     color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                                     color_irf = color_irf) + theme_irf()
  
  plt_rpoil_aggdemand <- plot_irf_response_shock(response = "rpoil", shock = "aggdemand", 
                                                 my_title = my_title_aggdemand, my_ytitle = my_ytitle_rpoil, 
                                                 dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                 yintercept_value = 0,
                                                 alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                 color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                                 color_irf = color_irf) + theme_irf()
  
  plt_dempl_aggdemand <- plot_irf_response_shock("dempl", "aggdemand", 
                                                 my_title = my_title_aggdemand, my_ytitle = my_ytitle_dempl, 
                                                 dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                 yintercept_value = 0,
                                                 alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                 color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                                 color_irf = color_irf) + theme_irf()
  
  # Real-price-of-oil shock
  my_title_rpoil <- "Oil-specific-demand shock"
  plt_oilsupply_rpoil <- plot_irf_response_shock(response = "oilsupply", shock = "rpoil", 
                                                 my_title = my_title_rpoil, my_ytitle = my_ytitle_oilsupply, 
                                                 dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                 yintercept_value = 0,
                                                 alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                 color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                                 color_irf = color_irf) + theme_irf()
  
  plt_aggdemand_rpoil <- plot_irf_response_shock(response = "aggdemand", shock = "rpoil", 
                                                 my_title = my_title_rpoil, my_ytitle = my_ytitle_aggdemand, 
                                                 dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                 yintercept_value = 0,
                                                 alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                 color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                                 color_irf = color_irf) + theme_irf()
  
  plt_rpoil_rpoil <- plot_irf_response_shock(response = "rpoil", shock = "rpoil", 
                                             my_title = my_title_rpoil, my_ytitle = my_ytitle_rpoil, 
                                             dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                             yintercept_value = 0,
                                             alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                             color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                             color_irf = color_irf) + theme_irf()
  
  plt_dempl_rpoil <- plot_irf_response_shock("dempl", "rpoil", 
                                             my_title = my_title_rpoil, my_ytitle = my_ytitle_dempl, 
                                             dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                             yintercept_value = 0,
                                             alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                             color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci = linewidth_ci,
                                             color_irf = color_irf) + theme_irf()
  
  
  # Employment-demand shock
  my_title_dempl <- "Regional-demand shock"
  plt_oilsupply_dempl <- plot_irf_response_shock(response = "oilsupply", shock = "dempl", 
                                                 my_title = my_title_dempl, my_ytitle = my_ytitle_oilsupply, 
                                                 dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                 yintercept_value = 0,
                                                 alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                 color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci,
                                                 color_irf = color_irf,
                                                 my_xtitle = my_xtitle) + theme_irf()
  
  
  plt_aggdemand_dempl <- plot_irf_response_shock(response = "aggdemand", shock = "dempl", 
                                                 my_title = my_title_dempl, my_ytitle = my_ytitle_aggdemand, 
                                                 dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                                 yintercept_value = 0,
                                                 alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                                 color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci,
                                                 color_irf = color_irf,
                                                 my_xtitle = my_xtitle) + theme_irf()
  
  plt_rpoil_dempl <- plot_irf_response_shock(response = "rpoil", shock = "dempl", 
                                             my_title = my_title_dempl, my_ytitle = my_ytitle_rpoil, 
                                             dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                             yintercept_value = 0,
                                             alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                             color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci,
                                             color_irf = color_irf,
                                             my_xtitle = my_xtitle) + theme_irf()
  
  plt_dempl_dempl <- plot_irf_response_shock("dempl", "dempl", 
                                             my_title = my_title_dempl, my_ytitle = my_ytitle_dempl, 
                                             dat_irf = irf$irf_tidy, dat_ci = dat_irf_ci, dat_ci1 = dat_irf_ci1, 
                                             yintercept_value = 0,
                                             alpha_ci = alpha_ci, alpha_ci1 = alpha_ci1,
                                             color_ci = color_ci, linetype_ci = linetype_ci, linewidth_ci,
                                             color_irf = color_irf,
                                             my_xtitle = my_xtitle) + theme_irf()
  
  plt_all <- (plt_oilsupply_oilsupply | plt_aggdemand_oilsupply | plt_rpoil_oilsupply | plt_dempl_oilsupply) /
    (plt_oilsupply_aggdemand | plt_aggdemand_aggdemand | plt_rpoil_aggdemand | plt_dempl_aggdemand) /
    (plt_oilsupply_rpoil | plt_aggdemand_rpoil | plt_rpoil_rpoil | plt_dempl_rpoil) /
    (plt_oilsupply_dempl | plt_aggdemand_dempl | plt_rpoil_dempl | plt_dempl_dempl) &  
    scale_x_continuous(expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15)) 
    # The following is now erroring out through what looks like a clash between ggplot2 and patchwork
    # theme_minimal() &
    # theme(
    #   panel.grid.minor.x = element_blank(),
    #   plot.title = element_text(size = 8),
    #   axis.title.x = element_text(size = 8), 
    #   axis.title.y = element_text(size = 8), 
    #   axis.text.y = element_text(size = 6), 
    #   axis.text.x = element_text(size = 6)      
    # )
  
  # In plt_response_shock form 
  plt_oil_block <- (plt_oilsupply_oilsupply | plt_aggdemand_oilsupply | plt_rpoil_oilsupply) /
    (plt_oilsupply_aggdemand | plt_aggdemand_aggdemand | plt_rpoil_aggdemand) /
    (plt_oilsupply_rpoil | plt_aggdemand_rpoil | plt_rpoil_rpoil) &  
    scale_x_continuous(expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15)) 
  # &
  #   theme_minimal() &
  #   theme(
  #     panel.grid.minor.x = element_blank(),
  #     plot.title = element_text(size = 8),
  #     axis.title.x = element_text(size = 8), 
  #     axis.title.y = element_text(size = 8), 
  #     axis.text.y = element_text(size = 6), 
  #     axis.text.x = element_text(size = 6)      
  #   )
  
  # In plt_response_shock form
  plt_empl_block <- (plt_dempl_oilsupply) /
    (plt_dempl_aggdemand) /
    (plt_dempl_rpoil) /
    (plt_dempl_dempl) &  
    scale_x_continuous(expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15)) 
  # &
  #   theme_minimal() &
  #   theme(
  #     panel.grid.minor.x = element_blank(),
  #     plot.title = element_text(size = 8),
  #     axis.title.x = element_text(size = 8), 
  #     axis.title.y = element_text(size = 8), 
  #     axis.text.y = element_text(size = 6), 
  #     axis.text.x = element_text(size = 6)      
  #   )
  
  ret <- list(plt_all = plt_all, 
              plt_oil_block = plt_oil_block,
              plt_empl_block = plt_empl_block,
              plt_dempl_oilsupply = plt_dempl_oilsupply,
              plt_dempl_aggdemand = plt_dempl_aggdemand,
              plt_dempl_rpoil = plt_dempl_rpoil,
              plt_dempl_dempl = plt_dempl_dempl,
              var_sol = sol,
              var_dat = dat_ret,
              var_order = var_order,
              county_name = county_name)
  return(ret)
}

get_my_county_name <- function(county_name) {
  my_county_name1 <- str_split_i(county_name, " ", 1)
  my_county_name2 <- str_split_i(county_name, " ", 2)
  my_county_name3 <- str_split_i(county_name, " ", 3)
  my_county_name4 <- str_split_i(county_name, " ", 4)
  
  if (is.na(my_county_name4)) {
    my_county_name4 <- ""
  }
  
  my_county_name <- paste0(my_county_name1, " ", my_county_name2, "\n", my_county_name3, " ", my_county_name4)
}

combine_irf <- function(plt_kern_analysis, plt_weld_analysis, plt_eddy_analysis, plt_mckenzie_analysis, plt_karnes_analysis) {
  plt_karnes <- (plt_karnes_analysis$plt_dempl_oilsupply + labs(title = get_my_county_name(plt_karnes_analysis$county_name), 
                                                                subtitle = "Oil supply shock")) / 
                (plt_karnes_analysis$plt_dempl_aggdemand + labs(title = "", subtitle = "Aggregate demand shock")) /
                (plt_karnes_analysis$plt_dempl_rpoil + labs(title = "", subtitle = "Oil-specific demand shock")) / 
                (plt_karnes_analysis$plt_dempl_dempl + labs(title = "", subtitle = "Regional demand shock"))
  # plt_karnes <- plt_karnes + plot_layout(axis_titles = "collect")
  
  plt_mckenzie <- (plt_mckenzie_analysis$plt_dempl_oilsupply + labs(title = get_my_county_name(plt_mckenzie_analysis$county_name), 
                                                                subtitle = "Oil supply shock")) / 
                  (plt_mckenzie_analysis$plt_dempl_aggdemand + labs(title = "", subtitle = "Aggregate demand shock")) /
                  (plt_mckenzie_analysis$plt_dempl_rpoil + labs(title = "", subtitle = "Oil-specific demand shock")) / 
                  (plt_mckenzie_analysis$plt_dempl_dempl + labs(title = "", subtitle = "Regional demand shock")) 
  # plt_mckenzie <- plt_mckenzie + plot_layout(axis_titles = "collect")
  
  plt_kern <- (plt_kern_analysis$plt_dempl_oilsupply + labs(title = get_my_county_name(plt_kern_analysis$county_name), 
                                                                subtitle = "Oil supply shock")) / 
              (plt_kern_analysis$plt_dempl_aggdemand + labs(title = "", subtitle = "Aggregate demand shock")) /
              (plt_kern_analysis$plt_dempl_rpoil     + labs(title = "", subtitle = "Oil-specific demand shock"))     /
              (plt_kern_analysis$plt_dempl_dempl     + labs(title = "", subtitle = "Regional demand shock"))     
  # plt_kern <- plt_kern + plot_annotation(title = plt_kern_analysis$county_name) # plot_layout(axis_titles = "collect") 
  
  plt_eddy <- (plt_eddy_analysis$plt_dempl_oilsupply + labs(title = get_my_county_name(plt_eddy_analysis$county_name), 
                                                                subtitle = "Oil supply shock")) / 
              (plt_eddy_analysis$plt_dempl_aggdemand + labs(title = "", subtitle = "Aggregate demand shock")) /
              (plt_eddy_analysis$plt_dempl_rpoil + labs(title = "", subtitle = "Oil-specific demand shock"))     /
              (plt_eddy_analysis$plt_dempl_dempl + labs(title = "", subtitle = "Regional demand shock"))   
  # plt_eddy <- plt_eddy + plot_annotation(title = plt_eddy_analysis$county_name) # + plot_layout(axis_titles = "collect") 
  
  plt_weld <- (plt_weld_analysis$plt_dempl_oilsupply + labs(title = get_my_county_name(plt_weld_analysis$county_name), 
                                                                subtitle = "Oil supply shock")) / 
              (plt_weld_analysis$plt_dempl_aggdemand + labs(title = "", subtitle = "Aggregate demand shock")) /
              (plt_weld_analysis$plt_dempl_rpoil + labs(title = "", subtitle = "Oil-specific demand shock"))   /
              (plt_weld_analysis$plt_dempl_dempl + labs(title = "", subtitle = "Regional demand shock")) 
  # plt_weld <- plt_weld + plot_annotation(title = plt_weld_analysis$county_name) # + plot_layout(axis_titles = "collect") 
  
  # plt <- (plt_kern | plt_eddy | plt_weld | plt_karnes | plt_mckenzie) + plot_layout(axis_titles = "collect") & theme_minimal() & 
  plt <- (plt_kern | plt_eddy | plt_weld | plt_karnes | plt_mckenzie) + plot_layout(axis_titles = "collect_x") & theme_minimal() & 
    theme(
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 8),
      axis.title.x = element_text(size = 8), 
      axis.title.y = element_text(size = 8), 
      axis.text.y = element_text(size = 8), 
      axis.text.x = element_text(size = 8)
  )
}

# COMPUTE HD --------------------------------------------------------------

compute_hd <- function(plt_analysis) {
  sol <- plt_analysis$var_sol
  var_order <- plt_analysis$var_order
  var_dat_drop_na <- drop_na(plt_analysis$var_dat, dempl)

  hd <- compute_hist_decomp(sol, date = var_dat_drop_na$date, var_order = var_order) 
  return(hd)
}

plot_hd_cumulative_change_empl <- function(plt_analysis, hd, date_start) {
  # The SVAR for the vector y(t) enters the change in employment as 
  #   log(empl(t)) - log(empl(t-1)). 
  # The historical decomposition approximates y(t). The approximation to the
  # change in employment is summed to approximate the cumulative change.
  # 
  # Inputs:
  #    plt_analysis: used to 
  #    hd: historical-decomposition object
  #    date_start: date to start the analysis

  # The actual data joined with output from the historical decomposition 
  dat_hd <- plt_analysis$var_dat |> 
    # Drop the first row of 
    drop_na(dempl) |> 
    left_join(hd, by = join_by(date))  
  
  my_series_levels <- c("Employment growth",  
                        "Less oil-supply shocks",
                        "Less aggregate demand shocks",
                        "Less oil-specific demand shocks",
                        "Less region-specific demand shocks")
  
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
}

plot_hd <- function(hd, dat_recess, date_start, plt_hd_linewidth, plt_hd_linecolor, ylim_lo, ylim_hi) {
  
  mylinewidth <- plt_hd_linewidth
  mylinecolor <- plt_hd_linecolor
  myhlinecolor <- "grey12"
  
  myylabel <- ""
  
  # Data
  df <- hd |> 
    mutate(year = year(date),
         quarter = quarter(date)) |> 
    group_by(year, quarter) |> 
    summarise(date = first(date),
              across(c(hd_series_shock_dempl_oilsupply, hd_series_shock_dempl_aggdemand, hd_series_shock_dempl_rpoil, hd_series_shock_dempl_dempl), mean))
  
  plt_dat_recess <- filter(dat_recess,
                           begin >= min(df$date),
                           begin <= max(df$date))
  
  plt_oilsupply <- ggplot(data = df) +
    geom_rect(
      data = plt_dat_recess,
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_hline(yintercept = 0.0, color = myhlinecolor) +
    geom_line(
      mapping = aes(x = date, y = 4 * hd_series_shock_dempl_oilsupply),
      linewidth = mylinewidth,
      color = mylinecolor
    ) +
    coord_cartesian(ylim = c(ylim_lo, ylim_hi)) +
    labs(title = "Cumulative effect of oil-supply shock", x = "", y = myylabel) +
    scale_x_date(expand = c(0, 0))
  # ylim(ylim_lo, ylim_hi)


  plt_aggdemand <- ggplot(data = df) +
    geom_rect(
      data = plt_dat_recess,
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +    
    geom_hline(yintercept = 0.0, color = myhlinecolor) +
    geom_line(
      mapping = aes(x = date, y = 4 * hd_series_shock_dempl_aggdemand),
      linewidth = mylinewidth,
      color = mylinecolor
    ) +
    coord_cartesian(ylim = c(ylim_lo, ylim_hi)) +
    labs(title = "Cumulative effect of aggregate-demand shock", x = "", y = myylabel) +
    scale_x_date(expand = c(0, 0))
  # ylim(ylim_lo, ylim_hi)

  plt_rpoil <- ggplot(data = df) +
    geom_rect(
      data = plt_dat_recess,
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +    
    geom_hline(yintercept = 0.0, color = myhlinecolor) +
    geom_line(
      mapping = aes(x = date, y = 4 * hd_series_shock_dempl_rpoil),
      linewidth = mylinewidth,
      color = mylinecolor
    ) +
    coord_cartesian(ylim = c(ylim_lo, ylim_hi)) +
    labs(title = "Cumulative effect of oil-specific-demand shock", x = "", y = myylabel) +
    scale_x_date(expand = c(0, 0))
  # ylim(ylim_lo, ylim_hi)

  plt_dempl <- ggplot(data = df) +
    geom_rect(
      data = plt_dat_recess,
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +    
    geom_hline(yintercept = 0.0, color = myhlinecolor) +
    geom_line(
      mapping = aes(x = date, y = 4 * hd_series_shock_dempl_dempl),
      linewidth = mylinewidth,
      color = mylinecolor
    ) +
    coord_cartesian(ylim = c(ylim_lo, ylim_hi)) +
    labs(title = "Cumulative effect of regional-demand shock", x = "", y = myylabel) +
    scale_x_date(expand = c(0, 0))
  # ylim(ylim_lo, ylim_hi)

  plt_oilsupply / plt_aggdemand / plt_rpoil / plt_dempl + plot_layout(axes = "collect_x") & 
    theme_minimal() & 
    theme(plot.title = element_text(size = 10))
}

make_hd_level <- function(plt_analysis, historical_decomp, date_start, date_end) {
  # This function approximates the level of employment based on the historical
  # decomposition. To keep in mind:
  # - Because the estimates is based on the change in employment, the
  #   approximation starts one month after the start date passed to the function.
  # - The function produces a check of the level in addition to producing a
  #   dataset that can be used in subsequent analysis.
  # - To match the level, the approximation exactly matches the mean growth rate
  #   over the relevant period. This is accomplished by subtracting off the
  #   growth-rate approximation and adding in the average growth rate computed in
  #   the data.
  
  df <- drop_na(plt_analysis$var_dat)
  
  dat <- df |>
    left_join(historical_decomp, by = "date") |>
    filter(date >= date_start & date <= date_end) |>
    arrange(date) |>
    mutate(multiplicative_factor = case_when(row_number() == 2 ~ lag(empl), .default = 1)) |>
    tail(-1) |>
    mutate(
      gapprox_empl_temp = hd_series_shock_dempl_oilsupply +
        hd_series_shock_dempl_aggdemand +
        hd_series_shock_dempl_rpoil +
        hd_series_shock_dempl_dempl,
      gapprox_empl = gapprox_empl_temp - mean(gapprox_empl_temp) + mean(change_log_empl),
      gapprox_oilsupply = gapprox_empl - hd_series_shock_dempl_oilsupply,
      gapprox_aggdemand = gapprox_empl - hd_series_shock_dempl_aggdemand,
      gapprox_rpoil = gapprox_empl - hd_series_shock_dempl_rpoil,
      gapprox_dempl = gapprox_empl - hd_series_shock_dempl_dempl,
      empl_approx = cumprod(multiplicative_factor * (1 + gapprox_empl / 100)),
      empl_approx_oilsupply = cumprod(multiplicative_factor * (1 + gapprox_oilsupply / 100)),
      empl_approx_aggdemand = cumprod(multiplicative_factor * (1 + gapprox_aggdemand / 100)),
      empl_approx_rpoil = cumprod(multiplicative_factor * (1 + gapprox_rpoil / 100)),
      empl_approx_dempl = cumprod(multiplicative_factor * (1 + gapprox_dempl / 100)),
      diff_rpoil = near(empl - empl_approx_rpoil, max(empl - empl_approx_rpoil)),
      diff_rpoil_label = empl - empl_approx_rpoil,
      diff_dempl = near(empl - empl_approx_dempl, max(empl - empl_approx_dempl)),
      diff_dempl_label = empl - empl_approx_dempl
    )

    # Plot that checks the level approximation
    dat_lvl <- dat |>
      select(date, empl, empl_approx) |>
      mutate(abs_diff = abs(empl - empl_approx)) |>
      pivot_longer(cols = c(starts_with("empl"), empl_approx), names_to = "series", values_to = "empl") |>
      mutate(my_label = as_factor(case_when(
        series == "empl" ~ "Data",
        series == "empl_approx" ~ "Approximation"
      )))

    dat_lvl_diff <- dat_lvl |>
      summarise(abs_diff = round(max(abs_diff, 0))) |> pull(abs_diff)

    plt_lvl_check <- ggplot(data = dat_lvl) +
      geom_line(mapping = aes(x = date, y = empl, color = my_label, linetype = my_label), linewidth = 1.0) +
      scale_color_manual(values = c(Data = "black", Approximation = "red")) +
      labs(x = "", y = "Employed persons", caption = paste0("Maximum absolute difference between series: ", format(dat_lvl_diff, big.mark = ","), " persons")) +
      scale_y_continuous(label = scales::label_comma()) +
      theme_minimal() +
      guides(color = guide_legend(title = NULL, position = "inside"),
             linetype = guide_legend(title = NULL, position = "inside")) +
      theme(legend.position.inside = c(.2, 0.8)) 

    # Data for the main plot
    dat_plt <- select(dat, -empl) |>
      select(date, starts_with("empl_approx")) |>
      pivot_longer(cols = starts_with("empl_approx")) |>
      mutate(
        my_series_label = case_when(
          name == "empl_approx" ~ "Employment data",
          name == "empl_approx_oilsupply" ~ "Less oil-supply disruption",
          name == "empl_approx_aggdemand" ~ "Less agg demand",
          name == "empl_approx_rpoil" ~ "Less oil-specific demand",
          name == "empl_approx_dempl" ~ "Less local shocks"
        )
      )

    ret <- list(plt_lvl_check = plt_lvl_check,
                dat_plt = dat_plt)
    return(ret)
}

rename_cols_counterfactual_empl <- function(vec) {
  new_names_1 <- vec[1]
  new_names_2 <- vec[2:length(vec)]
  
  new_names_2 <- str_sub(new_names_2, start = 1, end = -12)
  new_names_2 <- str_replace(new_names_2, "empl_percent_away", "Percent")
  new_names_2 <- str_replace(new_names_2, "empl", "Employment")
  
  c("Counterfactual", new_names_2)
}

make_col_pretty <- function(x, highlight_color = "blue") {
  # Find furthest away from the actual data
  # located in the first position 
  to_replace <- which.max(abs(x - x[[1]]))
  
  x_pretty <- ifelse(x > 1000,
                     formatC(x, format = "f", digits = 0, big.mark = ","),
                     formatC(x, format = "f", digits = 1, big.mark = ",", drop0trailing = FALSE)
                     ) # prettyNum(x, big.mark = ",", drop0trailing = FALSE))
  x_pretty <- str_replace(x_pretty, "-", "$-$")
  x_pretty[[to_replace]] <- paste0("\\textcolor{", highlight_color, "}{", x_pretty[[to_replace]], "}")
  return(x_pretty)
}

make_table_counterfactual_empl <- function(df, table_dates) {
  dat_tbl <- df |>
    filter(date %in% table_dates) |>
    rename(empl = value) |>
    group_by(date) |>
    mutate(
      empl_rel_index = max(empl * (my_series_label == "Employment data")),
      empl_percent_away = round(((empl - empl_rel_index) / empl_rel_index) * 100, digits = 1),
      empl = round(empl, digits = 0)
    )
  
  dat_tbl_wide <- dat_tbl |>
    pivot_wider(
      id_cols = my_series_label,
      names_from = date,
      values_from = c(empl, empl_percent_away)
    ) |>
    # Order the columns as needed
    select(
      my_series_label,
      ends_with(as.character(table_dates[1])),
      ends_with(as.character(table_dates[2])),
      ends_with(as.character(table_dates[3]))
    ) |>
    mutate(across(-my_series_label, ~make_col_pretty(.x))) |> 
    mutate(my_series_label = case_when(
      my_series_label == "Employment data" ~ "\\textbf{Employment data}",
      .default = my_series_label
    ))
  
  tbl_col_names <- rename_cols_counterfactual_empl(names(dat_tbl_wide))
  
  my_header_names_dates <- c(" ",
                             paste0(month(table_dates[[1]], label = TRUE), " ", year(table_dates[[1]])),
                             paste0(month(table_dates[[2]], label = TRUE), " ", year(table_dates[[2]])),
                             paste0(month(table_dates[[3]], label = TRUE), " ", year(table_dates[[3]])))
  my_header_names_cols <- c(1, 2, 2, 2)
  my_header_names <- data.frame(my_header_names_dates, my_header_names_cols)
  kable(dat_tbl_wide,
    col.names = tbl_col_names,
    # Formatting done above
    # digits = c(0, 0, 2, 0, 2, 0, 2),
    # format.args = list(big.mark = ","),
    align = "lcrcrcr",
    booktabs = TRUE,
    format = "latex",
    table.envir = NULL,
    escape = FALSE
  ) |>
    add_header_above(header = my_header_names) 
}

plot_counterfactual_empl_lvl <- function(df, dat_recess, my_ylab, my_legend_title, mycolors, mycolor_highlight, plt_empl_lo, plt_empl_hi, plt_y_step) {
  plt_dat_recess <- filter(dat_recess,
                           begin >= min(df$date),
                           begin <= max(df$date))
  
  my_label_func <- scales::label_comma()
  df_end <- df |> filter(date == max(date)) |> 
    mutate(my_label = paste0(my_label_func(round(value, 0))))
  
  df_middle <- df |> 
    filter(date == ymd("2019-01-01")) |> 
    mutate(my_label = paste0(my_label_func(round(value, 0))))
  
  df_begin <- df |> filter(date == min(date))
  
  plt <- ggplot(data = df) +
    geom_rect(
      data = plt_dat_recess,
      mapping = aes(
        xmin = begin,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(mapping = aes(x =  date, y = value, color = my_series_label, linetype = my_series_label), linewidth = 0.9) +
    geom_label_repel(data = df_end, mapping = aes(x = date, y = value, label = my_label, color = my_series_label), size = 2.25, show.legend = FALSE, nudge_x = 365 * 2.0, hjust = "right", box.padding = 0.5, max.overlaps = Inf) + # default font size = 3.88, nudge_y = c(50000 / 2, 25000 / 2, -5000 / 2, -25000 / 2, -50000 / 2), 
    # geom_label_repel(data = df_middle, mapping = aes(x = date, y = value, label = my_label, color = my_series_label), size = 3.0, show.legend = FALSE, hjust = "left", box.padding = 0.5, max.overlaps = Inf) +    
    labs(x = "", y = my_ylab,
         color = my_legend_title,
         linetype = my_legend_title,
         title = paste(
           month(min(df$date), label = TRUE), year(min(df$date)), "through",
           month(max(df$date), label = TRUE), year(max(df$date))
         )) +
    scale_color_manual(values = c("Employment data" = mycolor_highlight,
                                  "Less oil-supply disruption" = mycolors[["less_oilsupply"]],
                                  "Less agg demand" = mycolors[["less_aggdemand"]],
                                  "Less oil-specific demand" = mycolors[["less_rpoil"]],
                                  "Less local shocks" = mycolors[["less_local"]])) +
    scale_y_continuous(breaks = seq(plt_empl_lo, plt_empl_hi, by = plt_y_step), labels = scales::label_comma(), limits = c(plt_empl_lo, plt_empl_hi)) +
    theme_minimal(base_size = 10) + scale_x_date(expand = expansion(mult = c(0.05, 0.12))) + # default: expansion(0.05, 0.05)
    theme(legend.position = "inside", legend.position.inside = c(0.2, 0.8), legend.text = element_text(size = 10),
          legend.key.width = unit(3, "line"), axis.title.y.left = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10, unit = "pt")))
  
  ret <- list(plt = plt, 
              df_begin = df_begin,
              df_end = df_end)
  return(ret)
}

# Forecast-error variance decomposition ----------------------------------

make_fevd <- function(dat, var_name) {
  dat_fevd <- kilianr::compute_fcast_error_var_decomp(dat$var_sol, h = Inf, var_name = var_name, eps = 1e-4)
  dat_fevd_keep <- dat_fevd |> 
    slice(1, 2, 3, 12, n())
    # slice(1, 2, 3, 6, 9, 12, 15, n())
}

compute_fevd <- function(plt_kern_analysis, plt_weld_analysis, plt_eddy_analysis, plt_mckenzie_analysis, plt_karnes_analysis) {
  
  my_economies <- tribble(
    ~dat,
    plt_eddy_analysis,     
    plt_karnes_analysis,    
    plt_kern_analysis, 
    plt_mckenzie_analysis,     
    plt_weld_analysis
    ) |> 
    mutate(county_name = map(dat, \(x) x$county_name)) |> 
    unnest(county_name) |> 
    mutate(fevd = map(dat, .f = make_fevd))
  
  my_economies_table_groups <- my_economies |> 
    unnest(fevd) |> 
    mutate(rnum = row_number()) |> 
    group_by(county_name) |> 
    summarise(rnum1 = first(rnum),
              rnumn = last(rnum)) 

  my_economies_table <- my_economies |> 
    unnest(fevd) |> 
    select(horizon, oilsupply, aggdemand, rpoil, dempl) |> 
    mutate(across(oilsupply:dempl, \(x) round(x, digits = 1)),
           horizon = make_infty(horizon)) |> 
    kableExtra::kable("latex", booktabs = TRUE, escape = FALSE,
                      caption = "\\label{tab:fevd} Forecast error variance decomposition for employment growth",
                      align = rep("l", 5),
                      col.names = linebreak(c("\nHorizon", "Oil-supply\nshock", "Aggregate-\ndemand shock", "Oil-specific-\ndemand shock", "Residual\nshock"),
                                            align = "l")) |> 
    pack_rows(group_label = my_economies_table_groups$county_name[[1]], 
              start_row = my_economies_table_groups$rnum1[[1]], 
              end_row = my_economies_table_groups$rnumn[[1]]) |>
    pack_rows(group_label = my_economies_table_groups$county_name[[2]], 
              start_row = my_economies_table_groups$rnum1[[2]], 
              end_row = my_economies_table_groups$rnumn[[2]]) |>  
    pack_rows(group_label = my_economies_table_groups$county_name[[3]], 
              start_row = my_economies_table_groups$rnum1[[3]], 
              end_row = my_economies_table_groups$rnumn[[3]]) |>      
    pack_rows(group_label = my_economies_table_groups$county_name[[4]], 
              start_row = my_economies_table_groups$rnum1[[4]], 
              end_row = my_economies_table_groups$rnumn[[4]]) |>          
    pack_rows(group_label = my_economies_table_groups$county_name[[5]], 
              start_row = my_economies_table_groups$rnum1[[5]], 
              end_row = my_economies_table_groups$rnumn[[5]]) |>          
    add_header_above(c(" ", "Percent of $h$-step ahead forecast error variance explained by" = 4), escape = FALSE)
  
  return(my_economies_table)
}

compute_fevd_kern <- function(plt_kern_analysis, var_name, caption_name, latex_suffix) {
  
  my_economies <- tribble(
    ~dat,
    plt_kern_analysis
  ) |> 
    mutate(county_name = map(dat, \(x) x$county_name)) |> 
    unnest(county_name) |> 
    mutate(fevd = map2(dat, var_name, .f = make_fevd))
  
  my_economies_table_groups <- my_economies |> 
    unnest(fevd) |> 
    mutate(rnum = row_number()) |> 
    group_by(county_name) |> 
    summarise(rnum1 = first(rnum),
              rnumn = last(rnum)) 
  
  my_economies_table <- my_economies |> 
    unnest(fevd) |> 
    select(horizon, oilsupply, aggdemand, rpoil, dempl) |> 
    mutate(across(oilsupply:dempl, \(x) round(x, digits = 1)),
           horizon = make_infty(horizon)) |> 
    kableExtra::kable("latex", booktabs = TRUE, escape = FALSE,
                      caption = paste0("\\label{tab:fevd-",  latex_suffix, "} Forecast error variance decomposition for ", caption_name),
                      align = rep("l", 5),
                      col.names = linebreak(c("\nHorizon", "Oil-supply\nshock", "Aggregate-\ndemand shock", "Oil-specific-\ndemand shock", "Residual\nshock"),
                                            align = "l")) |> 
    # pack_rows(group_label = my_economies_table_groups$county_name[[1]], 
    #           start_row = my_economies_table_groups$rnum1[[1]], 
    #           end_row = my_economies_table_groups$rnumn[[1]]) |>
    # pack_rows(group_label = my_economies_table_groups$county_name[[2]], 
    #           start_row = my_economies_table_groups$rnum1[[2]], 
    #           end_row = my_economies_table_groups$rnumn[[2]]) |>  
    # pack_rows(group_label = my_economies_table_groups$county_name[[3]], 
    #           start_row = my_economies_table_groups$rnum1[[3]], 
    #           end_row = my_economies_table_groups$rnumn[[3]]) |>      
    # pack_rows(group_label = my_economies_table_groups$county_name[[4]], 
    #           start_row = my_economies_table_groups$rnum1[[4]], 
    #           end_row = my_economies_table_groups$rnumn[[4]]) |>          
    # pack_rows(group_label = my_economies_table_groups$county_name[[5]], 
    #           start_row = my_economies_table_groups$rnum1[[5]], 
    #           end_row = my_economies_table_groups$rnumn[[5]]) |>          
    add_header_above(c(" ", "Percent of $h$-step ahead forecast error variance explained by" = 4), escape = FALSE)
  
  return(my_economies_table)
}

# Second-stage analysis ---------------------------------------------------

plot_irf2_wage_kern <- function(dat_wage2_kern, plt_kern_analysis, irf2_color = csub_blue, horizon = 8, my_block_length = 4) {
  dat <- dat_wage2_kern$dat
  
  # Plotting parameters
  color_irf <- irf2_color
  color_ci <- irf2_color
  
  linewidth_irf <- 1.0
  linewidth_ci <- 0.5
  
  linetype_irf <- "solid"
  linetype_ci <- "solid"
  
  alpha_ci <- 0.3
  alpha_ci1 <- 0.6
  
  my_title_oilsupply <- "Oil-supply shock"
  my_title_rpoil <- "Oil-specific-demand shock"

  my_ytitle <- "Cumulative percent change in average weekly wage"
  my_xtitle <- "Quarters"
  
  # Construct structural shocks
  sol <- plt_kern_analysis$var_sol
  dat_var <- plt_kern_analysis$var_dat
  
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
           month = month(date),
           qtr = quarter(date)) |> 
    group_by(year, qtr) |> 
    summarise(date = first(date),
              shock_oilsupply = mean(shock_oilsupply),
              shock_aggdemand = mean(shock_aggdemand),
              shock_rpoil = mean(shock_rpoil),
              cnt = n()) |> 
    # Make sure there are 3 months within each quarter
    filter(cnt == 3) |> 
    ungroup() |> 
    left_join(dat_wage2_kern$dat, by = join_by(year == year, qtr == qtr, date == date)) |> 
    mutate(log_avg_wkly_wage = log(avg_wkly_wage),
           d_log_avg_wkly_wage = log_avg_wkly_wage - lag(log_avg_wkly_wage, order_by = date, n = 1))
  
  # Construct datasets from second-stage impulse responses
  irf2_oilsupply <- stage2irf(
    y = 100 * dat2$d_log_avg_wkly_wage,
    x = -dat2$shock_oilsupply,
    p = horizon,
    block_length = my_block_length,
    nrep = 1000,
    standard_deviation_factor = 2,
    boot_seed = 676,
    cumeffect = TRUE
  )
  
  irf2_oilsupply <- irf2_oilsupply |>
    mutate(
      irfstd = irf2 - irf2_lo,
      irf2_lolo = irf2 - 2 * irfstd,
      irf2_hihi = irf2 + 2 * irfstd
    )

  
  irf2_rpoil <- stage2irf(
    y = 100 * dat2$d_log_avg_wkly_wage,
    x = dat2$shock_rpoil,
    p = horizon,
    block_length = my_block_length,
    nrep = 1000,
    standard_deviation_factor = 1,
    boot_seed = 676,
    cumeffect = TRUE
  )
  
  
  irf2_rpoil <- irf2_rpoil |>
    mutate(
      irfstd = irf2 - irf2_lo,
      irf2_lolo = irf2 - 2 * irfstd,
      irf2_hihi = irf2 + 2 * irfstd
    )
  

  # Plots
  plt_rpoil <- ggplot(data = irf2_rpoil) +
    geom_hline(yintercept = 0.0, color = "black") +
    geom_line(mapping = aes(x = horizon, y = irf2), color = color_irf, linewidth = linewidth_irf) +
    geom_ribbon(mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
              alpha = alpha_ci1, fill = color_ci, color = color_ci, linetype = linetype_ci, linewidth = 0.0) +
    geom_ribbon(
      mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
      alpha = alpha_ci,
      fill = color_ci,
      color = color_ci,
      linetype = "solid",
      linewidth = 0.0
    ) +
    scale_x_continuous(breaks = seq(1, horizon, by = 1), expand = c(0, 0)) +
    theme_minimal() +
    labs(x = my_xtitle, y = my_ytitle, title = my_title_rpoil)
  
  plt_oilsupply <- ggplot(data = irf2_oilsupply) +
    geom_hline(yintercept = 0.0, color = "black") +
    geom_line(
      mapping = aes(x = horizon, y = irf2),
      color = color_irf,
      linewidth = linewidth_irf
    ) +
    geom_ribbon(
      mapping = aes(x = horizon, ymin = irf2_lo, ymax = irf2_hi),
      alpha = alpha_ci1,
      fill = color_ci,
      color = color_ci,
      linetype = linetype_ci,
      linewidth = 0.0
    ) +
    geom_ribbon(
      mapping = aes(x = horizon, ymin = irf2_lolo, ymax = irf2_hihi),
      alpha = alpha_ci,
      fill = color_ci,
      color = color_ci,
      linetype = "solid",
      linewidth = 0.0
    ) +
    scale_x_continuous(breaks = seq(1, horizon, by = 1), expand = c(0, 0)) +
    theme_minimal() +
    labs(x = my_xtitle, y = my_ytitle, title = my_title_oilsupply)  
  
  ret <- list(plt_rpoil = plt_rpoil, 
              plt_oilsupply = plt_oilsupply)
  return(ret)
}

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




# Helper functions --------------------------------------------------------

seasonally_adjust <- function(x, date, obs_per_yr, ...) {
  # Check for availability of suggested package seasonal
  if (!requireNamespace("seasonal", quietly = TRUE)) {
    stop(
      "Package \"seasonal\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  # Check that data are monthly or quarterly
  if ((obs_per_yr != 4) & (obs_per_yr != 12)) {
    stop(
      "The function is designed to handle monthly or quarterly seasonal adjustment."
    )
  }
  
  N <- length(x)
  
  x_indxnon <- which(!is.na(x))
  
  xx <- x[x_indxnon]
  n <- length(xx)
  
  if (n != N) {
    message("   *** Missing values detected.")
  }
  
  date_xx <- date[x_indxnon]
  
  junk <- seasonal::final(seasonal::seas(ts(
    xx,
    start = c(year(min(date_xx)), month(min(date_xx))),
    frequency = obs_per_yr
  ), ...))
  
  x[x_indxnon] <- as.numeric(junk)
  
  return(x)
}

make_infty <- function(x) {
  if_else(x <= 15, paste0(x), "$\\infty$")
}

tikzsave <- function(filename, plot, width, height) {
  tikzDevice::tikz(file = filename, width = width, heigh = height)
  
  print(plot)
  
  dev.off()
  
  filename
}

tbl_save <- function(kable_obj, fout) {
  kableExtra::save_kable(x = kable_obj, file = fout)
  fout
}

tar_fig_save <- function(fout, plot, width, height) {
  ggsave(filename = fout, plot = plot, width = width, height = height)
  fout
}
