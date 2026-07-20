library(dplyr)
library(tidyr)
library(lubridate)
library(DescTools)

library(conflicted)
conflicted::conflicts_prefer(dplyr::select)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

################################
## Local Microstructure databases builder
################################

spatial_radius <- 10
time_window <- 30
# 3.04% of tom are less than 5 minutes

spatial_microstructure_db_builder <- function (spatial_radius, time_window) {
  
  splistings <- listings |>
    left_join(
      Locations |> select(asset_id = TOKEN_ID, x=X, y=Y),
      by = c("asset_id")
    ) |>
    filter(tom > (60/86400)) # 1 minute
  spasks <- asks |>
    left_join(
      Locations |> select(asset_id = TOKEN_ID, x=X, y=Y),
      by = c("asset_id")
    ) |>
    filter(tom > 0)
  
  mslistings <- splistings |>
    left_join(
      splistings |> select(nb_list_start = list_start, nb_list_end = list_end, nb_x = x, nb_y = y),
      by = join_by(date >= nb_list_start, date < nb_list_end),
      relationship = "many-to-many"
    ) |>
    mutate(
      dist = if_else(!is.na(nb_x) & !is.na(nb_y), sqrt((x - nb_x)^2 + (y - nb_y)^2), NA_real_)
    ) |>
    filter(!is.na(dist) & dist > 0 & dist < spatial_radius) |>
    summarise(
      locac_lists = n(),
      .by = c("asset_id", "date")
    ) |>
    right_join(splistings, by = c("asset_id", "date")) |>
    mutate(
      locac_lists = replace_na(locac_lists, 0L)
    ) |>
    arrange(date)
  
  mslistings <- splistings |>
    left_join(
      spasks |> select(nb_ask_start = ask_start, nb_ask_end = ask_end, nb_asker = asker, nb_x = x, nb_y = y),
      by = join_by(date >= nb_ask_start, date < nb_ask_end),
      relationship = "many-to-many"
    ) |>
    mutate(
      dist = if_else(!is.na(nb_x) & !is.na(nb_y), sqrt((x - nb_x)^2 + (y - nb_y)^2), NA_real_)
    ) |>
    filter(!is.na(dist) & dist > 0 & dist < spatial_radius) |>
    summarise(
      locac_asks = n(),
      locac_askers = n_distinct(nb_asker),
      .by = c("asset_id", "date")
    ) |>
    right_join(mslistings, by = c("asset_id", "date")) |>
    mutate(
      locac_asks = replace_na(locac_asks, 0L),
      locac_askers = replace_na(locac_askers, 0L)
    ) |>
    mutate(
      locac_pressure = if_else(locac_lists == 0, 0L, locac_asks / locac_lists)
    ) |>
    relocate(locac_pressure, .after = locac_lists) |>
    arrange(date)
  
  mslistings <- splistings |>
    mutate(
      window_start = floor_date(list_start - ddays(time_window), unit = "day"),
      window_end = floor_date(list_start, unit = "day")
    ) |>
    left_join(
      splistings |> 
        filter(status == "filled") |> 
        select(nb_sale_date = list_end, nb_x = x, nb_y = y, nb_tom = tom) |> 
        mutate(nb_tom = if_else(nb_tom > 365, 365, nb_tom)),
      by = join_by(window_start <= nb_sale_date, window_end > nb_sale_date),
      relationship = "many-to-many"
    ) |>
    mutate(
      dist = if_else(!is.na(nb_x) & !is.na(nb_y), sqrt((x - nb_x)^2 + (y - nb_y)^2), NA_real_)
    ) |>
    filter(!is.na(dist) & dist > 0 & dist < spatial_radius) |>
    summarise(
      locrc_sales = n(),
      locrc_sales_mtom = median(nb_tom),
      locrc_sales_atom = mean(nb_tom),
      .by = c("asset_id", "date")
    ) |>
    right_join(mslistings, by = c("asset_id", "date")) |>
    mutate(
      locrc_sales = replace_na(locrc_sales, 0L),
      locrc_sales_mtom = replace_na(locrc_sales_mtom, 0L),
      locrc_sales_atom = replace_na(locrc_sales_atom, 0L)
    ) |>
    arrange(date)
  
  mslistings <- splistings |>
    mutate(
      window_start = floor_date(list_start - ddays(time_window), unit = "day"),
      window_end = floor_date(list_start, unit = "day")
    ) |>
    left_join(
      splistings |> select(nb_list_start = list_start, nb_list_end = list_end, nb_status = status, nb_x = x, nb_y = y, nb_asset_id = asset_id, nb_maker = maker),
      by = join_by(window_start <= nb_list_start, window_end > nb_list_start, window_start <= nb_list_end, window_end > nb_list_end),
      relationship = "many-to-many"
    ) |>
    mutate(
      dist = if_else(!is.na(nb_x) & !is.na(nb_y), sqrt((x - nb_x)^2 + (y - nb_y)^2), NA_real_)
    ) |>
    filter(!is.na(dist) & dist > 0 & dist < spatial_radius) |>
    summarise(
      locrc_lists = n_distinct(nb_asset_id),
      locrc_slrat = sum(nb_status == "filled") / n_distinct(nb_asset_id, nb_maker),
      locrc_flrat = sum(nb_status != "filled") / n(),
      .by = c("asset_id", "date")
    ) |>
    right_join(mslistings, by = c("asset_id", "date")) |>
    mutate(
      locrc_lists = replace_na(locrc_lists, 0L),
      locrc_slrat = replace_na(locrc_slrat, 0L),
      locrc_flrat = replace_na(locrc_flrat, 0L)
    ) |>
    arrange(date)
  
  mslistingsdb <- mslistings |>
    filter(tom > 0) |>
    mutate(
      LocalListings = log1p(locac_lists),
      LocalDemand = factor(if_else(locac_pressure > 0, "YES", "NO"), levels = c("NO", "YES")),
      LocalPastLists = factor(if_else(locrc_lists == 0, "ZERO", if_else(locrc_lists < 5, "LOW", "HIGH")), levels = c("ZERO", "LOW", "HIGH")),
      LocalPastLSRatio = locrc_slrat,
      LocalPastSales = factor(if_else(locrc_sales > 0, "YES", "NO"), levels = c("NO", "YES")),
      LocalPastTom = Winsorize(locrc_sales_atom, val = quantile(locrc_sales_atom, c(0.01, 0.99))),
      Sold = as.numeric(status == "filled" & !is.na(tom) & tom <= 365),
      Tom = if_else(tom > 365, 365, tom),
      LogPrice = log(Winsorize(price_usd, val = quantile(price_usd, probs = c(0.01, 0.99))))
    ) |>
    transmute(
      asset_id,
      Date = date,
      Sold,
      Tom,
      LogPrice,
      LocalListings,
      LocalDemand,
      LocalPastLists,
      LocalPastLSRatio,
      LocalPastSales,
      LocalPastTom,
    )
  
  rm(list = c("splistings", "spasks", "mslistings"))
  mslistingsdb <- locations_merger(mslistingsdb)
  
  return(mslistingsdb)
}

# Spatial liquidity db H8
splq_db_r10 <- spatial_microstructure_db_builder(spatial_radius = 10, time_window = 30)
saveRDS(splq_db_r10, file = "splq_db_r10.RDS")
splq_db_r15 <- spatial_microstructure_db_builder(spatial_radius = 15, time_window = 30)
saveRDS(splq_db_r15, file = "splq_db_r15.RDS")
splq_db_r25 <- spatial_microstructure_db_builder(spatial_radius = 25, time_window = 30)
saveRDS(splq_db_r25, file = "splq_db_r25.RDS")

# Order prices db H10
opr_ls_db <- locations_merger(cplistings)
opr_as_db <- locations_merger(cpasks)
saveRDS(list(ls=opr_ls_db, as=opr_as_db), file = "opr_db.RDS")

# Spread db H10
# In `base_hypo_dclmkp.R`

# Relistings Database
relisting_db_builder <- function (edit_time_length) {
  x1 <- listings |>
    arrange(asset_id, date) |>
    group_by(asset_id) |>
    mutate(
      prev_list_end = dplyr::lag(cummax(as.numeric(list_end))),
      prev_status = dplyr::lag(status),
      prev_canceled_by = dplyr::lag(canceled_by),
      prev_price_raw = dplyr::lag(price_raw),
      prev_tom = dplyr::lag(tom),
      cumm_tom = cumsum(tom),
      is_new_listing = if_else(
        is.na(prev_list_end),
        1L,
        if_else(
          #as.numeric(list_start) <= prev_list_end + 5 & prev_status == "canceled" & prev_canceled_by != "transfer" & (prev_price_raw == price_raw | prev_tom < 30*30/86400),
          as.numeric(list_start) <= prev_list_end + 5 & prev_status == "canceled" & prev_canceled_by != "transfer" & prev_tom < edit_time_length*60/86400,
          0L,
          1L
        )
      ),
      listing = cumsum(is_new_listing)
    ) |>
    ungroup() |>
    summarise(
      date = first(date),
      maker = first(maker),
      tom = sum(tom),
      list_start = first(list_start),
      list_end = last(list_end),
      status = last(status),
      n_price_usd = last(price_usd),
      n_price_raw = last(price_raw),
      canceled_by = last(canceled_by),
      edit_n = n()-1,
      edit_price = last(price_raw)-first(price_raw),
      .by = c("asset_id", "listing")
    ) |>
    rename(
      price_usd = n_price_usd,
      price_raw = n_price_raw,
    ) |>
    select(-listing)
  
  x3 <- x1 |>
    arrange(asset_id, date) |>
    group_by(asset_id) |>
    mutate(
      prev_list_end = dplyr::lag(cummax(as.numeric(list_end))),
      prev_status = dplyr::lag(status),
      prev_canceled_by = dplyr::lag(canceled_by),
      prev_price_raw = dplyr::lag(price_raw),
      prev_tom = dplyr::lag(tom),
      is_new_listing = if_else(
        is.na(prev_list_end),
        1L,
        if_else(
          as.numeric(list_start) <= prev_list_end + 5 & prev_status == "canceled" & prev_canceled_by != "transfer",
          0L,
          1L
        )
      ),
      listing = cumsum(is_new_listing)
    ) |>
    select(-prev_list_end, -prev_status, -prev_canceled_by, -prev_price_raw, -prev_tom) |>
    ungroup() 
  
  x2 <- x3 |>
    select(asset_id, date, list_start, list_end) |>
    left_join(
      asks |> select(asset_id, ask_date, ask_start, ask_end, asker, price_raw, price_usd),
      by = join_by(asset_id, list_start <= ask_date, list_end > ask_date),
      relationship = "many-to-many"
    ) |>
    filter(!is.na(ask_date)) |>
    summarise(
      asks_n = n(),
      askers_n = n_distinct(asker),
      ask_price = mean(price_raw),
      .by = c("asset_id", "date")
    ) |>
    right_join(
      x3,
      by = join_by(asset_id, date)
    ) |>
    mutate(
      asks_n = replace_na(asks_n, 0L),
      askers_n = replace_na(askers_n, 0L),
      ask_price = replace_na(ask_price, 0L),
      ask_spread_rat = ask_price / price_raw
    ) |>
    group_by(asset_id, listing) |>
    mutate(
      update_v = if_else(is_new_listing == 0, 1L, NA),
      update_p = if_else(is_new_listing == 0, if_else(dplyr::lag(price_raw) == price_raw, 0L, 1L), NA),
      tom_cm = cumsum(tom),
      tom_pv = if_else(is_new_listing == 0, cumsum(dplyr::lag(tom, default = 0)), NA),
      spread_rat = if_else(is_new_listing == 0, price_raw / dplyr::lag(price_raw), NA),
      spread_sub = if_else(is_new_listing == 0, price_usd * (1 - dplyr::lag(price_raw)/price_raw), NA),
      spread_cm_rat = if_else(is_new_listing == 0, price_raw / first(price_raw), NA),
      spread_cm_sub = if_else(is_new_listing == 0, price_usd * (1 - first(price_raw)/price_raw), NA),
      ext_list_start = first(list_start),
      ext_list_end = last(list_end)
    ) |>
    mutate(
      fw_update_v = dplyr::lead(update_v),
      fw_update_p = dplyr::lead(update_p),
      fw_spread_rat = dplyr::lead(spread_rat),
      fw_spread_sub = dplyr::lead(spread_sub),
      ask_spread_sub = if_else(ask_price != 0, (ask_price - price_raw)*(dplyr::lead(price_usd)/dplyr::lead(price_raw)), NA)
    ) |>
    ungroup() |>
    relocate(
      ask_spread_sub, .after = ask_spread_rat
    ) |>
    relocate(
      all_of(c("asks_n", "askers_n", "ask_price", "ask_spread_rat", "ask_spread_sub")), .after = fw_spread_sub
    ) |>
    arrange(date) |>
    select(-is_new_listing, -listing)
  
  return(x2)
}

relistings <- relisting_db_builder(30)

################################


fit = cox_model(
  mslistingsdb |> filter((year(Date) == 2019 & quarter(Date) > 1) | year(Date) == 2020) |> 
    mutate(qname = paste0("Q",quarter(Date),"_",year(Date))) |>
    mutate(v = 1) |> 
    pivot_wider(names_from = qname, values_from = v, values_fill = 0),
  Y_time = "Tom",
  Y_event = "Sold",
  X = c("LocalListings", "LocalDemand", "LocalPastLSRatio", "LocalPastTom", "DPC", "CPX", "CRD", "CDX", "North", "LogPrice", "Q3_2019", "Q4_2019", "Q1_2020", "Q2_2020", "Q3_2020", "Q4_2020"),
  error_cluster = "asset_id"
)
summary(fit)
get_gof(fit)

x = locations_merger(cplistings)
x = x |> 
  mutate(
    discount = price_usd_l / (price_usd_l - price_usd_delta),
    discounted = as.numeric(discount < 1),
    augmented = as.numeric(discount > 1)
  ) |>
  filter(relistings > 0) |>
  filter(discount > quantile(discount, 0.25) - 1.5*IQR(discount), discount < quantile(discount, 0.75) + 1.5*IQR(discount))

y = logit_model(
  dataset = x |> filter(year(date) == 2019 | year(date) == 2020) |> 
    mutate(qname = paste0("Q",quarter(date),"_",year(date))) |>
    mutate(v = 1) |> 
    pivot_wider(names_from = qname, values_from = v, values_fill = 0), 
  Y = c("discounted"), 
  X = c("DPC", "CPX", "CRD", "CDX", "North", "Q2_2019", "Q3_2019", "Q4_2019", "Q1_2020", "Q2_2020", "Q3_2020", "Q4_2020")
)
summary(y)

y = logit_model(
  dataset = x |> filter(year(date) == 2019 | year(date) == 2020) |> 
    mutate(qname = paste0("Q",quarter(date),"_",year(date))) |>
    mutate(v = 1) |> 
    pivot_wider(names_from = qname, values_from = v, values_fill = 0), 
  Y = c("augmented"), 
  X = c("DPC", "CPX", "CRD", "CDX", "North", "Q2_2019", "Q3_2019", "Q4_2019", "Q1_2020", "Q2_2020", "Q3_2020", "Q4_2020")
)
summary(y)

y = lm_model(
  dataset = x |> filter(year(date) == 2019 | year(date) == 2020) |> 
    mutate(qname = paste0("Q",quarter(date),"_",year(date))) |>
    mutate(v = 1) |> 
    pivot_wider(names_from = qname, values_from = v, values_fill = 0), 
  Y = c("discount"), 
  X = c("DPC", "CPX", "CRD", "CDX", "North", "Q2_2019", "Q3_2019", "Q4_2019", "Q1_2020", "Q2_2020", "Q3_2020", "Q4_2020")
)
summary(y$model)


fit = cox_model(
  dataset = x |> filter(year(date) == 2019 | year(date) == 2020) |> 
    mutate(qname = paste0("Q",quarter(date),"_",year(date))) |>
    mutate(v = 1) |> 
    pivot_wider(names_from = qname, values_from = v, values_fill = 0) |>
    mutate(
      Sold = as.numeric(status == "filled" & !is.na(tom) & tom <= 365),
      Tom = if_else(tom > 365, 365, tom),
      LogPrice = log(Winsorize(price_usd_l, val = quantile(price_usd_l, probs = c(0.01, 0.99)))),
      ChPrice = factor(if_else(discount == 1, "None", if_else(discount < 1, "Disc", "Aug")), levels = c("None", "Disc", "Aug")),
      Discounted = factor(if_else(discounted == 1, "Yes", "No"), levels = c("No", "Yes")),
      Augmented = factor(if_else(augmented == 1, "Yes", "No"), levels = c("No", "Yes"))
    ),
  Y_time = "Tom",
  Y_event = "Sold",
  X = c("Augmented", "DPC", "CPX", "CRD", "CDX", "North", "LogPrice", "Q2_2019", "Q3_2019", "Q4_2019", "Q1_2020", "Q2_2020", "Q3_2020", "Q4_2020"),
  error_cluster = "asset_id"
)
summary(fit)
get_gof(fit)







