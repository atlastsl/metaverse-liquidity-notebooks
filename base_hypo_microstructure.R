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







