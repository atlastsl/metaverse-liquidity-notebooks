
library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)

#############################################################################
# Script R de calcul pour le papier final du chapitre 1 v. Attention ########
#############################################################################

LandContract <- "0xf87e31492faf9a91b02ee0deaad50d51d56d5d4d"
EstateContract <- "0x959e104e1a4db6317fa58f8295f586e1a978c297"

#############################################################################
#############################################################################
######### DECENTRALAND MARKETPLACE ##########################################
#############################################################################
#############################################################################

####################################
####### DATABASES ##################

## Dcl Marketplace Data
Tx_DclMkp <- read_excel("../Donnees/OnChainDataAll4.xlsx") |> filter(!is.na(type))

## Locations Data
Locations <- read_excel(
  "../Donnees/LocationsFull.xlsx", 
  col_types = c(
    "numeric", "numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "numeric", "numeric", "numeric", "text"
  )
)

# Transaction data with Location Data merge function
locations_merger <- function (database) {
  loc_merged_db <- database |>
    left_join(
      Locations |> 
        mutate(
          CPX = factor(if_else(DIST_NRS_PLAZA > 10, "No", "Yes"), levels = c("No", "Yes")),
          CRD = factor(if_else(DIST_ROAD > 10, "No", "Yes"), levels = c("No", "Yes")),
          CDX = factor(if_else(DIST_NRS_DISTRICT_CAT > 10, "No", "Yes"), levels = c("No", "Yes")),
          North = factor(if_else(Y < 0, "No", "Yes"), levels = c("No", "Yes")),
          IDX = factor(if_else(TYPE != "district" & DIST_NRS_DISTRICT_CAT > 0, "No", "Yes"), levels = c("No", "Yes"))
        ) |>
        select(
          asset_id = TOKEN_ID,
          DPC      = DIST_PLAZA_central,
          CPX,
          CRD,
          CDX,
          North,
          IDX
          # DPX      = DIST_NRS_PLAZA,
          # NPX      = NAME_NRS_PLAZA,
          # DRD      = DIST_ROAD,
          # DDX      = DIST_NRS_DISTRICT_CAT,
          # NDX      = NAME_NRS_DISTRICT_CAT,
          # DPXn     = DIST_PLAZA_north,
          # DPXs     = DIST_PLAZA_south,
          # DPXe     = DIST_PLAZA_east,
          # DPXw     = DIST_PLAZA_west,
          # DPXne    = `DIST_PLAZA_north-east`,
          # DPXse    = `DIST_PLAZA_south-east`,
          # DPXnw    = `DIST_PLAZA_north-west`,
          # DPXsw    = `DIST_PLAZA_south-west`
        ) |>
        mutate(
          DPC = log1p(DPC) #,
          # DPX = log1p(DPX),
          # DRD = log1p(DRD),
          # DDX = log1p(DDX),
          # DPXn = log1p(DPXn),
          # DPXs = log1p(DPXs),
          # DPXe = log1p(DPXe),
          # DPXw = log1p(DPXw),
          # DPXne = log1p(DPXne),
          # DPXse = log1p(DPXse),
          # DPXnw = log1p(DPXnw),
          # DPXsw = log1p(DPXsw),
        ),
      by = "asset_id"
    )
  
  return(loc_merged_db)
}

# Listing Database builder
listings_db_builder <- function () {
  
  # Step 1: Listings extraction & edited listings (less than 60min after listing posted) correction
  date_now <- lubridate::now()
  listings_base <- Tx_DclMkp |>
    filter(asset_contract == LandContract & type == "order" & order_type == "list" & order_market == "dcl-marketplace-1") |>
    select(hash, order_id, asset_id, maker, date, order_time_on_market, order_status, order_canceled_by, amount_usd, currency, amount) |>
    transmute( 
      # Rearrange listing database, create listing period (interval [Date, Date+Tom)), and 30 days lookback interval
      asset_id,
      date,
      maker,
      tom = order_time_on_market,
      list_start = date,
      list_end = as.POSIXct(ifelse(order_status == "pending", date_now, date + dmilliseconds(floor(order_time_on_market*86400*1000))), tz = "UTC"),
      status = order_status,
      price_usd = amount_usd,
      price_raw = amount,
      canceled_by = order_canceled_by
    ) |>
    distinct(asset_id, date, .keep_all = T) |>
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
          as.numeric(list_start) <= prev_list_end + 5 & prev_status == "canceled" & prev_canceled_by != "transfer" & prev_tom < 60*60/86400,
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
      price_usd = last(price_usd),
      price_raw = last(price_raw),
      canceled_by = last(canceled_by),
      edit_n = n()-1,
      edit_price = last(price_raw)-first(price_raw),
      .by = c("asset_id", "listing")
    ) |>
    select(-listing) |>
    filter(tom > 0) |>
    mutate(
      day = floor_date(date, unit = "day"),
      regime = case_when(
        year(date) %in% 2019:2020 ~ "Normal",
        year(date) %in% 2021 ~ "Boom",
        year(date) %in% 2022 ~ "Crash",
        year(date) %in% 2023:2024 ~ "Desert",
        TRUE ~ NA_character_
      )
    )
  
  # Step 2: Listing 30 previous days interval instanciation
  interval_base <- listings_base |>
    select(asset_id, date) |>
    mutate(
      interval_start = pmax(floor_date(date - ddays(30), unit = "day"), floor_date(min(Tx_DclMkp$date, na.rm = T), unit = "day")),
      interval_end = floor_date(date, unit = "day")
    )
  
  # Step 3: Sollicitations (Asks) database extraction
  asks_base <- Tx_DclMkp |>
    filter(asset_contract == LandContract, type == "order", order_type == "ask", order_market == "dcl-marketplace-1") |>
    select(hash, order_id, asset_id, date, taker, order_time_on_market, order_status, order_canceled_by, amount_usd, currency, amount) |>
    transmute( 
      # Rearrange asks database, create asks periods (interval [Date, Date+Tom))
      asset_id,
      ask_date = date,
      asker = taker,
      tom = order_time_on_market,
      ask_start = date,
      ask_end = as.POSIXct(ifelse(order_status == "pending", date_now, date + dmilliseconds(floor(order_time_on_market*86400*1000))), tz = "UTC"),
      status = order_status,
      price_usd = amount_usd,
      price_raw = amount,
      canceled_by = order_canceled_by
    ) |>
    distinct(asset_id, ask_date, .keep_all = T) |>
    left_join(
      listings_base |> select(asset_id, list_start, list_end),
      by = join_by(asset_id, ask_date >= list_start, ask_date < list_end)
    ) |>
    select(-list_end) |>
    rename(list_date = list_start)
  
  # Step 4: Construction of Listed Periods in intervals 
  listed_periods <- interval_base |>
    select(asset_id, date, interval_start, interval_end) |>
    left_join(
      listings_base |> select(asset_id, past_list_start = list_start, past_list_end = list_end, value = price_usd),
      by = join_by(asset_id, interval_end > past_list_start, interval_start < past_list_end),
      relationship = "many-to-many"
    ) |>
    filter(!is.na(past_list_start)) |>
    mutate(
      intl_start = pmax(past_list_start, interval_start),
      intl_end = pmin(past_list_end, interval_end)
    ) |>
    filter(intl_start < intl_end) |>
    arrange(asset_id, date, intl_start) |>
    group_by(asset_id, date) |>
    mutate(
      prev_max_inl_end = dplyr::lag(cummax(as.numeric(intl_end))),
      new_block = if_else(is.na(prev_max_inl_end) | as.numeric(intl_start) > prev_max_inl_end + 5, 1L, 0L),
      block = cumsum(new_block)
    ) |>
    ungroup() |>
    group_by(asset_id, date, block) |>
    summarise(
      interval_start = first(interval_start),
      interval_end = first(interval_end),
      bintl_start = min(intl_start),
      bintl_end = max(intl_end),
      bintl_mvalue = mean(value),
      bintl_n = n(),
      .groups = "drop"
    ) |>
    select(-block)
  
  # Step 5: Construction of non listed periods in interval
  nonlisted_periods_from_listed <- listed_periods |>
    arrange(asset_id, date) |>
    group_by(asset_id, date, interval_start, interval_end) |>
    reframe(
      bnonl_start = c(first(interval_start), bintl_end),
      bnonl_end = c(bintl_start, first(interval_end)),
    ) |>
    ungroup() |>
    filter(bnonl_start < bnonl_end)
  
  nonlisted_periods_unlisted <- interval_base |>
    select(asset_id, date, interval_start, interval_end) |>
    anti_join(listed_periods |> distinct(asset_id, date), by = c("asset_id", "date")) |>
    transmute(
      asset_id,
      date,
      bnonl_start = interval_start,
      bnonl_end = interval_end
    )
  
  nonlisted_periods <- bind_rows(nonlisted_periods_from_listed, nonlisted_periods_unlisted)
  
  # Step 6: Statistic of listed periods
  listed_periods_stats <- listed_periods |>
    summarise(
      ndays_li_30d = sum(time_length(bintl_end - bintl_start, unit = "second"), na.rm = T)/86400,
      mvalue_li_30d = sum(bintl_mvalue * bintl_n, na.rm = T) / sum(bintl_n),
      .by = c("asset_id", "date")
    )
  
  # Step 7: Statistics of non listed periods
  nonlisted_periods_stats <- nonlisted_periods |>
    left_join(
      asks_base |> select(asset_id, ask_date, asker),
      by = join_by(asset_id, bnonl_start <= ask_date, bnonl_end > ask_date),
      relationship = "many-to-many"
    ) |>
    summarise(
      asks_ul_30d = sum(!is.na(ask_date)),
      askers_ul_30d = n_distinct(asker[!is.na(ask_date)]),
      .by = c("asset_id", "date")
    )
  
  # Step 8: Statistics for whole interval
  whole_interval_stats <- interval_base |>
    select(asset_id, date, interval_start, interval_end) |>
    left_join(
      asks_base |> select(asset_id, ask_date, asker),
      by = join_by(asset_id, interval_start <= ask_date, interval_end > ask_date),
      relationship = "many-to-many"
    ) |>
    summarise(
      asks_ll_30d = sum(!is.na(ask_date)),
      askers_ll_30d = n_distinct(asker[!is.na(ask_date)]),
      .by = c("asset_id", "date")
    ) |>
    left_join(
      interval_base |>
        select(asset_id, date, interval_start, interval_end) |>
        summarise(
          ndays_ll_30d = sum(time_length(interval_end - interval_start, unit = "second"), na.rm = T)/86400,
          .by = c("asset_id", "date")
        ),
      by = c("asset_id", "date")
    )
  
  # Step 9: Database finalization
  listings <- interval_base |>
    left_join(listings_base, by = c("asset_id", "date")) |>
    left_join(listed_periods_stats, by = c("asset_id", "date")) |>
    left_join(nonlisted_periods_stats, by = c("asset_id", "date")) |>
    left_join(whole_interval_stats, by = c("asset_id", "date")) |>
    mutate(
      across(
        c(
          ndays_li_30d, mvalue_li_30d,
          asks_ul_30d, askers_ul_30d,
          asks_ll_30d, askers_ll_30d, ndays_ll_30d
        ),
        ~ replace_na(.x, 0L)
      )
    )
  
  # Step 10: Global Investors attention measure
  listings <- listings |>
    left_join(
      readRDS("glt_db.RDS") |>
        rename(day = date, att_vol = vol_tot) |>
        select(day, att_vol), 
      by = c("day")
    ) |>
    left_join(
      readRDS("gtt_db.RDS") |>
        rename(day = date, att_gt = lagged.wtrend) |>
        select(day, att_gt), 
      by = c("day")
    )
  
  # Step 11: Merge locations
  listings <- locations_merger(listings)
  
  return(list(listings_base = listings_base, asks_base = asks_base, listings = listings))
}
lst_data <- listings_db_builder()
saveRDS(lst_data, file = "artdata/artdatalst.RDS")

listings <- lst_data$listings
View(lst_data$asks_base)












