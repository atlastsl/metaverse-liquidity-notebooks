
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
####################################

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
LocationsEstates <- read_excel(
  "../Donnees/LocationsEstates.xlsx"
)

####################################
####### Summary Statistics #########
####################################

saveRDS(
  Tx_DclMkp |> mutate(quarter = lubridate::quarter(date)),
  file = "artdata/transactions.RDS"
)

volume_summary_builder <- function (period, filter_func, aggr_func = function(db){return(sum(db[["amount_usd"]])/1000)}) {
  smr <- NULL
  if (period == "year") {
    smr <- Tx_DclMkp |>
      filter(filter_func(.data)) |>
      distinct(asset_id, date, .keep_all = T) |>
      mutate(year = as.character(year(date))) |>
      summarise(
        fs_vol = aggr_func(.data),
        .by = c("year")
      ) |>
      bind_rows(
        Tx_DclMkp |>
          filter(filter_func(.data)) |>
          distinct(asset_id, date, .keep_all = T) |>
          summarise(
            year = "Total",
            fs_vol = aggr_func(.data),
          )
      ) 
  } else {
    smr <- Tx_DclMkp |>
      filter(filter_func(.data)) |>
      distinct(asset_id, date, .keep_all = T) |>
      mutate(year = as.character(year(date))) |>
      mutate(month = lubridate::month(date)) |>
      summarise(
        fs_vol = aggr_func(.data),
        .by = c("year", "month")
      ) |>
      bind_rows(
        Tx_DclMkp |>
          filter(filter_func(.data)) |>
          distinct(asset_id, date, .keep_all = T) |>
          summarise(
            year = "Total",
            month = 0,
            fs_vol = aggr_func(.data),
          )
      )
  }
  return(smr)
}

volume_summary_func <- function (period) {
  fsales <- volume_summary_builder(period, function(db) {
    return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["sale_related_market"]] %in% c("auction-1", "auction-2"))
  })
  ssales_l1 <- volume_summary_builder(period, function(db) {
    return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["asset_contract"]] == LandContract & db[["sale_related_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
  })
  ssales_e1 <- volume_summary_builder(period, function(db) {
    return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["asset_contract"]] == EstateContract & db[["sale_related_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
  })
  ssales_l2 <- volume_summary_builder(period, function(db) {
    return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["asset_contract"]] == LandContract & db[["sale_related_market"]] %in% c("third-party-marketplace"))
  })
  ssales_e2 <- volume_summary_builder(period, function(db) {
    return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["asset_contract"]] == EstateContract & db[["sale_related_market"]] %in% c("third-party-marketplace"))
  })
  lsts_l <- volume_summary_builder(period, function(db) {
    return(db[["type"]] == "order" & db[["order_type"]] == "list" & db[["asset_contract"]] == LandContract & db[["order_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
  }, function(db) {
    return(dplyr::n())
  })
  lsts_e <- volume_summary_builder(period, function(db) {
    return(db[["type"]] == "order" & db[["order_type"]] == "list" & db[["asset_contract"]] == EstateContract & db[["order_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
  }, function(db) {
    return(dplyr::n())
  })
  asks_l <- volume_summary_builder(period, function(db) {
    return(db[["type"]] == "order" & db[["order_type"]] == "ask" & db[["asset_contract"]] == LandContract & db[["order_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
  }, function(db) {
    return(dplyr::n())
  })
  asks_e <- volume_summary_builder(period, function(db) {
    return(db[["type"]] == "order" & db[["order_type"]] == "ask" & db[["asset_contract"]] == EstateContract & db[["order_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
  }, function(db) {
    return(dplyr::n())
  })
  vol_smr <- NULL
  if (period == "year") {
    vol_smr <- tibble(data.frame(year=c(as.character(2018:2026), "Total"))) |>
      left_join(fsales |> rename(fsales = fs_vol), by = c("year")) |>
      left_join(ssales_l1 |> rename(ssales_l1 = fs_vol), by = c("year")) |>
      left_join(ssales_e1 |> rename(ssales_e1 = fs_vol), by = c("year")) |>
      left_join(ssales_l2 |> rename(ssales_l2 = fs_vol), by = c("year")) |>
      left_join(ssales_e2 |> rename(ssales_e2 = fs_vol), by = c("year")) |>
      left_join(lsts_l |> rename(lsts_l = fs_vol), by = c("year")) |>
      left_join(lsts_e |> rename(lsts_e = fs_vol), by = c("year")) |>
      left_join(asks_l |> rename(asks_l = fs_vol), by = c("year")) |>
      left_join(asks_e |> rename(asks_e = fs_vol), by = c("year"))
  } else {
    vol_smr <- tibble(
      rbind(
        data.table::CJ(year = as.character(2018:2026), month = 1:12),
        data.frame(year = "Total", month = 0)
      )
    ) |>
      left_join(fsales |> rename(fsales = fs_vol), by = c("year", "month")) |>
      left_join(ssales_l1 |> rename(ssales_l1 = fs_vol), by = c("year", "month")) |>
      left_join(ssales_e1 |> rename(ssales_e1 = fs_vol), by = c("year", "month")) |>
      left_join(ssales_l2 |> rename(ssales_l2 = fs_vol), by = c("year", "month")) |>
      left_join(ssales_e2 |> rename(ssales_e2 = fs_vol), by = c("year", "month")) |>
      left_join(lsts_l |> rename(lsts_l = fs_vol), by = c("year", "month")) |>
      left_join(lsts_e |> rename(lsts_e = fs_vol), by = c("year", "month")) |>
      left_join(asks_l |> rename(asks_l = fs_vol), by = c("year", "month")) |>
      left_join(asks_e |> rename(asks_e = fs_vol), by = c("year", "month"))
  }
  return(vol_smr)
}

voly_smr <- volume_summary_func("year")
saveRDS(voly_smr, file = "artdata/volysummary.RDS")

volm_smr <- volume_summary_func("month")
saveRDS(volm_smr, file = "artdata/volmsummary.RDS")


####################################
####### Analysis Db Builder ########
####################################

# Transaction data with Location Data merge function
locations_merger <- function (database, assetType = "land") {
  loc_merged_db <- NULL
  tp_threshold <- 12
  if (assetType == "land") {
    loc_merged_db <- database |>
      left_join(
        Locations |> 
          mutate(
            CPX = factor(if_else(DIST_NRS_PLAZA > tp_threshold, "No", "Yes"), levels = c("No", "Yes")),
            CRD = factor(if_else(DIST_ROAD > tp_threshold, "No", "Yes"), levels = c("No", "Yes")),
            CDX = factor(if_else(DIST_NRS_DISTRICT_CAT > tp_threshold, "No", "Yes"), levels = c("No", "Yes")),
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
          ) |>
          mutate(
            DPC = log1p(DPC)
          ),
        by = "asset_id"
      ) 
  }
  else if (assetType == "estate") {
    loc_merged_db <- database |>
      select(asset_id, date) |>
      left_join(
        LocationsEstates |> 
          mutate(
            CPX = factor(if_else(DIST_NRS_PLAZA > tp_threshold, "No", "Yes"), levels = c("No", "Yes")),
            CRD = factor(if_else(DIST_ROAD > tp_threshold, "No", "Yes"), levels = c("No", "Yes")),
            CDX = factor(if_else(DIST_NRS_DISTRICT_CAT > tp_threshold, "No", "Yes"), levels = c("No", "Yes")),
            #North = factor(if_else(Y < 0, "No", "Yes"), levels = c("No", "Yes")),
            IDX = factor(if_else(DIST_NRS_DISTRICT_CAT > 0, "No", "Yes"), levels = c("No", "Yes"))
          ) |>
          select(
            asset_id = TOKEN_ID,
            loc_date = UPDATED_AT,
            size     = NEW_SIZE,
            DPC      = DIST_PLAZA_central,
            CPX,
            CRD,
            CDX,
            #North,
            IDX
          ) |>
          mutate(
            DPC = log1p(DPC)
          ),
        by = join_by(asset_id, date > loc_date),
        relationship = "many-to-many"
      ) |>
      summarise(
        loc_date = last(loc_date, na_rm = T),
        size = last(size, na_rm = T),
        DPC = last(DPC, na_rm = T),
        CPX = last(CPX, na_rm = T),
        CRD = last(CRD, na_rm = T),
        CDX = last(CDX, na_rm = T),
        #North = last(North, na_rm = T),
        IDX = last(IDX, na_rm = T),
        .by = c("asset_id", "date")
      )
    loc_merged_db <- database |>
      right_join(
        loc_merged_db,
        by = c("asset_id", "date")
      )
  }
  return(loc_merged_db)
}

# Listing Database builder
listings_db_builder <- function (assetType = "land") {
  
  assetContract = ""
  if (assetType == "land") {
    assetContract = LandContract
  } else if (assetType == "estate") {
    assetContract = EstateContract
  }
  
  # Step 1: Listings extraction & edited listings (less than 60min after listing posted) correction
  date_now <- lubridate::now()
  listings_base <- Tx_DclMkp |>
    filter(asset_contract == assetContract & type == "order" & order_type == "list" & order_market == "dcl-marketplace-1") |>
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
      # regime = case_when(
      #   year(date) %in% 2019:2020 ~ "Normal",
      #   year(date) %in% 2021 ~ "Boom",
      #   year(date) %in% 2022 ~ "Crash",
      #   year(date) %in% 2023:2024 ~ "Desert",
      #   TRUE ~ NA_character_
      # )
      regime = case_when(
        as.numeric(date) < as.numeric(as.POSIXct("2019-04-01", tz = "UTC")) ~ NA_character_,
        as.numeric(date) < as.numeric(as.POSIXct("2021-03-01", tz = "UTC")) ~ "Normal",
        as.numeric(date) < as.numeric(as.POSIXct("2022-03-01", tz = "UTC")) ~ "Boom",
        as.numeric(date) < as.numeric(as.POSIXct("2023-04-01", tz = "UTC")) ~ "Crash",
        as.numeric(date) < as.numeric(as.POSIXct("2025-01-01", tz = "UTC")) ~ "Desert",
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
    filter(asset_contract == assetContract, type == "order", order_type == "ask", order_market == "dcl-marketplace-1") |>
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
      listings_base |> select(asset_id, past_list_start = list_start, past_list_end = list_end, value = price_usd, status),
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
      bintl_status = last(status),
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
      nfilled_li_30d = sum(bintl_status == "filled"),
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
          ndays_li_30d, mvalue_li_30d, nfilled_li_30d,
          asks_ul_30d, askers_ul_30d,
          asks_ll_30d, askers_ll_30d, ndays_ll_30d
        ),
        ~ replace_na(.x, 0L)
      )
    )
  
  # Step 10: Global Investors attention measure
  # listings <- listings |>
  #   left_join(
  #     readRDS("glt_db.RDS") |>
  #       rename(day = date, att_vol = vol_tot) |>
  #       select(day, att_vol), 
  #     by = c("day")
  #   ) |>
  #   left_join(
  #     readRDS("gtt_db.RDS") |>
  #       rename(day = date, att_gt = lagged.wtrend) |>
  #       select(day, att_gt), 
  #     by = c("day")
  #   )
  
  # Step 11: Merge locations
  listings <- locations_merger(listings, assetType)
  
  return(list(listings_base = listings_base, asks_base = asks_base, listings = listings))
}

lst_data <- listings_db_builder("land")
saveRDS(lst_data, file = "artdata/artdatalst.RDS")
listings <- lst_data$listings
View(lst_data$asks_base)

elst_data <- listings_db_builder("estate")
saveRDS(elst_data, file = "artdata/artdataelst.RDS")
elistings <- elst_data$listings
View(elst_data$asks_base)












