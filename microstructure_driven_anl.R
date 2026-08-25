
setwd("~/Aurelien/Recherche/C1/Explore")
rm(list = ls())

library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)

LandContract <- "0xf87e31492faf9a91b02ee0deaad50d51d56d5d4d"
EstateContract <- "0x959e104e1a4db6317fa58f8295f586e1a978c297"

####################
## Data loading
####################

## Dcl Marketplace

Tx_DclMkp <- read_excel("../Donnees/OnChainDataAll4.xlsx") |> filter(!is.na(type))

## Opensea

opensea_db_reader <- function () {
  
  # Opensea listings & sales
  opensea_ls <- read_excel(
    "../Donnees/Opensea.xlsx",
    col_types = c(
      "text", "text", "text", "text", "date", "date", "date", "text", "text", 
      "text", "text", "text", "text", "text", "text", "text", "text", "text", 
      "text", "text", "numeric", "numeric", "text", "text", "numeric", "numeric", 
      "text", "text", "text", "text", "text"
    )
  )
  # reformat dates entries
  opensea_ls$update_date <- as.POSIXct(format(opensea_ls$update_date), tz = "America/New_York")
  attr(opensea_ls$update_date, "tzone") <- "UTC"
  opensea_ls$expiration_date <- as.POSIXct(format(opensea_ls$expiration_date), tz = "America/New_York")
  attr(opensea_ls$expiration_date, "tzone") <- "UTC"
  opensea_ls$date <- as.POSIXct(format(opensea_ls$date), tz = "America/New_York")
  attr(opensea_ls$date, "tzone") <- "UTC"
  
  # Bids on private parcels.
  opensea_bids_o <- read_csv(
    "../Donnees/BidsAll.csv",  
    col_types = cols(
      asset_id = col_character(), date = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"), 
      start_date = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"), 
      expiration_date = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ")
    )
  )
  # Bids on private parcels and some non-private parcels (districts)
  opensea_bids_l <- read_excel(
    "../Donnees/Bids_opl_Opensea.xlsx", 
    col_types = c(
      "text", "text", "text", "text", "date", "text", "text", "text", 
      "text", "numeric", "numeric", "text", "date", "date"
    )
  )
  
  # Merge bids databases
  opensea_bids <- rbind(opensea_bids_o |> mutate(src="p"), opensea_bids_l |> mutate(src="l"))
  opensea_bids <- opensea_bids |>
    distinct(asset_id, date, bidder, amount, payment_ccy, .keep_all = T) |>
    group_by(asset_id, date, bidder, amount, payment_ccy) |>
    mutate(
      ct = n()
    ) |>
    ungroup() |>
    arrange(dplyr::desc(ct)) |>
    select(-ct, -src)
  rm(list = c("opensea_bids_o", "opensea_bids_l")) 
  
  return(list(opensea_ls=opensea_ls, opensea_bids=opensea_bids))
}

Tx_Ops <- opensea_db_reader()

## Locations

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

####################
## Summary
####################

### Volume summary builder ###

volume_summary_builder <- function (filter_func, aggr_func = function(db){return(sum(db[["amount_usd"]])/1000)}) {
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
  return(smr)
}

fsales <- volume_summary_builder(function(db) {
  return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["sale_related_market"]] %in% c("auction-1", "auction-2"))
})
ssales_l1 <- volume_summary_builder(function(db) {
  return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["asset_contract"]] == LandContract & db[["sale_related_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
})
ssales_e1 <- volume_summary_builder(function(db) {
  return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["asset_contract"]] == EstateContract & db[["sale_related_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
})
ssales_l2 <- volume_summary_builder(function(db) {
  return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["asset_contract"]] == LandContract & db[["sale_related_market"]] %in% c("third-party-marketplace"))
})
ssales_e2 <- volume_summary_builder(function(db) {
  return(db[["type"]] == "transfer" & db[["is_sale"]] == T & db[["asset_contract"]] == EstateContract & db[["sale_related_market"]] %in% c("third-party-marketplace"))
})
lsts_l <- volume_summary_builder(function(db) {
  return(db[["type"]] == "order" & db[["order_type"]] == "list" & db[["asset_contract"]] == LandContract & db[["order_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
}, function(db) {
  return(dplyr::n())
})
lsts_e <- volume_summary_builder(function(db) {
  return(db[["type"]] == "order" & db[["order_type"]] == "list" & db[["asset_contract"]] == EstateContract & db[["order_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
}, function(db) {
  return(dplyr::n())
})
asks_l <- volume_summary_builder(function(db) {
  return(db[["type"]] == "order" & db[["order_type"]] == "ask" & db[["asset_contract"]] == LandContract & db[["order_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
}, function(db) {
  return(dplyr::n())
})
asks_e <- volume_summary_builder(function(db) {
  return(db[["type"]] == "order" & db[["order_type"]] == "ask" & db[["asset_contract"]] == EstateContract & db[["order_market"]] %in% c("dcl-marketplace-1", "dcl-marketplace-2"))
}, function(db) {
  return(dplyr::n())
})
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
rm(list = c("fsales", "ssales_l1", "ssales_e1", "ssales_l2", "ssales_e2", "lsts_l", "lsts_e", "asks_l", "asks_e"))
saveRDS(vol_smr, file = "artdata/volsummary.RDS")


### Listings database construction & summary ###

# Listings db builder
listings_db_builder <- function () {
  
  # Actual date
  date_now <- lubridate::now()
  
  # Listings on Dcl marketplace V1
  listings <- Tx_DclMkp |>
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
  
  # Investors attention measure
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
  
  # Merge locations
  listings <- locations_merger(listings)
  
  return(listings)
}
listings <- listings_db_builder()

# Save listing database
saveRDS(listings, file = "artdata/listings.RDS")

listings |>
  select(regime, tom) |> 
  filter(!is.na(regime)) |>
  mutate(ltom = log1p(tom), regime=factor(regime, levels = c("Normal", "Boom", "Crash", "Desert"))) |>
  ggplot(aes(x = ltom)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  geom_vline(
    data = listings |> 
      select(regime, tom) |> 
      filter(!is.na(regime)) |> 
      summarise(q01 = log1p(quantile(tom, 0.5)), .by = "regime"),
    aes(xintercept = q01),
    colour = "red", 
    linetype = "dashed", 
    linewidth = 0.8
  ) +
  facet_wrap(~ regime, nrow = 2, ncol = 2, scales = "free") +
  theme_minimal()


### Liquidity provision database construction & summary ###

# Estates updates
library(jsonlite)
est_updates <- as_tibble(fromJSON("../Donnees/estates_updates.json")) |>
  mutate(
    n_added = lengths(lands_added),
    n_removed = lengths(lands_removed),
    n_diff = n_added - n_removed
  ) |>
  transmute(
    hash = transaction_hash,
    asset_id,
    n_added,
    n_removed,
    n_diff
  ) |>
  left_join(Tx_DclMkp |> select(hash, date), by = "hash") |>
  arrange(asset_id, date) |>
  group_by(asset_id) |>
  mutate(
    size = cumsum(n_diff)
  ) |>
  ungroup() |>
  arrange(asset_id, date)

# Transfers Database
transfers <- Tx_DclMkp |>
  filter(asset_contract == LandContract & type == "transfer") |>
  select(hash, asset_id, is_sale, sale_related_market, from, to, date, amount_usd, currency, amount) |>
  transmute( 
    # Rearrange listing database, create listing period (interval [Date, Date+Tom)), and 30 days lookback interval
    hash,
    asset_id,
    is_sale,
    market = sale_related_market,
    sender = from,
    recipient = to,
    date,
    price_usd = amount_usd,
    price_raw = amount,
    currency
  ) |>
  distinct(asset_id, date, .keep_all = T) |>
  mutate(
    price_usd = if_else(is_sale, price_usd, 0L)
  )

liq_provision_db_builder <- function(s.year, e.year) {
  s.year <- 2019
  e.year <- 2020
  panel <- 
    data.table::CJ(
      week.start = seq(
        from = lubridate::floor_date(as.POSIXct(paste0(s.year, "-01-01"), tz="UTC"), unit = "week", week_start = 1),
        to = lubridate::floor_date(as.POSIXct(paste0(e.year, "-12-31"), tz="UTC"), unit = "week", week_start = 1),
        by = "week"
      ),
      asset_id = unique((transfers |> filter(is_sale == T, year(date) <= e.year))$asset_id)
    ) |>
    group_by(asset_id) |>
    mutate(
      week.end = dplyr::lead(week.start)
    ) |>
    ungroup() |>
    filter(!is.na(week.end)) |>
    left_join(
      listings |> select(asset_id, date, maker),
      by = join_by(asset_id, week.start <= date, week.end > date),
      relationship = "many-to-many"
    ) |>
    summarise(
      n_lists = sum(!is.na(date)),
      n_sellers = n_distinct(maker[which(!is.na(date))]),
      .by = c("asset_id", "week.start", "week.end")
    ) |>
    mutate(
      listed = as.numeric(n_listed > 0)
    ) |>
    
    arrange(week.start, asset_id)
  
}


### Relistings database construction & summary

# Relistings Database
relisting_db_builder <- function (edit_time_length) {

  listing_updated_base <- listings |>
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
  
  listing_updated <- listing_updated_base |>
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
      listing_updated_base,
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
  
  listing_owner_in_lands <- listings |>
    select(asset_id, date, list_end, maker) |>
    arrange(date) |>
    left_join(
      transfers |>
        select(
          sale_asset_id = asset_id,
          transfer_date = date,
          is_sale,
          sender,
          recipient,
          sale_price_usd = price_usd,
          sale_price_raw = price_raw,
          sale_currency = currency
        ) |>
        arrange(transfer_date),
      by = join_by(list_end >= transfer_date, maker == recipient),
      relationship = "many-to-many"
    ) |>
    mutate(
      recipient = maker
    ) |>
    select(-list_end)
  
  listing_owner_out_lands <- listings |>
    select(asset_id, date, list_end, maker) |>
    arrange(date) |>
    left_join(
      transfers |>
        select(
          sale_asset_id = asset_id,
          transfer_date = date,
          is_sale,
          sender,
          recipient,
          sale_price_usd = price_usd,
          sale_price_raw = price_raw,
          sale_currency = currency
        ) |>
        arrange(transfer_date),
      by = join_by(list_end >= transfer_date, maker == sender),
      relationship = "many-to-many"
    ) |>
    mutate(
      sender = maker
    ) |>
    select(-list_end)
  
  listing_owner_transfers_base <- bind_rows(listing_owner_in_lands, listing_owner_out_lands)
  
  listing_owner_transfers <- listing_owner_transfers_base |>
    group_by(asset_id, date) |>
    summarise(
      maker = first(maker),
      received = sum(maker == recipient, na.rm = T),
      sent = sum(maker == sender, na.rm = T),
      owned = sum(maker == recipient, na.rm = T) - sum(maker == sender, na.rm = T),
      # received = sum(maker == recipient & sender != EstateContract, na.rm = T),
      # sent = sum(maker == sender & recipient != EstateContract, na.rm = T),
      # owned = sum(maker == recipient & sender != EstateContract, na.rm = T) - sum(maker == sender & recipient != EstateContract, na.rm = T),
      sale_price_usd = coalesce(last(sale_price_usd[which(sale_asset_id == asset_id & maker == recipient & is_sale)]), last(sale_price_usd[which(sale_asset_id == asset_id & maker == recipient)])),
      sale_price_raw = coalesce(last(sale_price_raw[which(sale_asset_id == asset_id & maker == recipient & is_sale)]), last(sale_price_raw[which(sale_asset_id == asset_id & maker == recipient)])),
      sale_currency = coalesce(last(sale_currency[which(sale_asset_id == asset_id & maker == recipient & is_sale)]), last(sale_currency[which(sale_asset_id == asset_id & maker == recipient)])),
      .groups = "drop"
    )
  
  listing_updated <- listing_updated |>
    left_join(
      listing_owner_transfers |> 
        mutate(
          acq_mode = if_else(sale_price_usd > 0, "paid", "free")
        ) |>
        select(
          asset_id, 
          date, 
          maker_owned_parcels = owned, 
          acq_mode,
          acq_price_usd = sale_price_usd, 
          acq_price_raw = sale_price_raw,
          acq_price_ccy = sale_currency
        ),
      by = c("asset_id", "date")
    )
  
  listing_updated = locations_merger(listing_updated)
  
  return(listing_updated)
}

relistings <- relisting_db_builder(30)

saveRDS(relistings, file = "relistings.RDS")












