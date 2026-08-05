
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

####################
## Summary
####################

# Volume summary builder

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


















