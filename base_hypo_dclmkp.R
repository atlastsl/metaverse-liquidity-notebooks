
setwd("~/Aurelien/Recherche/C1/Explore")
rm(list = ls())

####################
## Data loading
####################

TransactionsRaw <- read_excel("~/Aurelien/Recherche/C1/Donnees/OnChainDataAll3.xlsx")

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

#####################
## Descriptive statistics
#####################

str(TransactionsRaw)
str(Locations)

library(lubridate)
library(tidyr)
library(dplyr)

Transactions <- TransactionsRaw |>
  filter(!is.na(type)) |>
  mutate(quarter = floor_date(date, unit = "quarter")) |>
  relocate(date, .after = type) |>
  relocate(quarter, .after = type)

quarters_list <- unique(Transactions$quarter)
quarters_list <- quarters_list[!is.na(quarters_list) & quarters_list != quarters_list[length(quarters_list)]]

TxSummary_base <- Transactions |>
  filter(!is.na(quarter) & quarter != unique(Transactions$quarter)[length(unique(Transactions$quarter))]) |>
  filter(asset_contract == "0xf87e31492faf9a91b02ee0deaad50d51d56d5d4d") |>
  mutate(quarter = format(zoo::as.yearqtr(quarter)))

TxSummary <- as_tibble(data.frame(quarter = c(format(zoo::as.yearqtr(quarters_list)), "Total"))) |>
  left_join(
    TxSummary_base |>
      filter(type == "order" & order_type == "list" & order_market == "dcl-marketplace-1") |>
      group_by(quarter) |>
      summarise(
        lists_n = n(),
        lists_p = median(amount_usd),
        lists_v = sum(amount_usd),
        .groups = "keep"
      ) |>
      ungroup() |>
      bind_rows(
        TxSummary_base |>
          filter(type == "order" & order_type == "list" & order_market == "dcl-marketplace-1") |>
          summarise(
            quarter = "Total",
            lists_n = n(),
            lists_p = median(amount_usd),
            lists_v = sum(amount_usd)
          )
      ),
    by = "quarter"
  ) |>
  left_join(
    TxSummary_base |>
      filter(type == "order" & order_type == "list" & order_market == "dcl-marketplace-1", order_status == "filled") |>
      group_by(quarter) |>
      summarise(
        sales1_n = n(),
        sales1_p = median(amount_usd),
        sales1_t = median(order_time_on_market),
        sales1_v = sum(amount_usd)/1000,
        .groups = "keep"
      ) |>
      ungroup() |>
      bind_rows(
        TxSummary_base |>
          filter(type == "order" & order_type == "list" & order_market == "dcl-marketplace-1", order_status == "filled") |>
          summarise(
            quarter = "Total",
            sales1_n = n(),
            sales1_p = median(amount_usd),
            sales1_t = median(order_time_on_market),
            sales1_v = sum(amount_usd)/1000,
          )
      ),
    by = "quarter"
  ) |>
  left_join(
    TxSummary_base |>
      filter(type == "order" & order_type == "list" & order_market == "dcl-marketplace-2", order_status == "filled") |>
      group_by(quarter) |>
      summarise(
        sales2_n = n(),
        sales2_p = median(amount_usd),
        sales2_t = median(order_time_on_market),
        sales2_v = sum(amount_usd)/1000,
        .groups = "keep"
      ) |>
      ungroup() |>
      bind_rows(
        TxSummary_base |>
          filter(type == "order" & order_type == "list" & order_market == "dcl-marketplace-2", order_status == "filled") |>
          summarise(
            quarter = "Total",
            sales2_n = n(),
            sales2_p = median(amount_usd),
            sales2_t = median(order_time_on_market),
            sales2_v = sum(amount_usd)/1000,
          )
      ),
    by = "quarter"
  ) |>
  left_join(
    TxSummary_base |>
      filter(type == "transfer" & is_sale == T & sale_related_market == "third-party-marketplace") |>
      group_by(quarter) |>
      summarise(
        sales3_n = n(),
        sales3_p = median(amount_usd),
        sales3_v = sum(amount_usd)/1000,
        .groups = "keep"
      ) |>
      ungroup() |>
      bind_rows(
        TxSummary_base |>
          filter(type == "transfer" & is_sale == T & sale_related_market == "third-party-marketplace") |>
          summarise(
            quarter = "Total",
            sales3_n = n(),
            sales3_p = median(amount_usd),
            sales3_v = sum(amount_usd)/1000,
          )
      ),
    by = "quarter"
  ) |>
  left_join(
    TxSummary_base |>
      filter(type == "order" & order_type == "ask" & order_market == "dcl-marketplace-1") |>
      group_by(quarter, asset_id, taker) |>
      summarise(
        asks_n = n(),
        .groups = "keep"
      ) |>
      ungroup() |>
      group_by(quarter, asset_id) |>
      summarise(
        asks_n = sum(asks_n),
        asks_a = n(),
        .groups = "keep"
      ) |>
      ungroup() |>
      group_by(quarter) |>
      summarise(
        asks_a = sum(asks_a),
        asks_n = sum(asks_n),
        .groups = "keep"
      ) |>
      ungroup() |>
      bind_rows(
        TxSummary_base |>
          filter(type == "order" & order_type == "ask" & order_market == "dcl-marketplace-1") |>
          group_by(asset_id, taker) |>
          summarise(
            asks_n = n(),
            .groups = "keep"
          ) |>
          ungroup() |>
          group_by(asset_id) |>
          summarise(
            asks_n = sum(asks_n),
            asks_a = n(),
            .groups = "keep"
          ) |>
          ungroup() |>
          summarise(
            quarter = "Total",
            asks_a = sum(asks_a),
            asks_n = sum(asks_n)
          )
      ),
    by = "quarter"
  )
  # |>
  # mutate(
  #   lists_n = replace_na(lists_n, 0L),
  #   lists_p = replace_na(lists_p, 0L),
  #   sales1_n = replace_na(sales1_n, 0L),
  #   sales1_p = replace_na(sales1_p, 0L),
  #   sales1_t = replace_na(sales1_t, 0L),
  #   sales1_v = replace_na(sales1_v, 0L),
  #   sales2_n = replace_na(sales2_n, 0L),
  #   sales2_p = replace_na(sales2_p, 0L),
  #   sales2_t = replace_na(sales2_t, 0L),
  #   sales2_v = replace_na(sales2_v, 0L),
  #   sales3_n = replace_na(sales3_n, 0L),
  #   sales3_p = replace_na(sales3_p, 0L),
  #   sales3_v = replace_na(sales3_v, 0L),
  #   asks_a = replace_na(asks_a, 0L),
  #   asks_n = replace_na(asks_n, 0L)
  # )
  # ) |>
  # relocate(sales1_n, .after = lists_n) |>
  # relocate(sales2_n, .after = sales1_n) |>
  # relocate(sales3_n, .after = sales3_n) |>
  # relocate(sales1_t, .after = sales3_n) |>
  # relocate(sales2_t, .after = sales1_t) |>
  # relocate(asks_a, .after = sales2_t) |>
  # relocate(asks_n, .after = asks_a) |>
  # relocate(sales1_p, .after = asks_n) |>
  # relocate(sales2_p, .after = sales1_p) |>
  # relocate(sales3_p, .after = sales2_p) |>
  # relocate(sales1_p, .after = sales3_p) |>
  # relocate(sales2_p, .after = sales1_p) |>
  # relocate(sales3_p, .after = sales2_p)
