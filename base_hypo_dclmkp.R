
setwd("~/Aurelien/Recherche/C1/Explore")
rm(list = ls())

####################
## Data loading
####################

library(readxl)
TransactionsRaw <- read_excel("../Donnees/OnChainDataAll4.xlsx")

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

LandContract <- "0xf87e31492faf9a91b02ee0deaad50d51d56d5d4d"
EstateContract <- "0x959e104e1a4db6317fa58f8295f586e1a978c297"

################################
## Database summary builder
################################

quarters_list <- unique(Transactions$quarter)
quarters_list <- quarters_list[!is.na(quarters_list) & quarters_list != quarters_list[length(quarters_list)]]

TxSummary_base <- Transactions |>
  filter(!is.na(quarter) & quarter != unique(Transactions$quarter)[length(unique(Transactions$quarter))]) |>
  filter(asset_contract == LandContract) |>
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

################################


################################
## Raw databases builder
################################

# Actual date
date_now <- lubridate::now()

# Listings on Dcl marketplace V1
listings <- Transactions |>
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
  distinct(asset_id, date, .keep_all = T)

# Joined listings (based on list_end and next list_start)
cplistings <- listings |>
  arrange(asset_id, date) |>
  group_by(asset_id) |>
  mutate(
    prev_list_end = dplyr::lag(cummax(as.numeric(list_end))),
    prev_status = dplyr::lag(status),
    prev_canceled_by = dplyr::lag(canceled_by),
    new_block = if_else(is.na(prev_list_end) | as.numeric(list_start) > prev_list_end + 5, 1L, if_else(prev_status == "canceled" & prev_canceled_by != "transfer", 0L, 1L)),
    block = cumsum(new_block)
  ) |>
  ungroup() |>
  group_by(asset_id, block) |>
  summarise(
    maker = first(maker),
    tom = sum(tom),
    date = first(date),
    list_start = first(list_start),
    list_end = last(list_end),
    status = last(status),
    relistings = n()-1,
    price_raw_f = first(price_raw),
    price_raw_l = last(price_raw),
    price_usd_f = first(price_usd),
    price_usd_l = last(price_usd),
    canceled_by = last(canceled_by),
    .groups = "drop"
  ) |>
  mutate(
    price_usd_delta = price_usd_l - price_raw_f*(price_usd_l/price_raw_l),
    tomc = time_length(list_end - list_start, unit = "second")/86400
  ) |>
  select(-block) |>
  arrange(date)

# Asks on Dcl marketplace V1
asks <- Transactions |>
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
    listings |> select(asset_id, list_start, list_end, list_price_raw = price_raw, list_price_usd = price_usd),
    by = join_by(asset_id, ask_date >= list_start, ask_date < list_end)
  ) |>
  select(-list_end) |>
  rename(list_date = list_start)

# Joined Asks (based on ask_end and next ask_start)
cpasks <- asks |>
  arrange(asset_id, asker, ask_date) |>
  group_by(asset_id, asker) |>
  mutate(
    prev_ask_end = dplyr::lag(cummax(as.numeric(ask_end))),
    new_block = if_else(is.na(prev_ask_end) | as.numeric(ask_start) > prev_ask_end + 5, 1L, 0L),
    block = cumsum(new_block)
  ) |>
  ungroup() |>
  group_by(asset_id, asker, block) |>
  summarise(
    tom = sum(tom),
    ask_date = first(ask_date),
    ask_start = first(ask_start),
    ask_end = last(ask_end),
    status = last(status),
    retries = n()-1,
    price_raw_f = first(price_raw),
    price_raw_l = last(price_raw),
    price_usd_f = first(price_usd),
    price_usd_l = last(price_usd),
    canceled_by = last(canceled_by),
    .groups = "drop"
  ) |>
  mutate(
    price_usd_delta = price_usd_l - price_raw_f*(price_usd_l/price_raw_l),
    tomc = time_length(ask_end - ask_start, unit = "second")/86400
  ) |>
  select(-block) |>
  arrange(ask_date)

# Listings - Asks Spreads on Dcl marketplace V1
spreads <- Transactions |>
  filter(asset_contract == LandContract, type == "order", order_type == "ask", order_market == "dcl-marketplace-1") |>
  select(asset_id, date, taker, amount, amount_usd, order_time_on_market, order_status, order_canceled_by) |>
  transmute(
    asset_id,
    ask_date = date,
    asker = taker,
    ask_price = amount,
    ask_price_usd = amount_usd,
    tom = order_time_on_market,
    status = order_status,
    canceled_by = order_canceled_by
  ) |>
  distinct(asset_id, ask_date, .keep_all = T) |>
  left_join(
    listings |> select(asset_id, list_start, list_end, list_price = price_raw),
    by = join_by(asset_id, ask_date >= list_start, ask_date < list_end)
  ) |>
  group_by(asset_id, ask_date) |>
  filter(n() == 1) |>
  ungroup() |>
  filter(!is.na(list_start)) |>
  # group_by(asset_id, list_start, taker) |>
  # mutate(
  #   nnn = n(),
  # ) |>
  # summarise(
  #   list_end = first(list_end),
  #   list_price = first(list_price),
  #   ask_price = max(ask_price),
  #   ask_price_usd = ask_price_usd[which.max(ask_price)],
  #   .groups = "drop"
  # ) |>
  mutate(
    spread = list_price - ask_price,
    spread_ss = spread * (ask_price_usd / ask_price),
    spread_rt = (list_price / ask_price),
    ctom = time_length(ask_date - list_start, unit = "second")/86400
  )
spreads <- locations_merger(spreads)
saveRDS(spreads, file = "spreads_db.RDS")

# 

################################


################################
## Attention databases builder
################################

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

database_builder <- function (interval_base, listings_base, asks_base) {
  
  # Step 1: Construction of Listed Periods in intervals 
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
  
  # Step 2: Construction of non listed periods in interval
  nonlisted_periods_from_listed <- listed_periods |>
    arrange(asset_id, date) |>
    group_by(asset_id, date, interval_start, interval_end) |>
    reframe(
      bnonl_start = c(first(interval_start), bintl_end),
      bnonl_end = c(bintl_start, first(interval_end)),
    ) |>
    ungroup() |>
    filter(bnonl_start < bnonl_end)
  
  # Step 3: Completion of non listed periods in interval. if parcel was non listed at least once in interval, non listed period is whole interval period
  nonlisted_periods_unlisted <- interval_base |>
    select(asset_id, date, interval_start, interval_end) |>
    anti_join(listed_periods |> distinct(asset_id, date), by = c("asset_id", "date")) |>
    transmute(
      asset_id,
      date,
      bnonl_start = interval_start,
      bnonl_end = interval_end
    )
  
  # Step 4: Union of non listed interval
  nonlisted_periods <- bind_rows(nonlisted_periods_from_listed, nonlisted_periods_unlisted)
  
  # Step 5: Statistic of listed periods
  listed_periods_stats <- listed_periods |>
    left_join(
      asks_base |> select(asset_id, ask_date, asker),
      by = join_by(asset_id, bintl_start <= ask_date, bintl_end > ask_date),
      relationship = "many-to-many"
    ) |>
    summarise(
      asks_li_30d = sum(!is.na(ask_date)),
      askers_li_30d = n_distinct(asker[!is.na(ask_date)]),
      .by = c("asset_id", "date")
    ) |>
    left_join(
      listed_periods |>
        summarise(
          ndays_li_30d = sum(time_length(bintl_end - bintl_start, unit = "second"), na.rm = T)/86400,
          mvalue_li_30d = sum(bintl_mvalue * bintl_n, na.rm = T) / sum(bintl_n),
          .by = c("asset_id", "date")
        ),
      by = c("asset_id", "date")
    ) |>
    mutate(
      asks_li_m30d = asks_li_30d / ndays_li_30d,
      askers_li_m30d = askers_li_30d / ndays_li_30d,
      askers_li_a30d = if_else(askers_li_30d > 0, asks_li_30d / askers_li_30d, 0L)
    )
  
  # Step 6: Statistics of non listed periods
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
    ) |>
    left_join(
      nonlisted_periods |>
        summarise(
          ndays_ul_30d = sum(time_length(bnonl_end - bnonl_start, unit = "second"), na.rm = T)/86400,
          .by = c("asset_id", "date")
        ),
      by = c("asset_id", "date")
    ) |>
    mutate(
      asks_ul_m30d = asks_ul_30d / ndays_ul_30d,
      askers_ul_m30d = askers_ul_30d / ndays_ul_30d,
      askers_ul_a30d = if_else(askers_ul_30d > 0, asks_ul_30d / askers_ul_30d, 0L),
    )
  
  # Step 7: Statistics for whole interval
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
      listings_base |> select(asset_id, past_date = date, value = price_usd),
      by = join_by(asset_id, date > past_date)
    ) |>
    arrange(asset_id, date, past_date) |>
    summarise(
      asks_ll_30d = first(asks_ll_30d),
      askers_ll_30d = first(askers_ll_30d),
      past_value = last(value, na_rm = T),
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
    ) |>
    mutate(
      asks_ll_m30d = asks_ll_30d / ndays_ll_30d,
      askers_ll_m30d = askers_ll_30d / ndays_ll_30d,
      askers_ll_a30d = if_else(askers_ll_30d > 0, asks_ll_30d / askers_ll_30d, 0L)
    )
  
  # Step 8: Statistics for current listing period
  current_listing_period_stats <- NULL
  if (all(c("list_start", "list_end") %in% colnames(interval_base))) {
    current_listing_period_stats <- interval_base |>
      select(asset_id, date, list_start, list_end) |>
      left_join(
        asks_base |> select(asset_id, ask_date, asker),
        by = join_by(asset_id, list_start <= ask_date, list_end > ask_date),
        relationship = "many-to-many"
      ) |>
      summarise(
        asks_cl = sum(!is.na(ask_date)),
        askers_cl = n_distinct(asker[!is.na(ask_date)]),
        .by = c("asset_id", "date")
      ) |>
      left_join(
        interval_base |>
          select(asset_id, date, list_start, list_end) |>
          summarise(
            ndays_cl = sum(time_length(list_end - list_start, unit = "second"), na.rm = T)/86400,
            .by = c("asset_id", "date")
          ),
        by = c("asset_id", "date")
      ) |>
      mutate(
        asks_cl_m = asks_cl / ndays_cl,
        askers_cl_m = askers_cl / ndays_cl,
        askers_cl_a = if_else(askers_cl > 0, asks_cl / askers_cl, 0L)
      )
  }
  
  # Step 9: Database finalization
  database <- interval_base |>
    left_join(listed_periods_stats, by = c("asset_id", "date")) |>
    left_join(nonlisted_periods_stats, by = c("asset_id", "date")) |>
    left_join(whole_interval_stats, by = c("asset_id", "date")) |>
    mutate(
      across(
        c(
          asks_li_30d, askers_li_30d, ndays_li_30d, asks_li_m30d, askers_li_m30d, askers_li_a30d, mvalue_li_30d,
          asks_ul_30d, askers_ul_30d, ndays_ul_30d, asks_ul_m30d, askers_ul_m30d, askers_ul_a30d,
          asks_ll_30d, askers_ll_30d, ndays_ll_30d, asks_ll_m30d, askers_ll_m30d, askers_ll_a30d, past_value
        ),
        ~ replace_na(.x, 0L)
      )
    )
  if (!is.null(current_listing_period_stats)) {
    database <- database |>
      left_join(current_listing_period_stats, by = c("asset_id", "date")) |>
      mutate(
        across(
          c(
            asks_cl, askers_cl, ndays_cl, asks_cl_m, askers_cl_m, askers_cl_a
          ),
          ~ replace_na(.x, 0L)
        )
      )
  }
  
  # Step 10: Join with location data
  database <- locations_merger(database)
  
  return(database)
}

gl_att_builder <- function () {
  db <- data.table::CJ(
      date = seq(
        from = lubridate::floor_date(as.POSIXct("2019-01-01", tz="UTC"), unit = "day"),
        to = lubridate::floor_date(as.POSIXct("2024-12-31", tz="UTC"), unit = "day"),
        by = "day"
      )
    ) |>
      mutate(
        interval_start = floor_date(date - ddays(30), unit = "day"),
        interval_end = date
      ) |> 
      left_join(
        Transactions |>
          filter(asset_contract == LandContract, lubridate::year(date) %in% 2018:2024) |>
          filter(type == "transfer" & is_sale == T & sale_related_market == "dcl-marketplace-1") |>
          select(sale_date = date, amount_usd),
        by = join_by(interval_start <= sale_date, interval_end > sale_date)
      ) |>
      summarise(
        vol_med = median(amount_usd),
        vol_tot = sum(amount_usd),
        vol_N = sum(!is.na(amount_usd)),
        .by = "date"
      ) |>
      mutate(
        vol_med = replace_na(vol_med, 0L),
        vol_tot = replace_na(vol_tot, 0L),
        vol_N = replace_na(vol_N, 0L)
      )
  return(db)
}

gt_att_builder <- function () {
  r1_gt <- ts_gtrends("decentraland", geo = "US", time = "2018-12-01 2021-01-01")
  r2_gt <- ts_gtrends("decentraland", geo = "US", time = "2020-12-01 2022-01-01")
  r3_gt <- ts_gtrends("decentraland", geo = "US", time = "2021-12-01 2023-01-01")
  r4_gt <- ts_gtrends("decentraland", geo = "US", time = "2022-12-01 2025-01-01")
  
  r1_gt <- r1_gt |> transmute(week = time, value) |> mutate(lvalue = dplyr::lag(value)) |>
    reframe(date = seq(from = week, by = "day", length.out = 7), lvalue = lvalue, .by = c(week)) |>
    filter(lubridate::year(date) %in% 2019:2020)
  
  r2_gt <- r2_gt |> transmute(week = time, value) |> mutate(lvalue = dplyr::lag(value)) |>
    reframe(date = seq(from = week, by = "day", length.out = 7), lvalue = lvalue, .by = c(week)) |>
    filter(lubridate::year(date) %in% 2021)
  
  r3_gt <- r3_gt |> transmute(week = time, value) |> mutate(lvalue = dplyr::lag(value)) |>
    reframe(date = seq(from = week, by = "day", length.out = 7), lvalue = lvalue, .by = c(week)) |>
    filter(lubridate::year(date) %in% 2022)
  
  r4_gt <- r4_gt |> transmute(week = time, value) |> mutate(lvalue = dplyr::lag(value)) |>
    reframe(date = seq(from = week, by = "day", length.out = 7), lvalue = lvalue, .by = c(week)) |>
    filter(lubridate::year(date) %in% 2023:2024)
  
  tab <- as_tibble(as.data.frame(do.call(rbind, list(r1_gt, r2_gt, r3_gt, r4_gt))))
  colnames(tab) <- c("week", "date", "lagged.wtrend")
  return(tab |> select(-week))
}

install.packages("remotes")
remotes::install_github("trendecon/trendecon")

################################



#########################################################################################
##### 1. LOCATION AND ATTENTION
#########################################################################################

library(conflicted)
conflicted::conflicts_prefer(dplyr::select)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# filter_parcels = 
#   as.numeric(Transactions$date) < as.numeric(as.POSIXct("2021-01-01", tz="UTC")) &
#   as.numeric(Transactions$date) >= as.numeric(as.POSIXct("2019-01-01", tz="UTC")) &
#   Transactions$asset_contract == LandContract
filter_parcels = 
  as.numeric(asks$ask_date) < as.numeric(as.POSIXct("2021-01-01", tz="UTC")) &
  as.numeric(asks$ask_date) >= as.numeric(as.POSIXct("2019-01-01", tz="UTC"))
parcels <- unique(asks$asset_id[filter_parcels])
parcels <- parcels[parcels %in% Locations$TOKEN_ID]
rm(list = c("filter_parcels"))
lat_base <- data.table::CJ(
  asset_id = parcels[!is.na(parcels)], 
  date = seq( 
    from = lubridate::floor_date(as.POSIXct("2019-01-01", tz="UTC"), unit = "quarter"),
    to = lubridate::floor_date(as.POSIXct("2020-12-31", tz="UTC"), unit = "quarter"),
    by = "quarter"
  )
)
  
lat_db <- database_builder(interval_base = lat_base, listings_base = listings, asks_base = asks)

saveRDS(lat_db, file = "lat_db_2019_2020_v2.RDS")

llq_db <- listings |>
  filter(lubridate::year(date) %in% c(2019, 2020)) |>
  left_join(
    Locations |> 
      mutate(
        IDX = as.numeric(TYPE == "district") # Infer is parcel is in a district or not
      ) |>
      select(
        asset_id = TOKEN_ID,
        IDX,
        DPC      = DIST_PLAZA_central,
        DPX      = DIST_NRS_PLAZA,
        NPX      = NAME_NRS_PLAZA,
        DRD      = DIST_ROAD,
        DDX      = DIST_NRS_DISTRICT_CAT,
        NDX      = NAME_NRS_DISTRICT_CAT,
        DPXn     = DIST_PLAZA_north,
        DPXs     = DIST_PLAZA_south,
        DPXe     = DIST_PLAZA_east,
        DPXw     = DIST_PLAZA_west,
        DPXne    = `DIST_PLAZA_north-east`,
        DPXse    = `DIST_PLAZA_south-east`,
        DPXnw    = `DIST_PLAZA_north-west`,
        DPXsw    = `DIST_PLAZA_south-west`
      ) |>
      mutate(
        DPC = log1p(DPC),
        DPX = log1p(DPX),
        DRD = log1p(DRD),
        DDX = log1p(DDX),
        DPXn = log1p(DPXn),
        DPXs = log1p(DPXs),
        DPXe = log1p(DPXe),
        DPXw = log1p(DPXw),
        DPXne = log1p(DPXne),
        DPXse = log1p(DPXse),
        DPXnw = log1p(DPXnw),
        DPXsw = log1p(DPXsw),
      ),
    by = "asset_id"
  )

saveRDS(llq_db, file = "llq_db_2019_2020.RDS")


alq_base <- listings |>
  mutate(
    interval_start = pmax(floor_date(date - ddays(30), unit = "day"), floor_date(min(Transactions$date, na.rm = T), unit = "day")),
    interval_end = floor_date(date, unit = "day")
  ) |> filter(lubridate::year(date) %in% c(2019, 2020))

alq_db <- database_builder(interval_base = alq_base, listings_base = listings, asks_base = asks)

saveRDS(alq_db, file = "alq_db_2019_2020_v2.RDS")


alq_db_R2 <- listings |>
  mutate(
    interval_start = pmax(floor_date(date - ddays(30), unit = "day"), floor_date(min(Transactions$date, na.rm = T), unit = "day")),
    interval_end = floor_date(date, unit = "day"),
    value = price_usd
  ) |> 
  filter(lubridate::year(date) %in% c(2021))
alq_db_R2 <- database_builder(interval_base = alq_db_R2, listings_base = listings, asks_base = asks)
saveRDS(alq_db_R2, file = "alq_db_2021.RDS")


alq_db_R3 <- listings |>
  mutate(
    interval_start = pmax(floor_date(date - ddays(30), unit = "day"), floor_date(min(Transactions$date, na.rm = T), unit = "day")),
    interval_end = floor_date(date, unit = "day"),
    value = price_usd
  ) |> 
  filter(lubridate::year(date) %in% c(2022))
alq_db_R3 <- database_builder(interval_base = alq_db_R3, listings_base = listings, asks_base = asks)
saveRDS(alq_db_R3, file = "alq_db_2022.RDS")


alq_db_R4 <- listings |>
  mutate(
    interval_start = pmax(floor_date(date - ddays(30), unit = "day"), floor_date(min(Transactions$date, na.rm = T), unit = "day")),
    interval_end = floor_date(date, unit = "day"),
    value = price_usd
  ) |> 
  filter(lubridate::year(date) %in% c(2023, 2024))
alq_db_R4 <- database_builder(interval_base = alq_db_R4, listings_base = listings, asks_base = asks)
saveRDS(alq_db_R4, file = "alq_db_2023_2024.RDS")


glt_base <- gl_att_builder()
saveRDS(glt_base, "glt_db.RDS")

gtt_base <- gt_att_builder()
saveRDS(gtt_base, "gtt_db.RDS")


###################################################################
###################################################################
######### MEDIATION ANALYSIS ######################################
###################################################################

library(lavaan)

medanl_models_formats <- '
  # Mediator regression equation
  Attention ~ a1*DPC + a2*DPX + a3*DRD + a4*DDX + q2*Q219 + q3*Q319 + q4*Q419 + q5*Q120 + q6*Q220 + q7*Q320 + q8*Q420
  
  # Final regression equation
  Ltom ~ b*Attention + c1*DPC + c2*DPX + c3*DRD + c4*DDX + d*Lvalue + p2*Q219 + p3*Q319 + p4*Q419 + p5*Q120 + p6*Q220 + p7*Q320 + p8*Q420
  
  # Indirect Effects
  iDPC := a1*b
  iDPX := a2*b
  iDRD := a3*b
  iDDX := a4*b
  
  # Total effects
  tDPC := c1 + iDPC
  tDPX := c2 + iDPX
  tDRD := c3 + iDRD
  tDDX := c4 + iDDX
'

alq_db_med <- alq_db |>
  mutate(quarter = quarter(date), year = year(date)) |> 
  filter(status == "filled") |>
  mutate(
    Lvalue = log1p(value),
    Ltom = log(tom),
    Attention = factor(as.numeric(asks_ul_30d > 0), levels = c(0, 1), labels=c("No", "Yes")), 
    AttNorm = asks_cl_m,
    Q219 = as.numeric(quarter == 2 & year == 2019),
    Q319 = as.numeric(quarter == 3 & year == 2019),
    Q419 = as.numeric(quarter == 4 & year == 2019),
    Q120 = as.numeric(quarter == 1 & year == 2020),
    Q220 = as.numeric(quarter == 2 & year == 2020),
    Q320 = as.numeric(quarter == 3 & year == 2020),
    Q420 = as.numeric(quarter == 4 & year == 2020)
  ) |>
  filter(if_all(c("Ltom", "Lvalue"), ~ filter_outliers(.x)))

# 3. Estimer le modèle avec Bootstrap (ex: 1000 réplications pour les p-values)
# Note : Augmentez à 5000 pour votre version finale
fit_global <- lavaan::sem(
  model = medanl_models_formats, 
  data = alq_db_med,
  se = "bootstrap", 
  bootstrap = 1000,
  ordered = "Attention"
)

# 4. Afficher les résultats avec les intervalles de confiance bootstrap
summary(fit_global, standardized = TRUE, ci = TRUE, rsquare = TRUE)



###################################################################
###################################################################
######### SURVIVAL  ANALYSIS ######################################
###################################################################

library(survival)
library(DescTools)
library(dplyr)
library(tidyr)
library(lubridate)
library(MASS)

cox_fit <- survival::coxph(
  survival::Surv(tom, dstatus) ~ DPC + DPX + DRD + DDX + lvalue + survival::cluster(asset_id),
  data = alq_db_B21 |>
    mutate(
      dstatus = as.numeric(status == "filled" & !is.na(tom) & tom <= 365),
      tom = if_else(tom > 365, 365, tom),
      lvalue = log(Winsorize(value, val = quantile(value, probs = c(0.01, 0.99))))
    ) |>
    filter(tom > 0)
)
summary(cox_fit)

aft_fit <- survival::survreg(
  survival::Surv(tom, dstatus) ~ DPC + DPX + DRD + DDX + lvalue + survival::cluster(asset_id),
  data = alq_db_B21 |>
    mutate(
      dstatus = as.numeric(status == "filled" & !is.na(tom) & tom <= 365),
      tom = if_else(tom > 365, 365, tom),
      lvalue = log(Winsorize(value, val = quantile(value, probs = c(0.01, 0.99))))
    ) |>
    filter(tom > 0),
  dist = "loglogistic"
)
summary(aft_fit)

aft_fit <- survival::survreg(
  survival::Surv(tom, dstatus) ~ DPC + 
    CPX + CRD + CDX + North + LogPrice + Q2_2019 + Q3_2019 + 
    Q4_2019 + Q1_2020 + Q2_2020 + Q3_2020 + Q4_2020 + survival::cluster(asset_id),
  data = db |>
    mutate(
      dstatus = as.numeric(status == "filled" & !is.na(tom) & tom <= 365),
      tom = if_else(tom > 365, 365, tom),
      lvalue = log(Winsorize(value, val = quantile(value, probs = c(0.01, 0.99))))
    ),
  dist = "weibull"
)
summary(aft_fit)

poiss_fit <- glm(
  Attention ~ DPC + DPX + DRD + DDX,
  data = alq_db |>
    filter(ndays_li_30d == 0) |>
    mutate(
      Attention = asks_ul_30d
    ),
  family = poisson(link = "log")
)
summary(poiss_fit)

negbin_model <- glm.nb(
  Attention ~ DPC + DPX + DRD + DDX,
  data = alq_db |>
    filter(ndays_li_30d == 0) |>
    mutate(
      Attention = asks_ul_30d
    )
)
summary(negbin_model)

# Load the pscl package
library(pscl)

# Fit the hurdle model
# Stage 1 (Binary): count > 0 ~ x1 + x2
# Stage 2 (Count): count ~ z1 + z2
# dist = "negbin" applies the negative binomial distribution to the count part
hurdle_fit <- hurdle(
  Attention ~ DPC + DPX + DRD + DDX + PastValue | DPC + DPX + DRD + DDX + PastValue, 
  data = alq_db |>
    #filter(ndays_li_30d == 0) |>
    mutate(
      Attention = asks_ll_30d,
      Listed = factor(if_else(ndays_li_30d > 0, "YES", "NO"), levels = c("NO", "YES")),
      PastValue = log1p(past_value)
    ), 
  dist = "negbin"
)

# View the results
summary(hurdle_fit)

cox_fit_2 <- survival::coxph(
  survival::Surv(tom, dstatus) ~ DPC + DPX + DRD + DDX + Attention + lvalue + survival::cluster(asset_id),
  data = alq_db |>
    filter(ndays_li_30d == 0)|>
    filter(tom > 0) |>
    mutate(
      Attention = asks_ul_30d,
      dstatus = as.numeric(status == "filled" & !is.na(tom) & tom <= 365),
      tom = if_else(tom > 365, 365, tom),
      lvalue = log(Winsorize(value, val = quantile(value, probs = c(0.01, 0.99))))
    ) 
)
summary(cox_fit_2)




