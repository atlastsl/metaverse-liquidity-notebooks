################################
## Regressions databases builder
################################

# Actual date
date_now <- lubridate::now()

# Filtered main (raw) database
# TxBase <- Transactions |> filter(lubridate::year(date) == 2020)

# Listings on Dcl marketplace V1
listings_base <- Transactions |>
  filter(asset_contract == LandContract & type == "order" & order_type == "list" & order_market == "dcl-marketplace-1") |>
  select(hash, order_id, asset_id, maker, date, order_time_on_market, order_status) |>
  transmute( 
    # Rearrange listing database, create listing period (interval [Date, Date+Tom)), and 30 days lookback interval
    asset_id,
    date,
    tom = order_time_on_market,
    start_at = date,
    end_at = as.POSIXct(ifelse(order_status == "pending", date_now, date + dmilliseconds(floor(order_time_on_market*86400*1000))), tz = "UTC"),
    status = order_status
  )
listings_base <- listings_base |>
  add_count(asset_id, date) |>
  filter(n == 1) |>
  bind_rows(
    listings_base |> add_count(asset_id, date) |> filter(n > 1, tom > 0)
  ) |>
  select(-n)

# Asks on Dcl marketplace V
asks_base <- Transactions |>
  filter(asset_contract == LandContract, type == "order", order_type == "ask", order_market == "dcl-marketplace-1") |>
  select(asset_id, date, taker) |>
  transmute(
    asset_id,
    ask_date = date,
    asker = taker
  )

# lookback_start = pmax(floor_date(date - ddays(30), unit = "day"), floor_date(min(Transactions$date, na.rm = T), unit = "day")),
# lookback_end = floor_date(date, unit = "day"),

# Join listings database with past listings overlap lookback period
listings_lookback_listed <- listings_base |>
  select(tx_hash, list_id, asset_id, lookback_start, lookback_end) |>
  left_join(
    listings_base |> select(asset_id, past_list_start = start_at, past_list_end = end_at),
    by = join_by(asset_id, lookback_end > past_list_start, lookback_start < past_list_end),
    relationship = "many-to-many"
  ) |>
  filter(lubridate::year(lookback_end) == 2020) |>
  filter(!is.na(past_list_start)) |>
  mutate(
    inl_start = pmax(past_list_start, lookback_start),
    inl_end = pmin(past_list_end, lookback_end)
  ) |>
  filter(inl_start < inl_end) |>
  arrange(asset_id, list_id, tx_hash) |>
  group_by(tx_hash, list_id, asset_id) |>
  mutate(
    prev_max_inl_end = dplyr::lag(cummax(as.numeric(inl_end))),
    new_block = if_else(is.na(prev_max_inl_end) | as.numeric(inl_start) > prev_max_inl_end + 5, 1L, 0L),
    block = cumsum(new_block)
  ) |>
  ungroup() |>
  group_by(asset_id, list_id, tx_hash, block) |>
  summarise(
    lookback_start = first(lookback_start),
    lookback_end = first(lookback_end),
    binl_start = min(inl_start),
    binl_end = max(inl_end),
    .groups = "drop"
  ) |>
  select(-block)

listings_lookback_nonlisted <- listings_lookback_listed |>
  arrange(asset_id, list_id, tx_hash) |>
  group_by(asset_id, list_id, tx_hash, lookback_start, lookback_end) |>
  reframe(
    non_start = c(first(lookback_start), binl_end),
    non_end = c(binl_start, first(lookback_end)),
  ) |>
  ungroup() |>
  filter(non_start < non_end)

listings_lookback_nonlisted_full <- listings_base |>
  select(tx_hash, list_id, asset_id, lookback_start, lookback_end) |>
  filter(lubridate::year(lookback_end) == 2020) |>
  anti_join(listings_lookback_listed |> distinct(tx_hash, list_id), by = c("tx_hash", "list_id")) |>
  transmute(
    tx_hash,
    list_id,
    asset_id,
    non_start = lookback_start,
    non_end = lookback_end
  )

listings_lookback_nonlisted <- bind_rows(listings_lookback_nonlisted, listings_lookback_nonlisted_full)
rm(list = c("listings_lookback_nonlisted_full"))

listings_lookback_listed_stats <- listings_lookback_listed |>
  left_join(
    asks_base,
    by = join_by(asset_id, binl_start <= ask_date, binl_end > ask_date),
    relationship = "many-to-many"
  ) |>
  summarise(
    asks_li_30d = sum(!is.na(ask_date)),
    askers_li_30d = n_distinct(asker[!is.na(ask_date)]),
    .by = c("tx_hash", "list_id")
  ) |>
  left_join(
    listings_lookback_listed |>
      summarise(
        ndays_li_30d = sum(time_length(binl_end - binl_start, unit = "second"), na.rm = T)/86400,
        .by = c("tx_hash", "list_id")
      ),
    by = c("tx_hash", "list_id")
  ) |>
  mutate(
    asks_li_m30d = asks_li_30d / ndays_li_30d,
    askers_li_m30d = askers_li_30d / ndays_li_30d,
    askers_li_a30d = if_else(askers_li_30d > 0, asks_li_30d / askers_li_30d, 0L)
  )
listings_lookback_nonlisted <- listings_lookback_nonlisted |>
  left_join(
    asks_base,
    by = join_by(asset_id, non_start <= ask_date, non_end > ask_date),
    relationship = "many-to-many"
  ) |>
  summarise(
    asks_ul_30d = sum(!is.na(ask_date)),
    askers_ul_30d = n_distinct(asker[!is.na(ask_date)]),
    ndays_ul_30d = sum(time_length(non_end - non_start, unit = "second")/86400),
    .by = c("tx_hash", "list_id")
  ) |>
  left_join(
    listings_lookback_listed |>
      summarise(
        ndays_ul_30d = sum(time_length(non_end - non_start, unit = "second"), na.rm = T)/86400,
        .by = c("tx_hash", "list_id")
      ),
    by = c("tx_hash", "list_id")
  ) |>
  mutate(
    asks_ul_m30d = asks_ul_30d / ndays_ul_30d,
    askers_ul_m30d = askers_ul_30d / ndays_ul_30d,
    askers_ul_a30d = if_else(askers_ul_30d > 0, asks_ul_30d / askers_ul_30d, 0L),
  )
listings_lookback_all <- listings_base |>
  select(tx_hash, list_id, asset_id, lookback_start, lookback_end) |>
  left_join(
    asks_base,
    by = join_by(asset_id, lookback_start <= ask_date, lookback_end > ask_date),
    relationship = "many-to-many"
  ) |>
  summarise(
    asks_ll_30d = sum(!is.na(ask_date)),
    askers_ll_30d = n_distinct(asker[!is.na(ask_date)]),
    .by = c("tx_hash", "list_id")
  ) |>
  mutate(
    asks_ll_m = asks_ll_30d / 30,
    askers_ll_m = askers_ll_30d / 30,
    askers_ll_a = if_else(askers_ll_30d > 0, asks_ll_30d / askers_ll_30d, 0L)
  )
listings_current_listed <- listings_base |>
  select(tx_hash, list_id, asset_id, start_at, end_at, tom) |>
  left_join(
    asks_base,
    by = join_by(asset_id, start_at <= ask_date, end_at > ask_date),
    relationship = "many-to-many"
  ) |>
  summarise(
    asks_cr = sum(!is.na(ask_date)),
    askers_cr = n_distinct(asker[!is.na(ask_date)]),
    .by = c("tx_hash", "list_id")
  ) |>
  mutate(
    asks_cr_m = asks_cr / tom,
    askers_cr_m = askers_cr / tom,
    askers_cr_a = if_else(askers_cr > 0, asks_cr / askers_cr, 0L)
  )




#################################################################################################

listings_base <- listings |>
  mutate(
    interval_start = pmax(floor_date(date - ddays(30), unit = "day"), floor_date(min(Transactions$date, na.rm = T), unit = "day")),
    interval_end = floor_date(date, unit = "day"),
  ) |>
  filter(lubridate::year(date) == 2020)
asks_base <- asks

interval_base <- lat_base
asks_base <- asks
listings_base <- listings

# Step 1: Construction of Listed Periods in intervals 
listed_periods <- interval_base |>
  select(asset_id, date, interval_start, interval_end) |>
  left_join(
    listings_base |> select(asset_id, past_list_start = list_start, past_list_end = list_end),
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
    asks_base,
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
    asks_base,
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
    asks_base,
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
      asks_base,
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
        asks_li_30d, askers_li_30d, ndays_li_30d, asks_li_m30d, askers_li_m30d, askers_li_a30d,
        asks_ul_30d, askers_ul_30d, ndays_ul_30d, asks_ul_m30d, askers_ul_m30d, askers_ul_a30d,
        asks_ll_30d, askers_ll_30d, ndays_ll_30d, asks_ll_m30d, askers_ll_m30d, askers_ll_a30d
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
database <- database |>
  left_join(
    Locations |> 
      # select( # Take only some columns in locations database
      #   TOKEN_ID, TYPE, DIST_PLAZA_central, DIST_NRS_PLAZA, NAME_NRS_PLAZA, DIST_ROAD, 
      #   DIST_NRS_DISTRICT_CAT, NAME_NRS_DISTRICT_CAT, DIST_PLAZA_north, DIST_PLAZA_south,
      #   DIST_PLAZA_east, DIST_PLAZA_west, `DIST_PLAZA_north-east`, `DIST_PLAZA_south-east`,
      #   `DIST_PLAZA_north-west`, `DIST_PLAZA_south-west`
      # ) |>
      mutate(
        IDX = as.numeric(TYPE == "district") # Infer is parcel is in a district or not
      ) |>
      # rename(
      #   asset_id = TOKEN_ID, # Parcel_id
      #   IDX = IN_DISTRICT, # Parcel is in district ?
      #   DPC = DIST_PLAZA_central, # Distance to central plaza
      #   DPX = DIST_NRS_PLAZA, # Distance to neareat peripheral plaza
      #   NPX = NAME_NRS_PLAZA, # Name of nearest peripheral plaza
      #   DRD = DIST_ROAD, # Distance to road
      #   DDX = DIST_NRS_DISTRICT_CAT, # Distance to nearest district
      #   NDX = NAME_NRS_DISTRICT_CAT, # Category of nearest ditrict,
      #   DPXn = DIST_PLAZA_north,
      #   DPXs = DIST_PLAZA_south,
      #   DPXe = DIST_PLAZA_east,
      #   DPXw = DIST_PLAZA_west,
      #   DPXne = DIST_PLAZA_north-east,
      #   DPXse = DIST_PLAZA_south-east,
      #   DPXnw = DIST_PLAZA_north-west,
      #   DPXsw = DIST_PLAZA_south-west
      # ) |>
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
      ), #|> select(-TYPE),
    by = "asset_id"
  )


#################################################################################################

asks_base <- asks
listings_base <- listings

asks_db_1 <- asks_base |>
  mutate(
    interval = floor_date(ask_date, unit = "quarter"),
    interval_start = interval,
    interval_end = ceiling_date(interval_start + dminutes(1), unit = "quarter"),
    quarter = lubridate::quarter(interval)
  ) |>
  summarise(
    asks = n(),
    askers = n_distinct(asker),
    quarter = first(quarter),
    interval_start = first(interval_start),
    interval_end = first(interval_end),
    .by = c("asset_id", "interval")
  ) |>
  left_join(
    listings_base |> select(asset_id, list_start, list_end),
    by = join_by(asset_id, interval_end > list_start, interval_start < list_end),
    relationship = "many-to-many"
  ) |>
  mutate(
    intl_start = pmax(if_else(is.na(list_start), NA, interval_start), list_start, na.rm = T),
    intl_end = pmin(if_else(is.na(list_end), NA, interval_end), list_end, na.rm = T),
    intl_diff = if_else(is.na(intl_start) | is.na(intl_end), 0L, as.numeric(time_length(intl_end - intl_start, unit = "second"))/86400)
  ) |>
  mutate(
    intl_diff = pmax(intl_diff, 0)
  ) |>
  summarise(
    asks = first(asks),
    askers = first(askers),
    quarter = first(quarter),
    interval_start = first(interval_start),
    interval_end = first(interval_end),
    list_days = sum(intl_diff),
    .by = c("asset_id", "interval")
  )
asks_db_1_2020 <- asks_db_1 |>
  filter(lubridate::year(interval) == 2020) |>
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
      ), #|> select(-TYPE),
    by = "asset_id"
  )

res = felp_model(
  asks_db_1_2020 |> 
    filter(quarter > 1) |>
    mutate(
      lasks = log(asks), 
      quarter = paste0("Q", quarter),
      was_listed = factor(as.numeric(list_days > 0), levels = c(0, 1), labels=c("No", "Yes")),
    ),
  Y = c("askers"),
  X = c("DPC", "DPX", "DRD", "DDX", "was_listed"),
  fe = c("quarter"),
  cl = c("quarter")
)
res

res = lm_model(
  asks_db_1_2020 |> 
    filter(quarter == 2) |>
    mutate(
      lasks = log(asks), 
      lldays = log1p(list_days),
      quarter = paste0("Q", quarter),
      was_listed = factor(as.numeric(list_days > 0), levels = c(0, 1), labels=c("No", "Yes")),
    ),
  Y = c("lasks"),
  X = c("DPC", "DPX", "DRD", "DDX", "lldays")
)
summary(res$model)

filter_tx = as.numeric(Transactions$date) < as.numeric(as.POSIXct("2021-01-01", tz="UTC")) &
  as.numeric(Transactions$date) >= as.numeric(as.POSIXct("2020-01-01", tz="UTC")) &
  Transactions$asset_contract == LandContract
uq_parcels_2020 = unique(Transactions$asset_id[filter_tx])
rm(list = c("filter_tx"))
#private_parcels = unique(Locations$TOKEN_ID[!is.na(Locations$TYPE)])
#uq_parcels_2020 = uq_parcels_2020[which(uq_parcels_2020 %in% private_parcels)]

asks_db_2 <- 
  data.table::CJ(
    asset_id = uq_parcels_2020, 
    interval = seq( 
      from = lubridate::floor_date(as.POSIXct("2020-01-01", tz = "UTC"), unit = "quarter"),
      to = lubridate::floor_date(as.POSIXct("2020-12-31", tz = "UTC"), unit = "quarter"),
      by = "quarter"
    )
  ) |>
  mutate(
    interval_start = interval,
    interval_end = ceiling_date(interval_start + dminutes(1), unit = "quarter"),
    quarter = lubridate::quarter(interval)
  ) |>
  left_join(
    asks_base,
    by = join_by(asset_id, interval_start <= ask_date, interval_end > ask_date)
  ) |>
  summarise(
    asks = sum(!is.na(ask_date)),
    askers = n_distinct(asker[!is.na(ask_date)]),
    quarter = first(quarter),
    interval_start = first(interval_start),
    interval_end = first(interval_end),
    .by = c("asset_id", "interval")
  ) |>
  left_join(
    listings_base |> select(asset_id, list_start, list_end),
    by = join_by(asset_id, interval_end > list_start, interval_start < list_end),
    relationship = "many-to-many"
  ) |>
  mutate(
    intl_start = pmax(if_else(is.na(list_start), NA, interval_start), list_start, na.rm = T),
    intl_end = pmin(if_else(is.na(list_end), NA, interval_end), list_end, na.rm = T),
    intl_diff = if_else(is.na(intl_start) | is.na(intl_end), 0L, as.numeric(time_length(intl_end - intl_start, unit = "second"))/86400)
  ) |>
  mutate(
    intl_diff = pmax(intl_diff, 0)
  ) |>
  summarise(
    asks = first(asks),
    askers = first(askers),
    quarter = first(quarter),
    interval_start = first(interval_start),
    interval_end = first(interval_end),
    list_days = sum(intl_diff),
    .by = c("asset_id", "interval")
  ) |>
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
      ), #|> select(-TYPE),
    by = "asset_id"
  )
  
res = felogit_model(
  asks_db_2 |> 
    #filter(quarter > 1) |>
    mutate(
      asks_dummy = as.numeric(asks > 0),
      quarter = paste0("Q", quarter),
      was_listed = factor(as.numeric(list_days > 0), levels = c(0, 1), labels=c("No", "Yes")),
    ),
  Y = c("asks_dummy"),
  X = c("DPC", "DPX", "DRD", "DDX", "was_listed"),
  fe = c("quarter"),
  cl = c("quarter")
)
res

res = logit_model(
  asks_db_2 |> 
    filter(quarter == 4) |>
    mutate(
      asks_dummy = as.numeric(asks > 0),
      quarter = paste0("Q", quarter),
      was_listed = factor(as.numeric(list_days > 0), levels = c(0, 1), labels=c("No", "Yes")),
    ),
  Y = c("asks_dummy"),
  X = c("DPC", "DPX", "DRD", "DDX", "was_listed")
)
summary(res)
PseudoR2(res, which = "McFadden")

#################################################################################################

source("./models.R")

res = lm_model(
  database |> 
    filter(status == "filled") |> mutate(lvalue = log1p(value)) |>
    filter(tom < quantile(tom, 0.75)+1.5*IQR(tom), tom > quantile(tom, 0.25)-1.5*IQR(tom)),
  Y = c("tom"),
  X = c("DPC", "DPX", "DRD", "DDX")
)
summary(res$model)

res = lm_model(
  database |> 
    filter(lubridate::month(date) > 1) |>
    filter(status == "filled", asks_li_m30d > 0) |> 
    filter(asks_li_m30d < quantile(asks_li_m30d, 0.75)+1.5*IQR(asks_li_m30d), asks_li_m30d > quantile(asks_li_m30d, 0.25)-1.5*IQR(asks_li_m30d)),
  Y = c("asks_li_m30d"),
  X = c("DPC", "DPX", "DRD", "DDX")
)
summary(res$model)

res = lm_model(
  database |> 
    filter(lubridate::month(date) > 1) |>
    filter(status == "filled", asks_cl > 0) |> 
    filter(tom < quantile(tom, 0.75)+1.5*IQR(tom), tom > quantile(tom, 0.25)-1.5*IQR(tom)) |>
    filter(asks_cl_m < quantile(asks_cl_m, 0.75)+1.5*IQR(asks_cl_m), asks_cl_m > quantile(asks_cl_m, 0.25)-1.5*IQR(asks_cl_m)),
  Y = c("tom"),
  X = c("asks_cl_m")
)
summary(res$model)
