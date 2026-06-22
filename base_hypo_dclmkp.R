
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
## Regressions databases builder
################################

# Actual date
date_now <- lubridate::now()

# Listings on Dcl marketplace V1
listings <- Transactions |>
  filter(asset_contract == LandContract & type == "order" & order_type == "list" & order_market == "dcl-marketplace-1") |>
  select(hash, order_id, asset_id, maker, date, order_time_on_market, order_status, order_canceled_by, amount_usd) |>
  transmute( 
    # Rearrange listing database, create listing period (interval [Date, Date+Tom)), and 30 days lookback interval
    asset_id,
    date,
    tom = order_time_on_market,
    list_start = date,
    list_end = as.POSIXct(ifelse(order_status == "pending", date_now, date + dmilliseconds(floor(order_time_on_market*86400*1000))), tz = "UTC"),
    status = order_status,
    value = amount_usd,
    canceled_by = order_canceled_by
  )
listings <- listings |>
  add_count(asset_id, date) |>
  filter(n == 1) |>
  bind_rows(
    listings |> add_count(asset_id, date) |> filter(n > 1, tom > 0)
  ) |>
  select(-n)

# Asks on Dcl marketplace V
asks <- Transactions |>
  filter(asset_contract == LandContract, type == "order", order_type == "ask", order_market == "dcl-marketplace-1") |>
  select(asset_id, date, taker) |>
  transmute(
    asset_id,
    ask_date = date,
    asker = taker
  )

database_builder <- function (interval_base, listings_base, asks_base) {
  
  # Step 1: Construction of Listed Periods in intervals 
  listed_periods <- interval_base |>
    select(asset_id, date, interval_start, interval_end) |>
    left_join(
      listings_base |> select(asset_id, past_list_start = list_start, past_list_end = list_end, value),
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
      listings_base |> select(asset_id, past_date = date, value),
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
  database <- database |>
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
  
  return(database)
}


#########################################################################################
##### 1. LOCATION AND ATTENTION
#########################################################################################

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
) |> mutate(
  interval_start = date,
  interval_end = ceiling_date(date + dminutes(1), unit = "quarter")
)
  
lat_db <- database_builder(interval_base = lat_base, listings_base = listings, asks_base = asks)

saveRDS(lat_db, file = "lat_db_2019_2020_va.RDS")

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

saveRDS(alq_db, file = "alq_db_2019_2020_va.RDS")


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



