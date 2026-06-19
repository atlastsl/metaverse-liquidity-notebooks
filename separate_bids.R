
# Get listing times of parcels
ltim <- ListingSales |>
  select(operation_id, type, asset_type, asset_id, creation_date, update_date, expiration_date, ref_id, ref_ttf) |>
  filter(type == "listing" & asset_type == "parcel") |>
  mutate(
    rtom = ifelse(is.na(ref_ttf), NA_real_, -as.numeric(ref_ttf)),
    lstart = ifelse(!is.na(creation_date), creation_date, update_date),
    lend = ifelse(!is.na(ref_id), lstart + dseconds(rtom * 86400), expiration_date)
  ) |>
  mutate(
    lstart = as.POSIXct(lstart, tz = "UTC"),
    lend = as.POSIXct(lend, tz = "UTC")
  ) |>
  select(operation_id, asset_id, rtom, lstart, lend) |>
  group_by(asset_id) |>
  arrange(lstart, .by_group = T) |>
  # mutate(
  #   lend_c = ifelse(!is.na(dplyr::lead(lstart)) & lend > dplyr::lead(lstart), dplyr::lead(lstart), lend),
  #   lend_u = ifelse(!is.na(dplyr::lead(lstart)) & lend > dplyr::lead(lstart), 1, 0),
  # ) |>
  # mutate(
  #   lend_c = as.POSIXct(lend_c, tz = "UTC")
  # ) |>
  mutate(
    lend = as.POSIXct(ifelse(!is.na(dplyr::lead(lstart)) & lend > dplyr::lead(lstart), dplyr::lead(lstart), lend), tz = "UTC")
  ) |>
  ungroup() |>
  select(operation_id, asset_id, lstart, lend, rtom)

# Determinate if at bid time, parcel was listed or not (filtered bids parcel listed)
fbpl <- FilteredBids |>
  select(asset_id, date, bidder) |>
  group_by(asset_id, date, bidder) |>
  summarise(
    tmp = n(), .groups = "keep"
  ) |>
  ungroup() |>
  select(asset_id, date, bidder) |>
  left_join(ltim |> select(asset_id, lstart, lend), by = "asset_id", multiple = "all", relationship = "many-to-many") |>
  mutate(
    was_listed = as.numeric(lstart <= date & date < lend)
  ) |>
  mutate(
    was_listed = replace_na(was_listed, 0L)
  ) |>
  group_by(asset_id, date, bidder) |>
  summarise(
    was_listed = sum(was_listed, na.rm = T),
    .groups = "keep"
  )

# Count number of bids on a parcel between listing time intervals (bids per listing intervals)
bpli <- ltim |>
  select(-operation_id) |>
  rename(
    tom = rtom
  ) |>
  left_join(
    FilteredBids |>
      select(asset_id, date, bidder) |>
      group_by(asset_id, date, bidder) |>
      summarise(
        tmp = n(), .groups = "keep"
      ) |>
      ungroup() |>
      select(asset_id, date, bidder) |>
      rename(
        bid_asset_id = asset_id,
        bid_date = date
      ),
    by = join_by(
      asset_id == bid_asset_id,
      lstart <= bid_date,
      lend > bid_date
    )
  ) |>
  group_by(asset_id, lstart, lend, bidder) |>
  summarise(
    tom = first(tom),
    n_bids = sum(!is.na(bid_date)),
    .groups = "keep"
  ) |>
  ungroup() |>
  group_by(asset_id, lstart, lend) |>
  summarise(
    tom = first(tom),
    n_bidders = sum(n_bids > 0),
    n_bids = sum(n_bids, na.rm = T),
    .groups = "keep"
  ) |>
  ungroup() |>
  mutate(
    ddays = time_length(interval(lstart, lend), unit = "seconds")/86400,
    nn_bidders = n_bidders / ddays,
    nn_bids = n_bids / ddays
  ) |>
  filter(ddays > 0 & nn_bidders > 0)


############################################################

source("./models.R")

fbpll <- fbpl |>
  filter(was_listed == 1) |>
  group_by(asset_id, bidder) |>
  summarise(
    n_bids = n(),
    .groups = "keep"
  ) |>
  ungroup() |>
  group_by(asset_id) |>
  summarise(
    n_bidders = n(),
    n_bids = sum(n_bids),
    .groups = "keep"
  ) |>
  ungroup() |>
  left_join( # Join with Locations database
    Locations |> 
      select( # Take only some columns in locations database
        TOKEN_ID, TYPE, DIST_PLAZA_central, DIST_NRS_PLAZA, NAME_NRS_PLAZA, DIST_ROAD, 
        DIST_NRS_DISTRICT_CAT, NAME_NRS_DISTRICT_CAT
      ) |> 
      mutate(
        IN_DISTRICT = as.numeric(TYPE == "district") # Infer is parcel is in a district or not
      ) |>
      rename(
        asset_id = TOKEN_ID, # Parcel_id
        IDX = IN_DISTRICT, # Parcel is in district ?
        DPC = DIST_PLAZA_central, # Distance to central plaza
        DPX = DIST_NRS_PLAZA, # Distance to neareat peripheral plaza
        NPX = NAME_NRS_PLAZA, # Name of nearest peripheral plaza
        DRD = DIST_ROAD, # Distance to road
        DDX = DIST_NRS_DISTRICT_CAT, # Distance to nearest district
        NDX = NAME_NRS_DISTRICT_CAT # Category of nearest ditrict
      ) |>
      select(-TYPE),
    by = "asset_id"
  )
  
res = lm_model(
  fbpll |> filter(n_bids >= (quantile(n_bids, 0.25) - 1.5*IQR(n_bids)) & n_bids <= (quantile(n_bids, 0.75) + 1.5*IQR(n_bids))),
  Y = c("n_bids"),
  X = c("DPC", "DPX", "NPX", "DRD", "DDX", "IDX")
)
summary(res$model)

res = lm_model(
  fbpll |> filter(n_bidders >= (quantile(n_bidders, 0.25) - 1.5*IQR(n_bidders)) & n_bidders <= (quantile(n_bidders, 0.75) + 1.5*IQR(n_bidders))),
  Y = c("n_bidders"),
  X = c("DPC", "DPX", "NPX", "DRD", "DDX", "IDX")
)
summary(res$model)
  

bplis <- bpli |>
  filter(!is.na(tom)) |>
  filter(
    tom >= (quantile(tom, 0.25) - 1.5*IQR(tom)) &
      tom <= (quantile(tom, 0.75) + 1.5*IQR(tom))
  ) |>
  filter(
    nn_bidders >= (quantile(nn_bidders, 0.25) - 1.5*IQR(nn_bidders)) &
      nn_bidders <= (quantile(nn_bidders, 0.75) + 1.5*IQR(nn_bidders))
  ) |>
  left_join( # Join with Locations database
    Locations |> 
      select( # Take only some columns in locations database
        TOKEN_ID, TYPE, DIST_PLAZA_central, DIST_NRS_PLAZA, NAME_NRS_PLAZA, DIST_ROAD, 
        DIST_NRS_DISTRICT_CAT, NAME_NRS_DISTRICT_CAT
      ) |> 
      mutate(
        IN_DISTRICT = as.numeric(TYPE == "district") # Infer is parcel is in a district or not
      ) |>
      rename(
        asset_id = TOKEN_ID, # Parcel_id
        IDX = IN_DISTRICT, # Parcel is in district ?
        DPC = DIST_PLAZA_central, # Distance to central plaza
        DPX = DIST_NRS_PLAZA, # Distance to neareat peripheral plaza
        NPX = NAME_NRS_PLAZA, # Name of nearest peripheral plaza
        DRD = DIST_ROAD, # Distance to road
        DDX = DIST_NRS_DISTRICT_CAT, # Distance to nearest district
        NDX = NAME_NRS_DISTRICT_CAT # Category of nearest ditrict
      ) |>
      select(-TYPE),
    by = "asset_id"
  )

res = lm_model(
  bplis,
  Y = c("tom"),
  X = c("DPC", "DPX", "DRD", "DDX", "NPX", "IDX")
)
summary(res$model)

res = lm_model(
  bplis,
  Y = c("tom"),
  X = c("nn_bidders")
)
summary(res$model)


res = lm_model(
  bplis,
  Y = c("tom"),
  X = c("nn_bidders", "DPC", "DPX", "DRD", "DDX", "NPX", "IDX")
)
summary(res$model)

  
  
