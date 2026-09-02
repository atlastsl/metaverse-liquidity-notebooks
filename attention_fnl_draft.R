library(DescTools)
library(MASS)
source("models.R")
library(ggplot2)

library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(modelsummary)

library(stringr)
library(tinytable)

library(conflicted)
conflicted::conflicts_prefer(dplyr::select)
conflict_prefer("filter", "dplyr")
  

x = listings |> filter(!is.na(regime), as.numeric(date) >= 1554076800) |>
  # select(asset_id, date, asks_ll_30d, nfilled_li_30d) |>
  # mutate(
  #   att = asks_ll_30d + nfilled_li_30d
  # ) |>
  mutate(
    # att_ll = if_else(asks_ll_30d == 0, "Z", if_else(asks_ll_30d == 1, "B", if_else(asks_ll_30d < 5, "A", "S"))),
    # att_ul = if_else(asks_ul_30d == 0, "Z", if_else(asks_ul_30d == 1, "B", if_else(asks_ul_30d < 5, "A", "S"))),
    # att_ul = if_else(asks_ul_30d == 0, "Z", if_else(asks_ll_30d <= 3, "L", "H")),
    # asks_ll_30d = asks_ll_30d + nfilled_li_30d,
    att_ll = if_else(asks_ll_30d == 0, "Z", if_else(asks_ll_30d <= 3, "L", "H"))
  ) |>
  mutate(
    # att_ll = factor(att_ll, levels = c("Z", "B", "A", "S"), ordered = T),
    # att_ul = factor(att_ul, levels = c("Z", "B", "A", "S"), ordered = T),
    # att_ul = factor(att_ul, levels = c("Z", "L", "H"), ordered = T),
    att_ll = factor(att_ll, levels = c("Z", "L", "H"), ordered = T),
    att_dmn = as.numeric(asks_ll_30d > 0),
    att_dmf = factor(if_else(asks_ll_30d > 0, "Y", "N"), levels = c("N", "Y")),
    mvalue_li_30d = log1p(Winsorize(mvalue_li_30d, val = quantile(mvalue_li_30d, probs = c(0, 0.99)))),
    lndays_li_30d = log1p(ndays_li_30d),
    nndays_li_30d = ndays_li_30d/30,
    listed = factor(if_else(ndays_li_30d > 0, "Y", "N"), levels = c("N", "Y"))
  ) |>
  arrange(date)
cor(x[, c("DPC", "ndays_li_30d", "mvalue_li_30d")])


ordinal_model_func <- function (reg) {
  dat = x |> filter(regime == reg) |>
  #dat = x |> filter(regime == reg, ndays_li_30d == 0) |>
  #dat = x |> filter(regime == reg, ndays_li_30d > 0) |>
    mutate(
      year = year(date),
      quarter = paste0("Q", quarter(date), "_", year)
    ) |>
    mutate(qname = quarter) |> 
    mutate(v = 1) |> 
    pivot_wider(names_from = qname, values_from = v, values_fill = 0)
  qlist <- unique(dat$quarter)
  #Xcols <- c("DPC", "CRD", "CPX", "CDX", "North", "listed", "nndays_li_30d", "DPC:nndays_li_30d", "mvalue_li_30d")
  Xcols <- c("DPC", "CRD", "CPX", "CDX", "North", "nndays_li_30d", "nndays_li_30d:DPC", "mvalue_li_30d")
  #Xcols <- c("DPC", "CRD", "CPX", "CDX", "North", "mvalue_li_30d")
  #Xcols <- c("DPC", "CRD", "CPX", "CDX", "North")
  #Xcols <- c("DPC", "CRD", "CPX", "CDX", "North", "ndays_li_30d")
  Xcols <- c(Xcols, qlist[-1])
  model <- NULL
  if(length(unique(dat[["att_ll"]])) > 2) {
    model <- ord_model(dataset = dat, Y = c("att_ll"), X = Xcols)
  } else {
    model <- logit_model(dataset = dat, Y = c("Att"), X = Xcols)
  }
  # model <- logit_model(dataset = dat, Y = c("att_dmn"), X = Xcols)
  # model <- hurdle_model(
  #   dataset = dat,
  #   Y = c("asks_ll_30d"),
  #   X = Xcols
  # )
  return(model)
}

ordinal_batch_func <- function () {
  regs <- c("Normal", "Boom", "Crash", "Desert")
  
  my_gof <- list(
    list("raw" = "qt.fe",     "clean" = "Quarters FE", fmt = 0),
    list("raw" = "nobs",     "clean" = "Num.Obs.",    "fmt" = 0),
    list("raw" = "mcfadden", "clean" = "R2 McFadden", "fmt" = 3),
    list("raw" = "r2.nagelkerke", "clean" = "R2 Nagel", "fmt" = 3),
    list("raw" = "r.squared", "clean" = "R2", "fmt" = 3),
    list("raw" = "r.squared.adj", "clean" = "R2 Adj", "fmt" = 3),
    list("raw" = "logLik",   "clean" = "Log Lik.",    "fmt" = 2),
    list("raw" = "rmse",     "clean" = "RMSE",        "fmt" = 2),
    list("raw" = "aic",      "clean" = "AIC",         "fmt" = 1),
    list("raw" = "bic",      "clean" = "BIC",         "fmt" = 1)
  )
  
  results <- vector(mode = "list", length = length(regs))
  for (k in seq_along(regs)) {
    fit = ordinal_model_func(regs[k])
    td = get_estimates(fit, ci_method = "wald") |>
      filter(!str_detect(term, "^Q\\d+_\\d+"))
    gf = get_gof(fit)
    gf[["qt.fe"]] = "Yes"
    gf[["mcfadden"]] = PseudoR2(fit)
    mod = list(tidy = td, glance = gf)
    class(mod) <- "modelsummary_list"
    results[[k]] = mod
  }
  
  names(results) <- regs
  tab <- modelsummary(
    results, 
    gof_map = my_gof,
    stars = TRUE, statistic = "p.value", output = "tinytable"
  ) |> style_tt(fontsize = 0.8)
  colnames(tab) <- c(" ", colnames(tab)[-1])
  
  tab
}

ordinal_batch_func()



liq_model_func <- function (reg, type = "aft", att = F) {
  dat = x |> filter(regime == reg) |>
  #dat = x |> filter(regime == reg, ndays_li_30d == 0) |>
    filter(!is.na(tom) & tom > 0) |>
    mutate(
      sold = as.numeric(status == "filled" & tom <= 365),
      tom = if_else(tom > 365, 365, tom),
      LogPrice = log(Winsorize(price_usd, val = quantile(price_usd, probs = c(0.01, 0.99))))
    ) |>
    mutate(
      year = year(date),
      quarter = paste0("Q", quarter(date), "_", year),
    ) |>
    mutate(qname = quarter) |> 
    mutate(v = 1) |> 
    pivot_wider(names_from = qname, values_from = v, values_fill = 0)
  
  qlist <- unique(dat$quarter)
  Xcols <- c("DPC", "CRD", "CPX", "CDX", "North", "LogPrice")
  if (att) {
    Xcols <- c(Xcols, c("att_ll", "att_ll:nndays_li_30d"))
    #Xcols <- c(Xcols, c("att_dmn", "att_dmn:nndays_li_30d"))
  }
  Xcols <- c(Xcols, qlist[-1])
  model <- NULL
  
  fit = NULL
  if(type == "aft") {
    fit = aft_model(
      dat,
      Y_time = "tom",
      Y_event = "sold",
      X = Xcols,
      error_cluster = "asset_id"
    )
  } else {
    fit = cox_model(
      dat,
      Y_time = "tom",
      Y_event = "sold",
      X = Xcols,
      error_cluster = "asset_id"
    )
  }
  return(fit)
}

liq_batch_func <- function (type = NULL) {
  regs <- c("Normal", "Boom", "Crash", "Desert")
  rnames <- c("Normal", "Normal<br>+Attention", 
              "Boom", "Boom<br>+Attention", 
              "Crash", "Crash<br>+Attention", 
              "Desert", "Desert<br>+Attention")
  
  my_gof <- list(
    list("raw" = "qt.fe",     "clean" = "Quarters FE", fmt = 0),
    list("raw" = "nobs",     "clean" = "Num.Obs.",    "fmt" = 0),
    list("raw" = "r2.nagelkerke", "clean" = "R2 Nagel", "fmt" = 3),
    list("raw" = "concordance",   "clean" = "Concordance",    "fmt" = 2),
    list("raw" = "concordance.std",   "clean" = "Concordance Std",    "fmt" = 3),
    list("raw" = "aic",      "clean" = "AIC",         "fmt" = 1),
    list("raw" = "bic",      "clean" = "BIC",         "fmt" = 1)
  )
  results <- vector(mode = "list", length = length(regs)*2)
  for (k in seq_along(regs)) {
    fit_raw = liq_model_func(regs[k], type, F)
    td_raw = get_estimates(fit_raw, ci_method = "wald") |>
      filter(!str_detect(term, "^Q\\d+_\\d+"))
    gf_raw = get_gof(fit_raw)
    gf_raw[["qt.fe"]] = "Yes"
    if(type == "cox"){
      gf_raw[["concordance"]] = fit_raw$concordance[6]
      gf_raw[["concordance.std"]] = fit_raw$concordance[7]
    }
    mod_raw = list(tidy = td_raw, glance = gf_raw)
    class(mod_raw) <- "modelsummary_list"
    
    fit_att = liq_model_func(regs[k], type, T)
    td_att = get_estimates(fit_att, ci_method = "wald") |>
      filter(!str_detect(term, "^Q\\d+_\\d+"))
    gf_att = get_gof(fit_att)
    gf_att[["qt.fe"]] = "Yes"
    if(type == "cox"){
      gf_att[["concordance"]] = fit_att$concordance[6]
      gf_att[["concordance.std"]] = fit_att$concordance[7]
    }
    mod_att = list(tidy = td_att, glance = gf_att)
    class(mod_att) <- "modelsummary_list"
    
    results[[2*k-1]] = mod_raw
    results[[2*k]] = mod_att
  }
  
  names(results) <- rnames
  tab <- modelsummary(
    results, 
    gof_map = my_gof,
    stars = TRUE, statistic = "p.value", output = "tinytable"
  ) |> style_tt(fontsize = 0.8)
  colnames(tab) <- c(" ", colnames(tab)[-1])
  
  tab
}

liq_batch_func("aft")
liq_batch_func("cox")






install.packages("regmedint")
library(regmedint)


predict_liq <- function (fit, dat, t, what = NULL) {
  eta <- predict(fit, newdata = dat, type = "lp")
  lambda <- exp(eta)
  shape <- 1 / (fit$scale)
  if (what == "sale") {
    1-exp(-(t/lambda)^shape)
  }
  else if (what == "rmst") {
    sapply(lambda, function(l){
      integrate(
        f = function(x) exp(-(x/l)^shape),
        lower = 0,
        upper = t
      )$value
    })
  }
  else{
    exp(-(t/lambda)^shape)
  }
}

med_anl_once_func <- function (boot_idx, dat, u_eval_func = median, liq_params = NULL, predict_mode = "sim", n_sims = 1000) {
  # ================================================================
  # Params:
  # - `boot_idx`: Bootstrap indexes
  # - `dat`: Full database
  # - `u_eval_func`: Function for building moderation variable (median, min, max)
  # - `med_predict_mode`: Either sim for simulation or exp for expectation
  # - `n_sims`: Number of Monte-Carlo simulations
  # ================================================================
  
  # ================================================================
  # Variables:
  # ================================================================
  # current bootstrap loop database
  boot_dat = dat[boot_idx, ]
  
  # mediator model main regressors
  X_med_vars = c("DPC*nndays_li_30d")
  # mediator model controls
  Ct_med_vars = c("mvalue_li_30d")
  # outcome model main regressors
  X_out_vars = c("DPC", "att_ll_u*nndays_li_30d")
  # outcome model controls
  Ct_out_vars = c("LogPrice")
  # all (mediator and outcome) models controls
  Ct_loc_vars = c("CRD", "CPX", "CDX", "North")
  # quarters fixed effects
  qlist <- unique(boot_dat$quarter)[-1]
  
  # mediator variable
  Y_med_var = c("att_ll")
  Y_med_exp_var = c("att_ll_u")
  
  # outcome variable
  Y_out_time_var = "tom"
  Y_out_event_var = "sold"
  
  # moderation variable
  mod_var = "nndays_li_30d"
  
  # X variable
  x_var = "DPC"
  
  # Liquidity params
  liq_measure = "sale"
  liq_horizon = 30
  if (!is.null(liq_params)) {
    if (!is.null(liq_params$measure)){
      liq_measure = liq_params$measure
    }
    if (!is.null(liq_params$horizon)){
      liq_horizon = liq_params$horizon
    }
  }
  
  # ================================================================
  # STEP 1 - Initialization, fitting mediation & outcome models
  # ================================================================
  # fitting mediator model
  med_fit <- ord_model(
    dataset = boot_dat, 
    Y = Y_med_var, 
    X = c(X_med_vars, Ct_loc_vars, Ct_med_vars, qlist)
  )
  # fitting outcome model
  out_fit <- aft_model(
    boot_dat,
    Y_time = Y_out_time_var,
    Y_event = Y_out_event_var,
    X = c(X_out_vars, Ct_loc_vars, Ct_out_vars, qlist),
    error_cluster = "asset_id"
  )
  
  # ================================================================
  # STEP 2 - Counterfactual worlds variables
  # ================================================================
  # x       = "control" value of DPC (observed mean)
  x = median(boot_dat[[x_var]])
  # x_star  = "treatment" value of DPC (mean + 1 SD)
  x_star = quantile(boot_dat[[x_var]], probs = c(0.1))
  # u_eval  = "level" of moderation variable `nndays_li_30d` at which we eval mediation
  u_eval = u_eval_func(boot_dat[[mod_var]])
  
  # ================================================================
  # STEP 3 - Counterfactual worlds databases
  # ================================================================
  # counterfactuals databases: one with `x` and one with `xstar`
  # and all other covariates stay at observed values
  boot_dat_x <- boot_dat
  boot_dat_x[[x_var]] <- x
  boot_dat_x[[mod_var]] = u_eval
  boot_dat_xstar <- boot_dat
  boot_dat_xstar[[x_var]] <- x_star
  boot_dat_xstar[[mod_var]] = u_eval
  
  # ================================================================
  # STEP 4 - Mediator predictions across all counterfactuals db
  # ================================================================
  Y_med_probs_x <- NULL
  Y_med_probs_xstar <- NULL
  # if reg mediator has more than 2 categories
  if (is.factor(boot_dat[[Y_med_var]])) {
    Y_med_probs_x <- predict(med_fit, newdata = boot_dat_x, type = "probs")
    Y_med_probs_xstar <- predict(med_fit, newdata = boot_dat_xstar, type = "probs")
  }
  # else, reg mediator is binary
  else {
    Y_med_probs_x <- predict(med_fit, newdata = boot_dat_x, type = "response")
    Y_med_probs_x <- cbind(1-Y_med_probs_x, Y_med_probs_x)
    colnames(Y_med_probs_x) <- levels(boot_dat[[Y_med_exp_var]])
    Y_med_probs_xstar <- predict(med_fit, newdata = boot_dat_xstar, type = "response")
    Y_med_probs_xstar <- cbind(1-Y_med_probs_xstar, Y_med_probs_xstar)
    colnames(Y_med_probs_xstar) <- levels(boot_dat[[Y_med_exp_var]])
  }
  
  # ================================================================
  # STEP 5 - Outcome prediction utility function
  # ================================================================
  # Categories of mediator
  levels_med <- colnames(Y_med_probs_x)
  
  # predict liquidity outcome for a counterfactual world
  predict_liq_func <- function (med_val, dpc_val) {
    med_val_vec <- NULL
    if (length(med_val) == 1) {
      med_val_vec <- rep(med_val, nrow(boot_dat))
    } else {
      med_val_vec <- med_val
    }
    d <- boot_dat
    d[[Y_med_exp_var]] <- factor(med_val_vec, levels = levels_med)
    d[[x_var]] = dpc_val
    d[[mod_var]] = u_eval
    predict_liq(out_fit, dat = d, t = liq_horizon, what = liq_measure)
  }
  
  # ================================================================
  # STEP 6-A1 - Predict mediator & outcome by Monte carlo simulation 
  # ================================================================
  liq_x_med_x <- NULL
  liq_x_med_xstar <- NULL
  liq_xstar_med_xstar <- NULL
  liq_xstar_med_x <- NULL
  # browser()
  if (predict_mode == "sim") {
    
    # build random value of mediator according predicted probabilities of categories
    random_Y_med_func <- function (probs_mat) {
      apply(probs_mat, 1, function (p) sample(levels_med, size = 1, prob = p))
    }
    # simulations of mediator given `x` and `xstar`
    sim_med_given_x <- replicate(n_sims, random_Y_med_func(Y_med_probs_x), simplify = F)
    sim_med_given_xstar <- replicate(n_sims, random_Y_med_func(Y_med_probs_xstar), simplify = F)
    
    # prediction of liquidity outcomes for all counter factual worlds
    liq_x_med_x <- mean(sapply(sim_med_given_x, predict_liq_func, dpc_val = x))
    liq_x_med_xstar <- mean(sapply(sim_med_given_xstar, predict_liq_func, dpc_val = x))
    liq_xstar_med_xstar <- mean(sapply(sim_med_given_xstar, predict_liq_func, dpc_val = x_star))
    liq_xstar_med_x <- mean(sapply(sim_med_given_x, predict_liq_func, dpc_val = x_star))
    
  }
  # ================================================================
  # STEP 6-A2 - Predict mediator & outcome by Expectations 
  # ================================================================
  else {
    
    # prediction of liquidity outcomes for all counter factual worlds
    liq_x_med_all <- sapply(levels_med, predict_liq_func, dpc_val = x)
    liq_xstar_med_all <- sapply(levels_med, predict_liq_func, dpc_val = x_star)
    
    liq_x_med_x <- mean(rowSums(liq_x_med_all * Y_med_probs_x))
    liq_x_med_xstar <- mean(rowSums(liq_x_med_all * Y_med_probs_xstar))
    liq_xstar_med_xstar <- mean(rowSums(liq_xstar_med_all * Y_med_probs_xstar))
    liq_xstar_med_x <- mean(rowSums(liq_xstar_med_all * Y_med_probs_x))
    
  }
  
  # ================================================================
  # STEP 7 - Evaluate mediation effets
  # ================================================================
  TE = liq_xstar_med_xstar - liq_x_med_x
  TNIE = liq_xstar_med_xstar - liq_xstar_med_x
  PNDE = liq_xstar_med_x - liq_x_med_x
  PNIE = liq_x_med_xstar - liq_x_med_x
  TNDE = liq_xstar_med_xstar - liq_x_med_xstar
  
  eff <- c(TE, TNIE, PNDE, PNIE, TNDE)
  names(eff) <- c("TE", "TNIE", "PNDE", "PNIE", "TNDE")
  
  return(eff)
}

dat = x |> filter(regime == "Normal") |>
  filter(!is.na(tom) & tom > 0) |>
  mutate(
    year = year(date),
    quarter = paste0("Q", quarter(date), "_", year),
    sold = as.numeric(status == "filled" & tom <= 365),
    tom = if_else(tom > 365, 365, tom),
    LogPrice = log(Winsorize(price_usd, val = quantile(price_usd, probs = c(0.01, 0.99)))),
    att_ll_u = factor(att_ll, levels = levels(att_ll), ordered = F)
  ) |>
  mutate(qname = quarter) |> 
  mutate(v = 1) |> 
  pivot_wider(names_from = qname, values_from = v, values_fill = 0)

i <- 0
boot_results <- replicate(499, {
  idx <- sample(seq_len(nrow(dat)), replace = TRUE)
  r = med_anl_once_func(
    boot_idx = idx,
    dat,
    predict_mode = "exp"
  )
  i <<- i + 1
  cat("Running bootstrap replicate:", i, "of", 499, "\r")
  r
})

i <- 0
boot_results_2 <- replicate(1, {
  idx <- sample(seq_len(nrow(dat)), replace = TRUE)
  r = med_anl_once_func(
    boot_idx = idx,
    dat,
    predict_mode = "exp",
    u_eval_func = median,
    liq_params = list(horizon=7, measure="rmst")
  )
  i <<- i + 1
  cat("Running bootstrap replicate:", i, "of", 499, "\r")
  r
})




med_anl_bootstrap <- function () {
  
  
  
}














liq_fit = liq_model_func("Normal", T)
summary(liq_fit)

ordinal_model <- polr(att_ll ~ DPC + CRD + CPX + CDX + ndays_li_30d + mvalue_li_30d, data = x, Hess = TRUE)
summary(ordinal_model)

ordinal_model <- polr(
  att_ll ~ DPC + CRD + CPX + CDX + North, 
  data = x |> filter(regime == "Crash", ndays_li_30d == 0), 
  Hess = TRUE
)
summary(ordinal_model)

ordinal_model <- logit_model(
  dataset = x |> filter(regime == "Normal", ndays_li_30d == 0) |> mutate(Att = as.numeric(asks_ul_30d > 0)),
  Y = c("Att"),
  X = c("DPC", "CRD", "CPX", "CDX")
)
summary(ordinal_model)
PseudoR2(ordinal_model)

ordinal_model <- logit_model(
  dataset = x |> filter(regime == "Desert") |> mutate(Att = as.numeric(asks_ll_30d > 0)),
  Y = c("Att"),
  X = c("DPC", "CRD", "CPX", "CDX", "ndays_li_30d", "mvalue_li_30d")
)
summary(ordinal_model)
PseudoR2(ordinal_model)


p <- ggplot(x, aes(x = att_ll, y = DPC)) +
  geom_boxplot(size = .75) +
  facet_grid(regime ~ 1, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p


