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
  mutate(
    # att_ll = if_else(asks_ll_30d == 0, "Z", if_else(asks_ll_30d == 1, "B", if_else(asks_ll_30d < 5, "A", "S"))),
    # att_ul = if_else(asks_ul_30d == 0, "Z", if_else(asks_ul_30d == 1, "B", if_else(asks_ul_30d < 5, "A", "S"))),
    att_ll = if_else(asks_ll_30d == 0, "Z", if_else(asks_ll_30d <= 3, "L", "H")),
    att_ul = if_else(asks_ul_30d == 0, "Z", if_else(asks_ll_30d <= 3, "L", "H"))
  ) |>
  mutate(
    # att_ll = factor(att_ll, levels = c("Z", "B", "A", "S"), ordered = T),
    # att_ul = factor(att_ul, levels = c("Z", "B", "A", "S"), ordered = T),
    att_ll = factor(att_ll, levels = c("Z", "L", "H"), ordered = T),
    att_ul = factor(att_ul, levels = c("Z", "L", "H"), ordered = T),
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
  Xcols <- c("DPC", "CRD", "CPX", "CDX", "North", "listed", "nndays_li_30d", "nndays_li_30d:DPC", "mvalue_li_30d")
  #Xcols <- c("DPC", "CRD", "CPX", "CDX", "North", "mvalue_li_30d")
  #Xcols <- c("DPC", "CRD", "CPX", "CDX", "North")
  #Xcols <- c("DPC", "CRD", "CPX", "CDX", "North", "ndays_li_30d")
  Xcols <- c(Xcols, qlist[-1])
  # model <- NULL
  # if(length(unique(dat[["att_ll"]])) > 2) {
  #   model <- ord_model(dataset = dat, Y = c("att_ll"), X = Xcols)
  # } else {
  #   model <- logit_model(dataset = dat, Y = c("Att"), X = Xcols)
  # }
  model <- logit_model(dataset = dat, Y = c("att_dmn"), X = Xcols)
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
      LogPrice = log(Winsorize(price_usd, val = quantile(price_usd, probs = c(0.01, 0.99)))),
      att_ll_u = factor(att_ll, levels = levels(att_ll), ordered = F),
      att_ul_u = factor(att_ul, levels = levels(att_ul), ordered = F)
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
    #Xcols <- c(Xcols, c("att_ll", "att_ll:nndays_li_30d"))
    Xcols <- c(Xcols, c("att_dmn", "att_dmn:nndays_li_30d"))
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
brm_med_anl_func <- function () {
  
  dat = x |> filter(regime == "Normal") |>
    filter(!is.na(tom) & tom > 0) |>
    mutate(
      year = year(date),
      quarter = paste0("Q", quarter(date), "_", year),
      sold = as.numeric(status == "filled" & tom <= 365),
      tom = if_else(tom > 365, 365, tom),
      LogPrice = log(Winsorize(price_usd, val = quantile(price_usd, probs = c(0.01, 0.99)))),
      att_ll_u = factor(att_ll, levels = levels(att_ll), ordered = F),
      att_ul_u = factor(att_ul, levels = levels(att_ul), ordered = F),
      CRD = as.numeric(CRD == "Yes"),
      CPX = as.numeric(CPX == "Yes"),
      CDX = as.numeric(CDX == "Yes"),
      North = as.numeric(North == "Yes"),
      listed = as.numeric(listed == "Y")
    ) |>
    mutate(qname = quarter) |> 
    mutate(v = 1) |> 
    pivot_wider(names_from = qname, values_from = v, values_fill = 0)
  
  
  
}














liq_fit = liq_model_func("Normal", T)
summary(liq_fit)

ordinal_model <- polr(att_ll ~ DPC + CRD + CPX + CDX + ndays_li_30d + mvalue_li_30d, data = x, Hess = TRUE)
summary(ordinal_model)

ordinal_model <- polr(att_ul ~ DPC + CRD + CPX + CDX + North, data = x |> filter(regime == "Crash", ndays_li_30d == 0), Hess = TRUE)
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


