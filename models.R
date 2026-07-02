library(plm)
library(fixest)
library(lmtest)
library(sandwich)
library(pscl)
library(survival)

lm_model <- function (dataset, Y, X, correct = T) {
  fml <- as.formula(paste0(Y, " ~ ", paste0(X, collapse = " + ")))
  res <- lm(fml, data = dataset, x = T, y = T)
  corrected <- coeftest(res, vcov = vcovHC(res, "HC1"))
  return(list(model=res, corrected=corrected))
}

logit_model <- function (dataset, Y, X) {
  fml <- as.formula(paste0(Y, " ~ ", paste0(X, collapse = " + ")))
  res <- glm(fml, data = dataset, family = "binomial", x = T, y = T)
  return(res)
}

felp_model <- function (dataset, Y, X, fe, cl) {
  fml <- as.formula(paste0(Y, " ~ ", paste0(X, collapse = " + "), " | ", paste0(fe, collapse = " + ")))
  fml_cl <- as.formula(paste0("~ ", paste0(cl, collapse = " + ")))
  res <- feols(fml, data = dataset, cluster = fml_cl)
  return(res)
}

felogit_model <- function (dataset, Y, X, fe, cl) {
  fml <- as.formula(paste0(Y, " ~ ", paste0(X, collapse = " + "), " | ", paste0(fe, collapse = " + ")))
  fml_cl <- as.formula(paste0("~ ", paste0(cl, collapse = " + ")))
  res <- feglm(fml, data = dataset, family = "binomial", cluster = fml_cl)
  return(res)
}

filter_outliers <- function(x) {
  # Calculate thresholds once to keep it fast
  q25 <- quantile(x, 0.25, na.rm = TRUE)
  q75 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- IQR(x, na.rm = TRUE)
  
  # Check if x is within the normal range (not an outlier)
  return(x >= (q25 - 1.5 * iqr_val) & x <= (q75 + 1.5 * iqr_val))
}

hurdle_model <- function (dataset, Y, X, count_dist = "negbin") {
  fml <- as.formula(paste0(Y, " ~ ", paste0(X, collapse = " + "), " | ", paste0(X, collapse = " + ")))
  fit <- hurdle(
    fml,
    data = dataset, 
    count_dist = count_dist
  )
  return(fit)
}

aft_model <- function (dataset, Y_time, Y_event, X, error_cluster = NULL, dist = "weibull") {
  fml_str <- paste0("survival::Surv(", Y_time, ", ", Y_event, ") ~ ", paste0(X, collapse = " + "))
  if(!is.null(error_cluster)) {
    fml_str <- paste0(fml_str, " + survival::cluster(", error_cluster, ")")
  }
  fml <- as.formula(fml_str)
  fit <- survival::survreg(
    fml,
    data = dataset,
    dist = dist
  )
  return(fit)
}

cox_model <- function (dataset, Y_time, Y_event, X, error_cluster = NULL) {
  fml_str <- paste0("survival::Surv(", Y_time, ", ", Y_event, ") ~ ", paste0(X, collapse = " + "))
  if(!is.null(error_cluster)) {
    fml_str <- paste0(fml_str, " + survival::cluster(", error_cluster, ")")
  }
  fml <- as.formula(fml_str)
  fit <- survival::coxph(
    fml,
    data = dataset
  )
  return(fit)
}