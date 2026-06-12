library(plm)
library(fixest)
library(lmtest)
library(sandwich)

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