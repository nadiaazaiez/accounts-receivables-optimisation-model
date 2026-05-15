
#Apolix Accounts Receivable — Data Cleaning
# File: apolix_01_cleaning.R

# 0. PACKAGES
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(survival)
library(ggsurvfit)
library(scales)
library(robustbase)

# 1. SETTINGS 
FILE_PATH      <- "/Users/nadiaazaiez/Desktop/anonymized apolix1.xlsx"
CENSORING_DATE <- as_date("2026-03-11")
PLOT_DIR       <- "thesis_plots"
SHEET          <- "complete cleam  ANO"

#2. LOAD & CLEAN COLUMN NAMES 

raw <- read_excel(FILE_PATH, sheet = SHEET,
                  skip = 11, col_names = TRUE, guess_max = 1000) |>
  clean_names() |>
  select(where(~ !all(is.na(.))))

if (!"account" %in% names(raw)) names(raw)[1] <- "account"
cat("Raw:", nrow(raw), "rows x", ncol(raw), "cols\n\n")

#3. SELECT and RENAME 
df <- raw |>
  select(
    account             = account,
    invoice_id          = sales_invoice_id,
    customer_ref        = customer_reference,
    invoice_date        = invoice_date,
    payment_term_days   = payment_term,
    due_date            = due_date,
    currency            = invoice_currency,
    invoice_total       = invoice_total,
    payment_status      = payment_status,
    payment_date_raw    = payment_date,
    project_id          = project_id,
    project_name        = project_name,
    match_type          = match_type,
    country             = country,
    account_outstanding = account_outstanding,
    account_total       = account_total,
    payment_risk_raw    = payemnt_risk_invocie_creation_date
  )

#4. TYPE COERCION 
df <- df |>
  mutate(
    invoice_date        = as_date(invoice_date),
    due_date            = as_date(due_date),
    payment_date        = as_date(payment_date_raw),
    invoice_total       = as.numeric(invoice_total),
    payment_term_days   = as.numeric(payment_term_days),
    account_outstanding = as.numeric(account_outstanding),
    account_total       = as.numeric(account_total),
    payment_risk = case_when(
      str_to_lower(as.character(payment_risk_raw)) == "na" ~ NA_real_,
      is.na(payment_risk_raw) ~ NA_real_,
      TRUE ~ as.numeric(payment_risk_raw)
    ),
    payment_status = str_to_title(str_trim(payment_status)),
    currency       = str_to_upper(str_trim(currency)),
    match_type     = str_to_title(str_trim(match_type)),
    account        = str_to_upper(str_trim(account)),
    country        = str_to_title(str_trim(country))
  ) |>
  select(-payment_date_raw, -payment_risk_raw)

# 5. EXCLUSION STEPS 

n_start <- nrow(df)

df_clean <- df |>
  filter(
    !is.na(invoice_id),
    !is.na(invoice_date),
    !is.na(due_date),
    !is.na(payment_status),
    !is.na(invoice_total), invoice_total > 0,
    due_date >= invoice_date
  )

cat("Rows removed:", n_start - nrow(df_clean), "\n")
cat("Rows kept:   ", nrow(df_clean), "\n\n")

# ── 6. SURVIVAL VARIABLES ────────────────────────────────────────────────────
df_clean <- df_clean |>
  mutate(
    event = if_else(payment_status == "Paid", 1L, 0L),
    time_to_payment = as.numeric(
      if_else(event == 1L, payment_date, CENSORING_DATE) - invoice_date
    ),
    days_overdue = as.numeric(payment_date - due_date),
    paid_label = case_when(
      event == 1L & days_overdue < 0  ~ "Early",
      event == 1L & days_overdue == 0 ~ "On Due Date",
      event == 1L & days_overdue > 0  ~ "Late",
      event == 0L                     ~ "Unpaid (censored)"
    ) |> factor(levels = c("Early", "On Due Date", "Late", "Unpaid (censored)"))
  ) |>
  filter(time_to_payment > 0)

print(df_clean |> count(event, paid_label) |>
        mutate(pct = scales::percent(n / sum(n), 0.1)))
cat("\n")

#7. CUSTOMER BEHAVIOURAL FEATURES
paid_history <- df_clean |>
  filter(event == 1L) |>
  select(account, invoice_date, payment_date, days_overdue)

fallback_avg_delay    <- mean(paid_history$days_overdue,     na.rm = TRUE)
fallback_late_ratio   <- mean(paid_history$days_overdue > 0, na.rm = TRUE)
fallback_variance     <- var(paid_history$days_overdue,      na.rm = TRUE)
fallback_median_delay <- median(paid_history$days_overdue,   na.rm = TRUE)
fallback_qn_scale     <- Qn(paid_history$days_overdue,       na.rm = TRUE)


behaviour_features <- df_clean |>
  select(invoice_id, account, invoice_date) |>
  pmap_dfr(function(invoice_id, account, invoice_date) {
    prior <- filter(paid_history,
                    account      == !!account,
                    payment_date <  !!invoice_date)
    n <- nrow(prior)
    tibble(
      invoice_id,
      hist_avg_delay      = if (n >= 1) mean(prior$days_overdue)           else NA_real_,
      hist_delay_variance = if (n >= 2) var(prior$days_overdue)            else NA_real_,
      hist_late_ratio     = if (n >= 1) mean(prior$days_overdue > 0)       else NA_real_,
      hist_invoice_freq   = n,
      hist_median_delay   = if (n >= 1) median(prior$days_overdue)         else NA_real_,
      hist_qn_scale       = if (n >= 2) Qn(prior$days_overdue, na.rm=TRUE) else NA_real_
    )
  }) |>
  mutate(
    hist_avg_delay      = coalesce(hist_avg_delay,      fallback_avg_delay),
    hist_delay_variance = coalesce(hist_delay_variance, fallback_variance),
    hist_late_ratio     = coalesce(hist_late_ratio,     fallback_late_ratio),
    hist_median_delay   = coalesce(hist_median_delay,   fallback_median_delay),
    hist_qn_scale       = coalesce(hist_qn_scale,       fallback_qn_scale)
  )

df_model <- df_clean |>
  left_join(behaviour_features, by = "invoice_id") |>
  mutate(country = factor(country))


#8. SAVE
saveRDS(df_model, "apolix_model_ready.rds")
write_csv(df_model, "apolix_model_ready.csv")

cat(sprintf("  Total invoices       : %d\n",   nrow(df_model)))
cat(sprintf("  Paid (event = 1)     : %d (%.1f%%)\n",
            sum(df_model$event), mean(df_model$event) * 100))
cat(sprintf("  Censored (event = 0) : %d (%.1f%%)\n",
            sum(df_model$event == 0), mean(df_model$event == 0) * 100))
cat(sprintf("  Unique customers     : %d\n",   n_distinct(df_model$account)))
cat(sprintf("  Countries            : %d\n",   n_distinct(df_model$country)))
cat(sprintf("  Date range           : %s to %s\n",
            min(df_model$invoice_date), max(df_model$invoice_date)))
cat(sprintf("  Censoring date       : %s\n",   CENSORING_DATE))

#9. PLOTS
dir.create(PLOT_DIR, showWarnings = FALSE)

theme_thesis <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey45"))

PAL <- c("Early"             = "#4C9BE8",
         "On Due Date"       = "#2ECC71",
         "Late"              = "#E07B54",
         "Unpaid (censored)" = "#A0A0A0")

save_plot <- function(name, plot_obj, w = 1200, h = 750) {
  path <- file.path(PLOT_DIR, name)
  png(path, width = w, height = h, res = 150)
  print(plot_obj)
  dev.off()
  cat("Saved:", path, "\n")
}

# P9 — KM overall 
save_plot("p9_km_overall.png",
  survfit(Surv(time_to_payment, event) ~ 1, data = df_model) |>
    ggsurvfit(linewidth = 1) +
    add_confidence_interval(fill = "#4C9BE8", alpha = 0.15) +
    add_risktable(risktable_stats = c("n.risk", "n.event")) +
    scale_x_continuous(breaks = seq(0, 240, 60), limits = c(0, 240)) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = NULL, subtitle = NULL,
         x = "Days Since Invoice Date",
         y = "Prob. of Remaining Unpaid") +
    theme_thesis,
  w = 1500, h = 1050
)




# File   : apolix_02_cox_model.R

# 0. PACKAGES 
library(tidyverse)
library(survival)
library(survminer)
library(car)
library(timeROC)
library(glmnet)
library(scales)
library(broom)

PLOT_DIR <- "thesis_plots"
dir.create(PLOT_DIR, showWarnings = FALSE)

theme_thesis <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey45"))

# 1. LOAD DATA 
df <- readRDS("apolix_model_ready.rds")
cat("Loaded:", nrow(df), "rows |", sum(df$event), "events\n\n")

# 2. FEATURE PREPARATION 

median_risk <- median(df$payment_risk, na.rm = TRUE)

df_prep <- df |>
  mutate(
    payment_risk_imp        = if_else(is.na(payment_risk), median_risk, payment_risk),
    payment_risk_miss       = as.integer(is.na(payment_risk)),
    log_invoice_total       = log1p(invoice_total),
    log_account_outstanding = log1p(account_outstanding),
    currency = as.character(currency),
    country  = as.character(country)
  )

cat(sprintf("payment_risk imputed for: %d rows\n\n",
            sum(df_prep$payment_risk_miss)))

#3. TEMPORAL TRAIN / TEST SPLIT
cutoff_date <- as.Date(quantile(as.numeric(df_prep$invoice_date), 0.80),
                        origin = "1970-01-01")
cat("Train/test cutoff date:", format(cutoff_date), "\n")

train_raw <- df_prep |> filter(invoice_date <= cutoff_date)
test_raw  <- df_prep |> filter(invoice_date >  cutoff_date)

currency_levels <- sort(unique(train_raw$currency))
country_levels  <- c("Germany",
                      sort(setdiff(unique(train_raw$country), "Germany")))

train <- train_raw |>
  mutate(currency = factor(currency, levels = currency_levels),
         country  = factor(country,  levels = country_levels))

test <- test_raw |>
  mutate(currency = factor(currency, levels = currency_levels),
         country  = factor(country,  levels = country_levels))

cat(sprintf("Train: %d rows | %d events (%.1f%%)\n",
            nrow(train), sum(train$event), mean(train$event) * 100))
cat(sprintf("Test : %d rows | %d events (%.1f%%)\n\n",
            nrow(test),  sum(test$event),  mean(test$event)  * 100))

#4. VIF CHECK 
vif_model <- lm(time_to_payment ~
                  log_invoice_total + payment_term_days + payment_risk_imp +
                  log_account_outstanding + hist_avg_delay +
                  hist_delay_variance + hist_late_ratio + hist_invoice_freq,
                data = filter(train, event == 1))

vif_vals <- vif(vif_model)
print(round(vif_vals, 2))

if (any(vif_vals > 5)) {
  cat(sprintf("\n⚠ VIF > 5: %s\n\n",
              paste(names(vif_vals[vif_vals > 5]), collapse = ", ")))
} else {
  cat("\n✓ All VIF < 5\n\n")
}

#5. MODEL FORMULA 
cox_formula <- Surv(time_to_payment, event) ~
  log_invoice_total + payment_term_days +
  payment_risk_imp + payment_risk_miss +
  log_account_outstanding + hist_avg_delay +
  hist_delay_variance + hist_late_ratio +
  hist_invoice_freq + currency + country

#6. STANDARD COX MODEL
cox_fit <- coxph(cox_formula, data = train,
                  x = TRUE, y = TRUE, ties = "efron")
print(summary(cox_fit))

hr_table <- tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high, p.value) |>
  rename(HR = estimate, CI_low = conf.low, CI_high = conf.high) |>
  mutate(across(where(is.numeric), ~ round(., 4)))
print(hr_table, n = 30)
write_csv(hr_table, "cox_hazard_ratios.csv")


#7. PH ASSUMPTION TEST
ph_test <- cox.zph(cox_fit)
print(ph_test)

ph_violations <- rownames(ph_test$table)[ph_test$table[, "p"] < 0.05]
ph_violations <- ph_violations[ph_violations != "GLOBAL"]
cat(sprintf("\n⚠ PH violated for: %s\n\n",
            paste(ph_violations, collapse = ", ")))

png(file.path(PLOT_DIR, "cox_ph_assumption.png"),
    width = 1500, height = 1200, res = 150)
ggcoxzph(ph_test)
dev.off()


#8. RIDGE COX 
train_complete <- train |> filter(!is.na(currency), !is.na(country))
test_complete  <- test  |> filter(!is.na(currency), !is.na(country))

cat(sprintf("Complete rows — Train: %d | Test: %d\n",
            nrow(train_complete), nrow(test_complete)))

X_train <- model.matrix(cox_formula, data = train_complete)[, -1]
y_train <- Surv(train_complete$time_to_payment, train_complete$event)
X_test  <- model.matrix(cox_formula, data = test_complete)[, -1]

missing_cols <- setdiff(colnames(X_train), colnames(X_test))
for (col in missing_cols) {
  X_test <- cbind(X_test,
                   matrix(0, nrow=nrow(X_test), ncol=1,
                           dimnames=list(NULL, col)))
}
X_test <- X_test[, colnames(X_train), drop = FALSE]

set.seed(42)
cv_ridge <- cv.glmnet(X_train, y_train, family="cox",
                       alpha=0, nfolds=5, type.measure="C")

cat(sprintf("lambda.min: %.4f | lambda.1se: %.4f\n",
            cv_ridge$lambda.min, cv_ridge$lambda.1se))

ridge_fit <- glmnet(X_train, y_train, family="cox",
                     alpha=0, lambda=cv_ridge$lambda.min)


#9. MODEL EVALUATION 

cox_pred_test   <- predict(cox_fit, newdata=test_complete, type="lp")
ridge_pred_test <- as.vector(predict(ridge_fit, newx=X_test, type="link"))

conc_cox   <- concordance(Surv(time_to_payment, event) ~ I(-cox_pred_test),
                           data=test_complete)
conc_ridge <- concordance(Surv(time_to_payment, event) ~ I(-ridge_pred_test),
                           data=test_complete)

cindex_cox   <- conc_cox$concordance
cindex_ridge <- conc_ridge$concordance
ci_cox_low   <- cindex_cox   - 1.96*sqrt(conc_cox$var)
ci_cox_high  <- cindex_cox   + 1.96*sqrt(conc_cox$var)
ci_ridge_low <- cindex_ridge - 1.96*sqrt(conc_ridge$var)
ci_ridge_high<- cindex_ridge + 1.96*sqrt(conc_ridge$var)

cat(sprintf("C-Index — Standard Cox : %.4f (95%% CI: %.4f-%.4f)\n",
            cindex_cox, ci_cox_low, ci_cox_high))
cat(sprintf("C-Index — Ridge Cox    : %.4f (95%% CI: %.4f-%.4f)\n\n",
            cindex_ridge, ci_ridge_low, ci_ridge_high))

# Time-dependent AUC
max_time  <- max(test_complete$time_to_payment)
auc_times <- c(30, 60, 90)[c(30, 60, 90) < max_time]

roc_cox <- timeROC(T=test_complete$time_to_payment, delta=test_complete$event,
                    marker=cox_pred_test, cause=1, weighting="marginal",
                    times=auc_times, iid=TRUE)

roc_ridge <- timeROC(T=test_complete$time_to_payment, delta=test_complete$event,
                      marker=ridge_pred_test, cause=1, weighting="marginal",
                      times=auc_times, iid=TRUE)
# Name AUC vectors so they survive missing time points
names(roc_cox$AUC)   <- auc_times
names(roc_ridge$AUC) <- auc_times

safe_auc <- function(roc, t) {
  v <- roc$AUC[as.character(t)]
  if (length(v) == 0 || is.na(v)) NA_real_ else unname(v)
}                      


cat(sprintf("  Cox  : t30=%.4f | t60=%.4f | t90=%.4f | mean=%.4f\n",
            roc_cox$AUC[1], roc_cox$AUC[2], roc_cox$AUC[3],
            mean(roc_cox$AUC)))
cat(sprintf("  Ridge: t30=%.4f | t60=%.4f | t90=%.4f | mean=%.4f\n\n",
            roc_ridge$AUC[1], roc_ridge$AUC[2], roc_ridge$AUC[3],
            mean(roc_ridge$AUC)))

#10. SAVE 
saveRDS(cox_fit,        "cox_fit.rds")
saveRDS(ridge_fit,      "cox_ridge_fit.rds")
saveRDS(train,          "train_set.rds")
saveRDS(test,           "test_set.rds")
saveRDS(train_complete, "train_complete.rds")
saveRDS(test_complete,  "test_complete.rds")
saveRDS(X_train,        "X_train.rds")
saveRDS(X_test,         "X_test.rds")

eval_summary <- tibble(
  model    = c("Standard Cox", "Ridge Cox"),
  c_index  = c(cindex_cox,   cindex_ridge),
  ci_low   = c(ci_cox_low,   ci_ridge_low),
  ci_high  = c(ci_cox_high,  ci_ridge_high),
  auc_t30  = c(safe_auc(roc_cox, 30), safe_auc(roc_ridge, 30)),
  auc_t60  = c(safe_auc(roc_cox, 60), safe_auc(roc_ridge, 60)),
  auc_t90  = c(safe_auc(roc_cox, 90), safe_auc(roc_ridge, 90))
) |> mutate(across(where(is.numeric), ~ round(., 4)))
write_csv(eval_summary, "cox_evaluation_summary.csv")

cat(sprintf("  Train: %d rows | Test: %d rows\n", nrow(train), nrow(test)))
cat(sprintf("  C-Index Standard Cox : %.4f\n", cindex_cox))
cat(sprintf("  C-Index Ridge Cox    : %.4f\n", cindex_ridge))


# File   : apolix_03_gbsa.R


# 0. PACKAGES 
library(tidyverse)
library(survival)
library(gbm)
library(timeROC)
library(scales)
library(glmnet)

PLOT_DIR <- "thesis_plots"
dir.create(PLOT_DIR, showWarnings = FALSE)

theme_thesis <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey45"))

# 1. LOAD DATA
train_raw <- readRDS("train_set.rds")
test_raw  <- readRDS("test_set.rds")

train <- train_raw |> arrange(invoice_date)
test  <- test_raw  |> arrange(invoice_date)

train_complete <- train |> filter(!is.na(currency), !is.na(country))
test_complete  <- test  |> filter(!is.na(currency), !is.na(country))

cox_results <- read_csv("cox_evaluation_summary.csv", show_col_types = FALSE)

cox_row   <- cox_results |> filter(model == "Standard Cox")
ridge_row <- cox_results |> filter(model == "Ridge Cox")

cindex_cox    <- cox_row$c_index
cindex_ridge  <- ridge_row$c_index
ci_cox_low    <- cox_row$ci_low
ci_cox_high   <- cox_row$ci_high
ci_ridge_low  <- ridge_row$ci_low
ci_ridge_high <- ridge_row$ci_high

auc_cox   <- c(cox_row$auc_t30,   cox_row$auc_t60,   cox_row$auc_t90)
auc_ridge <- c(ridge_row$auc_t30, ridge_row$auc_t60, ridge_row$auc_t90)

cat(sprintf("Train: %d rows | Test: %d rows\n",
            nrow(train_complete), nrow(test_complete)))
cat(sprintf("Cox C-Index: %.4f | Ridge C-Index: %.4f\n\n",
            cindex_cox, cindex_ridge))

#2. PREPARE FEATURES FOR GBM 
prep_gbm <- function(df) {
  df |>
    mutate(
      currency_GBP         = as.integer(currency == "GBP"),
      country_Belgium      = as.integer(country == "Belgium"),
      country_France       = as.integer(country == "France"),
      country_Ireland      = as.integer(country == "Ireland"),
      country_Italy        = as.integer(country == "Italy"),
      country_Jersey       = as.integer(country == "Jersey"),
      country_Netherlands  = as.integer(country == "Netherlands"),
      country_Poland       = as.integer(country == "Poland"),
      country_Saudi_Arabia = as.integer(country == "Saudi Arabia"),
      country_Slovakia     = as.integer(country == "Slovakia"),
      country_South_Africa = as.integer(country == "South Africa"),
      country_Spain        = as.integer(country == "Spain"),
      country_Sweden       = as.integer(country == "Sweden"),
      country_Switzerland  = as.integer(country == "Switzerland"),
      country_UK           = as.integer(country == "United Kingdom")
    ) |>
    select(
      time_to_payment, event,
      log_invoice_total, payment_term_days,
      payment_risk_imp, payment_risk_miss,
      log_account_outstanding,
      hist_avg_delay, hist_delay_variance,
      hist_late_ratio, hist_invoice_freq,
      currency_GBP,
      country_Belgium, country_France, country_Ireland,
      country_Italy, country_Jersey, country_Netherlands,
      country_Poland, country_Saudi_Arabia, country_Slovakia,
      country_South_Africa, country_Spain, country_Sweden,
      country_Switzerland, country_UK
    )
}

train_gbm <- prep_gbm(train_complete)
test_gbm  <- prep_gbm(test_complete)
feature_names <- setdiff(names(train_gbm), c("time_to_payment", "event"))

# 3. HYPERPARAMETER TUNING

tune_grid <- expand.grid(
  n.trees           = c(500, 1000),
  interaction.depth = c(1, 2),
  shrinkage         = c(0.005, 0.01),
  n.minobsinnode    = c(20, 30)
)

n_train   <- nrow(train_gbm)
n_folds   <- 5
fold_size <- floor(n_train / (n_folds + 1))
cv_results <- list()

for (i in seq_len(nrow(tune_grid))) {
  params      <- tune_grid[i, ]
  fold_cindex <- rep(NA_real_, n_folds)

  for (f in seq_len(n_folds)) {
    train_end <- fold_size * f
    val_start <- train_end + 1
    val_end   <- min(train_end + fold_size, n_train)
    cv_train  <- train_gbm[1:train_end, ]
    cv_val    <- train_gbm[val_start:val_end, ]

    if (sum(cv_val$event) == 0 || nrow(cv_train) < 20) next

    tryCatch({
      fit_cv <- gbm(
        formula           = Surv(time_to_payment, event) ~ .,
        data              = cv_train[, c("time_to_payment", "event", feature_names)],
        distribution      = "coxph",
        n.trees           = params$n.trees,
        interaction.depth = params$interaction.depth,
        shrinkage         = params$shrinkage,
        n.minobsinnode    = params$n.minobsinnode,
        bag.fraction      = 0.7,
        verbose           = FALSE
      )
      pred_cv        <- predict(fit_cv, newdata=cv_val,
                                 n.trees=params$n.trees, type="link")
      fold_cindex[f] <- concordance(Surv(time_to_payment,event) ~
                          I(-pred_cv), data=cv_val)$concordance
    }, error = function(e) { fold_cindex[f] <<- NA })
  }

  cv_results[[i]] <- data.frame(
    n.trees=params$n.trees, interaction.depth=params$interaction.depth,
    shrinkage=params$shrinkage, n.minobsinnode=params$n.minobsinnode,
    mean_cindex=mean(fold_cindex, na.rm=TRUE),
    valid_folds=sum(!is.na(fold_cindex))
  )
}

tuning_results <- bind_rows(cv_results) |>
  filter(valid_folds >= 2) |>
  arrange(desc(mean_cindex))
best_params <- tuning_results[1, ]
write_csv(tuning_results, "gbsa_tuning_results.csv")

cat(sprintf("Best: n.trees=%d | depth=%d | shrink=%.3f | minobs=%d\n\n",
            best_params$n.trees, best_params$interaction.depth,
            best_params$shrinkage, best_params$n.minobsinnode))

# 4. FIT FINAL GBSA MODEL
set.seed(42)
gbsa_fit <- gbm(
  formula           = Surv(time_to_payment, event) ~ .,
  data              = train_gbm[, c("time_to_payment","event",feature_names)],
  distribution      = "coxph",
  n.trees           = best_params$n.trees,
  interaction.depth = best_params$interaction.depth,
  shrinkage         = best_params$shrinkage,
  n.minobsinnode    = best_params$n.minobsinnode,
  bag.fraction      = 0.7,
  verbose           = FALSE
)

# 5. VARIABLE IMPORTANCE 
imp <- summary(gbsa_fit, n.trees=best_params$n.trees, plotit=FALSE) |>
  as_tibble() |>
  rename(variable=var, importance=rel.inf) |>
  arrange(desc(importance)) |>
  slice_head(n=15)

write_csv(imp, "gbsa_variable_importance.csv")
cat("Variable importance (top 10):\n")
print(imp |> head(10))
cat("\n")

# 6. MODEL EVALUATION

gbsa_pred_test <- predict(gbsa_fit, newdata=test_gbm,
                           n.trees=best_params$n.trees, type="link")

conc_gbsa   <- concordance(Surv(time_to_payment,event) ~ I(-gbsa_pred_test),
                             data=test_complete)
cindex_gbsa <- conc_gbsa$concordance
ci_gbsa_low <- cindex_gbsa - 1.96*sqrt(conc_gbsa$var)
ci_gbsa_high<- cindex_gbsa + 1.96*sqrt(conc_gbsa$var)

# Train-test gap
gbsa_pred_train <- predict(gbsa_fit, newdata=train_gbm,
                             n.trees=best_params$n.trees, type="link")
cindex_train <- concordance(Surv(time_to_payment,event) ~
                  I(-gbsa_pred_train), data=train_complete)$concordance
gap_gbsa <- cindex_train - cindex_gbsa

roc_gbsa <- timeROC(T=test_complete$time_to_payment,
                     delta=test_complete$event,
                     marker=gbsa_pred_test, cause=1,
                     weighting="marginal", times=c(30,60,90), iid=TRUE)

cat(sprintf("GBSA C-Index : %.4f (95%% CI: %.4f-%.4f)\n",
            cindex_gbsa, ci_gbsa_low, ci_gbsa_high))
cat(sprintf("Train-test gap: %.4f\n", gap_gbsa))
cat(sprintf("AUC: t30=%.4f | t60=%.4f | t90=%.4f | mean=%.4f\n\n",
            roc_gbsa$AUC[1], roc_gbsa$AUC[2], roc_gbsa$AUC[3],
            mean(roc_gbsa$AUC)))

#7. UNIFIED COMPARISON TABLE 
comparison_table <- tibble(
  Model    = c("Standard Cox", "Ridge Cox", "GBSA"),
  C_Index  = c(cindex_cox,   cindex_ridge,  cindex_gbsa),
  CI_Low   = c(ci_cox_low,   ci_ridge_low,  ci_gbsa_low),
  CI_High  = c(ci_cox_high,  ci_ridge_high, ci_gbsa_high),
  AUC_t30  = c(auc_cox[1], auc_ridge[1], roc_gbsa$AUC[1]),
  AUC_t60  = c(auc_cox[2], auc_ridge[2], roc_gbsa$AUC[2]),
  AUC_t90  = c(auc_cox[3], auc_ridge[3], roc_gbsa$AUC[3]),
  Train_Gap = c(NA, NA, gap_gbsa)
) |> mutate(across(where(is.numeric), ~ round(., 4)))



write_csv(comparison_table, "model_comparison_table.csv")
saveRDS(comparison_table,   "model_comparison_table.rds")
print(comparison_table)

#8. SAVE 
saveRDS(gbsa_fit,       "gbsa_fit.rds")
saveRDS(gbsa_pred_test, "gbsa_pred_test.rds")
saveRDS(roc_gbsa,       "gbsa_roc.rds")




# Behavioural Z-Score Framework
# File   : apolix_04_zscores.R


# 0. PACKAGES 
library(tidyverse)
library(survival)
library(glmnet)
library(robustbase)
library(scales)

PLOT_DIR <- "thesis_plots"
dir.create(PLOT_DIR, showWarnings = FALSE)

theme_thesis <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey45"))

#1. LOAD DATA AND MODELS 

df             <- readRDS("apolix_model_ready.rds")
ridge_fit      <- readRDS("cox_ridge_fit.rds")
train_complete <- readRDS("train_complete.rds")
test_complete  <- readRDS("test_complete.rds")
X_train        <- readRDS("X_train.rds")

# Full portfolio: combine train and test
full_data <- bind_rows(train_complete, test_complete) |>
  arrange(invoice_date)

cat(sprintf("Full portfolio: %d invoices | %d customers\n\n",
            nrow(full_data), n_distinct(full_data$account)))

#2. RIDGE COX LINEAR PREDICTORS 


cox_formula <- Surv(time_to_payment, event) ~
  log_invoice_total + payment_term_days +
  payment_risk_imp + payment_risk_miss +
  log_account_outstanding + hist_avg_delay +
  hist_delay_variance + hist_late_ratio +
  hist_invoice_freq + currency + country

currency_levels <- levels(train_complete$currency)
country_levels  <- levels(train_complete$country)

full_data_factored <- full_data |>
  mutate(currency = factor(currency, levels=currency_levels),
         country  = factor(country,  levels=country_levels)) |>
  filter(!is.na(currency), !is.na(country))

X_full <- model.matrix(cox_formula, data=full_data_factored)[, -1]

missing_cols <- setdiff(colnames(X_train), colnames(X_full))
for (col in missing_cols) {
  X_full <- cbind(X_full, matrix(0, nrow=nrow(X_full), ncol=1,
                                   dimnames=list(NULL, col)))
}
X_full <- X_full[, colnames(X_train), drop=FALSE]

lp_full <- as.vector(predict(ridge_fit, newx=X_full,
                               s=ridge_fit$lambda, type="link"))
cat(sprintf("Linear predictors computed for %d invoices\n\n", length(lp_full)))

#3. RMST 
proxy_df  <- full_data_factored |> mutate(ridge_lp = lp_full)
proxy_cox <- coxph(Surv(time_to_payment, event) ~ ridge_lp, data=proxy_df)
km_ridge  <- survfit(proxy_cox, newdata=proxy_df)

L <- quantile(full_data_factored$time_to_payment, 0.75, na.rm=TRUE)
cat(sprintf("Integration horizon L = %.1f days (75th percentile)\n", L))

compute_rmst <- function(surv_matrix, times, L) {
  n_invoices <- ncol(surv_matrix)
  rmst_vals  <- numeric(n_invoices)
  t_grid <- c(0, times[times <= L])
  for (j in seq_len(n_invoices)) {
    s_vals       <- c(1, surv_matrix[times <= L, j])
    rmst_vals[j] <- sum(diff(t_grid) *
                          (head(s_vals,-1) + tail(s_vals,-1)) / 2)
  }
  rmst_vals
}

Y_i <- compute_rmst(km_ridge$surv, km_ridge$time, L)
cat(sprintf("Y_i: mean=%.1f | range %.1f-%.1f days\n\n",
            mean(Y_i), min(Y_i), max(Y_i)))

full_data_scored <- full_data_factored |>
  mutate(Y_i=Y_i, predicted_delay=Y_i-payment_term_days, lp_ridge=lp_full)

cat(sprintf("Predicted delay: mean=%.1f | median=%.1f days\n\n",
            mean(full_data_scored$predicted_delay),
            median(full_data_scored$predicted_delay)))

#4. CUSTOMER BEHAVIOURAL BASELINES 
paid_history <- full_data_factored |>
  filter(event==1, !is.na(payment_date)) |>
  mutate(observed_delay = as.numeric(payment_date - due_date)) |>
  select(account, invoice_date, payment_date, observed_delay)

portfolio_mu    <- mean(paid_history$observed_delay, na.rm=TRUE)
portfolio_sigma <- max(sd(paid_history$observed_delay, na.rm=TRUE), 5)
portfolio_M     <- median(paid_history$observed_delay, na.rm=TRUE)
portfolio_Q_raw <- tryCatch(Qn(paid_history$observed_delay, finite.corr=FALSE),
                             error=function(e) IQR(paid_history$observed_delay)/1.349)
portfolio_Q     <- max(portfolio_Q_raw, 5)

customer_baseline <- full_data_factored |>
  select(invoice_id, account, invoice_date) |>
  pmap_dfr(function(invoice_id, account, invoice_date) {
    prior        <- filter(paid_history, account==!!account,
                            payment_date < !!invoice_date)
    n_prior      <- nrow(prior)
    prior_delays <- prior$observed_delay

    if (n_prior >= 2) {
      mu_c    <- mean(prior_delays)
      sigma_c <- max(sd(prior_delays), 5)
      M_c     <- median(prior_delays)
      Q_c     <- tryCatch(max(Qn(prior_delays, finite.corr=FALSE), 5),
                           error=function(e) max(IQR(prior_delays)/1.349, 5))
    } else if (n_prior == 1) {
      mu_c <- prior_delays[1]; sigma_c <- 5
      M_c  <- prior_delays[1]; Q_c     <- 5
    } else {
      mu_c <- NA_real_; sigma_c <- NA_real_
      M_c  <- NA_real_; Q_c     <- NA_real_
    }
    tibble(invoice_id=invoice_id, mu_c=mu_c, sigma_c=sigma_c,
           M_c=M_c, Q_c=Q_c, n_prior=n_prior)
  })

cat(sprintf("Baselines computed for %d invoices\n\n", nrow(customer_baseline)))

#5. Z-SCORES 

zscore_data <- full_data_scored |>
  left_join(customer_baseline |>
              select(invoice_id, mu_c, sigma_c, M_c, Q_c, n_prior),
            by="invoice_id") |>
  mutate(
    mu_c    = coalesce(mu_c,    portfolio_mu),
    sigma_c = coalesce(sigma_c, portfolio_sigma),
    M_c     = coalesce(M_c,     portfolio_M),
    Q_c     = coalesce(Q_c,     portfolio_Q),
    n_prior = coalesce(n_prior, 0L),
    z_traditional = (predicted_delay - mu_c) / sigma_c,
    z_robust      = (predicted_delay - M_c)  / Q_c,
    vol_traditional = abs(z_traditional),
    vol_robust      = abs(z_robust),
    tier_trad = case_when(vol_traditional < 1 ~ "Low",
                            vol_traditional < 2 ~ "Med", TRUE ~ "High"),
    tier_rob  = case_when(vol_robust      < 1 ~ "Low",
                            vol_robust      < 2 ~ "Med", TRUE ~ "High"),
    z_final    = z_robust,
    vol_final  = vol_robust,
    risk_final = tier_rob
  )

#6. FORMULATION COMPARISON 
comparison_df <- zscore_data |>
  filter(event==1, !is.na(days_overdue)) |>
  mutate(
    actually_late         = days_overdue > 0,
    pred_late_traditional = z_traditional > 0,
    pred_late_robust      = z_robust > 0,
    correct_trad          = (pred_late_traditional == actually_late),
    correct_robust        = (pred_late_robust == actually_late)
  )

acc_trad   <- mean(comparison_df$correct_trad,   na.rm=TRUE)
acc_robust <- mean(comparison_df$correct_robust, na.rm=TRUE)

cat(sprintf("Traditional accuracy : %.1f%%\n", acc_trad   * 100))
cat(sprintf("Robust accuracy      : %.1f%%\n", acc_robust * 100))
cat(sprintf("Improvement          : %+.1f pp\n\n",
            (acc_robust - acc_trad) * 100))
print(table(zscore_data$risk_final))
cat("\n")

# 7. SAVE 
saveRDS(zscore_data, "zscore_results.rds")
write_csv(
  zscore_data |> select(invoice_id, account, country, invoice_total,
                          payment_term_days, event, time_to_payment,
                          Y_i, predicted_delay, z_traditional, z_robust,
                          z_final, risk_final),
  "zscore_results.csv"
)
#  Prescriptive Optimisation Model
# File   : apolix_05_optimisation.R

# KEY PARAMETERS:
#   W_fte     = €35/hour
#   t_low     = 15 min (Low risk)
#   t_medium  = 30 min (Med risk)
#   t_high    = 45 min (High risk)
#   M_moderate = 2 hours/week 


#0. PACKAGES
library(tidyverse)
library(survival)
library(glmnet)
library(scales)

PLOT_DIR <- "thesis_plots"
dir.create(PLOT_DIR, showWarnings = FALSE)

theme_thesis <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey45"))

# 1. PARAMETERS 
W_fte      <- 35
t_low      <- 15
t_medium   <- 30
t_high     <- 45
M_moderate <- 2

cat(sprintf("W_fte=€%.0f/hr | Low=%dmin | Med=%dmin | High=%dmin\n\n",
            W_fte, t_low, t_medium, t_high))

#2. LOAD DATA 

zscore_data    <- readRDS("zscore_results.rds")
ridge_fit      <- readRDS("cox_ridge_fit.rds")
train_complete <- readRDS("train_complete.rds")
X_train        <- readRDS("X_train.rds")

cat(sprintf("Z-score data: %d invoices\n\n", nrow(zscore_data)))

#3. SURVIVAL PROBABILITY P_i 

cox_formula <- Surv(time_to_payment, event) ~
  log_invoice_total + payment_term_days +
  payment_risk_imp + payment_risk_miss +
  log_account_outstanding + hist_avg_delay +
  hist_delay_variance + hist_late_ratio +
  hist_invoice_freq + currency + country

currency_levels <- levels(train_complete$currency)
country_levels  <- levels(train_complete$country)

actionable <- zscore_data |>
  filter(predicted_delay > 0 | event == 0) |>
  mutate(currency = factor(currency, levels=currency_levels),
         country  = factor(country,  levels=country_levels)) |>
  filter(!is.na(currency), !is.na(country))

cat(sprintf("Actionable invoices: %d\n", nrow(actionable)))

X_action <- model.matrix(cox_formula, data=actionable)[, -1]
missing_cols <- setdiff(colnames(X_train), colnames(X_action))
for (col in missing_cols) {
  X_action <- cbind(X_action, matrix(0, nrow=nrow(X_action), ncol=1,
                                       dimnames=list(NULL, col)))
}
X_action <- X_action[, colnames(X_train), drop=FALSE]

lp_action <- as.vector(predict(ridge_fit, newx=X_action,
                                  s=ridge_fit$lambda, type="link"))

proxy_df  <- actionable |> mutate(ridge_lp=lp_action)
proxy_cox <- coxph(Surv(time_to_payment, event) ~ ridge_lp, data=proxy_df)
km_action <- survfit(proxy_cox, newdata=proxy_df)

P_i <- numeric(nrow(actionable))
for (j in seq_len(nrow(actionable))) {
  t_j    <- actionable$payment_term_days[j]
  idx    <- suppressWarnings(max(which(km_action$time <= t_j)))
  P_i[j] <- if (is.infinite(idx)) 1.0 else km_action$surv[idx, j]
}

actionable <- actionable |> mutate(P_i=P_i)
cat(sprintf("P_i: mean=%.3f | range %.3f-%.3f\n\n",
            mean(P_i), min(P_i), max(P_i)))

# 4. RISK-ADJUSTED EXPECTED LOSS R_i 
actionable <- actionable |>
  mutate(
    R_i     = invoice_total * P_i * (1 + vol_final),
    fcf_gap = (invoice_total * predicted_delay) / 365,
    t_i_minutes = case_when(
      risk_final == "High" ~ t_high,
      risk_final == "Med"  ~ t_medium,
      TRUE                 ~ t_low
    ),
    t_i_hours   = t_i_minutes / 60,
    K_i         = t_i_hours * W_fte,
    net_benefit = R_i - K_i,
    efficiency  = net_benefit / t_i_hours
  )

cat(sprintf("R_i: mean=€%.0f | total=€%s\n\n",
            mean(actionable$R_i),
            format(round(sum(actionable$R_i)), big.mark=",")))

# 5. GREEDY KNAPSACK 
knapsack <- function(items, capacity_hours) {
  sorted    <- items |> filter(net_benefit > 0) |> arrange(desc(efficiency))
  selected  <- character(0)
  total_R   <- 0; total_K <- 0; total_t <- 0; remaining <- capacity_hours

  for (i in seq_len(nrow(sorted))) {
    item <- sorted[i, ]
    if (item$t_i_hours <= remaining) {
      selected  <- c(selected, item$invoice_id)
      total_R   <- total_R   + item$R_i
      total_K   <- total_K   + item$K_i
      total_t   <- total_t   + item$t_i_hours
      remaining <- remaining - item$t_i_hours
    }
  }
  list(selected_ids=selected, n_selected=length(selected),
       total_R=total_R, total_K=total_K,
       net_value=total_R-total_K, hours_used=total_t,
       utilisation=total_t/capacity_hours)
}

#6. SCENARIO ANALYSIS 

scenarios <- tibble(
  scenario       = c("Tight (1h)", "Moderate (2h)", "Relaxed (4h)", "Uncapped"),
  capacity_hours = c(1, 2, 4, 999)
)

scenario_results <- scenarios |>
  rowwise() |>
  mutate(
    result      = list(knapsack(actionable, capacity_hours)),
    n_selected  = result$n_selected,
    total_R     = result$total_R,
    total_K     = result$total_K,
    net_value   = result$net_value,
    hours_used  = result$hours_used,
    utilisation = result$utilisation
  ) |>
  select(-result) |>
  ungroup()

print(scenario_results |>
  mutate(
    total_R   = paste0("€", format(round(total_R),   big.mark=",")),
    total_K   = paste0("€", format(round(total_K),   big.mark=",")),
    net_value = paste0("€", format(round(net_value), big.mark=",")),
    utilisation = paste0(round(utilisation*100,1), "%")
  ))

write_csv(scenario_results |> select(-utilisation), "optimisation_scenarios.csv")


# 7. BENCHMARK VS NAIVE 

naive_sorted <- actionable |>
  mutate(days_overdue_actual = pmax(time_to_payment - payment_term_days, 0)) |>
  arrange(desc(days_overdue_actual))

naive_selected <- character(0); naive_capacity <- M_moderate; naive_R <- 0
for (i in seq_len(nrow(naive_sorted))) {
  item <- naive_sorted[i, ]
  if (item$t_i_hours <= naive_capacity) {
    naive_selected <- c(naive_selected, item$invoice_id)
    naive_R        <- naive_R + item$R_i
    naive_capacity <- naive_capacity - item$t_i_hours
  }
}

risk_result <- knapsack(actionable, M_moderate)
improvement <- (risk_result$total_R - naive_R) / naive_R * 100



#8. FINAL INTERVENTION LIST 
final_list <- actionable |>
  mutate(selected=invoice_id %in% risk_result$selected_ids,
         priority_rank=rank(-efficiency, ties.method="first")) |>
  filter(selected) |>
  arrange(priority_rank)

cat(sprintf("Selected: %d invoices | R_i=€%s | Cost=€%s\n\n",
            nrow(final_list),
            format(round(sum(final_list$R_i)), big.mark=","),
            format(round(sum(final_list$K_i)), big.mark=",")))

# FROZEN SNAPSHOT 
frozen_date <- as.Date("2025-10-01")
df_raw      <- readRDS("apolix_model_ready.rds")

snapshot <- df_raw |>
  inner_join(actionable |> select(invoice_id, R_i, K_i, t_i_hours,
                                  net_benefit, efficiency, risk_final),
           by = "invoice_id") |>
  filter(invoice_date <= frozen_date,
         event == 0 | (event == 1 & !is.na(payment_date) &
                         payment_date > frozen_date))

# Model knapsack on snapshot
recommended_ids <- knapsack(snapshot, M_moderate)$selected_ids
recommended     <- snapshot |> filter(invoice_id %in% recommended_ids)

# Naive on snapshot
naive_sorted_frozen <- snapshot |>
  mutate(days_since_due = as.numeric(frozen_date - due_date)) |>
  arrange(desc(days_since_due))

naive_ids_frozen <- character(0); naive_cap <- M_moderate; naive_R_frozen <- 0
for (i in seq_len(nrow(naive_sorted_frozen))) {
  item <- naive_sorted_frozen[i, ]
  if (item$t_i_hours <= naive_cap) {
    naive_ids_frozen <- c(naive_ids_frozen, item$invoice_id)
    naive_R_frozen   <- naive_R_frozen + item$R_i
    naive_cap        <- naive_cap - item$t_i_hours
  }
}

#9. BENCHMARK FIGURE 
benchmark_frozen <- tibble(
  approach    = c("Naive Chronological", "Risk-Based (Model)"),
  R_protected = c(naive_R_frozen, sum(recommended$R_i))  
)

p2 <- benchmark_frozen |>
  ggplot(aes(x=approach, y=R_protected, fill=approach)) +
  geom_col(width=0.5) +
  geom_text(aes(label=paste0("€", format(round(R_protected), big.mark=","))),
            vjust=-0.5, size=4) +
  scale_fill_manual(values=c("#4C9BE8","#E07B54"), guide="none") +
  scale_y_continuous(labels=label_dollar(prefix="€", big.mark=","),
                     expand=expansion(mult=c(0,0.25))) +
  labs(x=NULL, y=expression(italic(R)[italic(i)]~"Protected (EUR)")) +
  theme_thesis

png(file.path(PLOT_DIR, "optimisation_benchmark_frozen.png"),
    width=1000, height=700, res=150)
print(p2)
dev.off()


#10. SAVE ALL OUTPUTS 
saveRDS(final_list, "intervention_list.rds")
saveRDS(actionable, "optimisation_full.rds")
write_csv(final_list, "intervention_list.csv")
write_csv(
  actionable |> select(invoice_id, account, country, invoice_total,
                          payment_term_days, predicted_delay, P_i,
                          vol_final, R_i, fcf_gap, K_i, net_benefit,
                          t_i_minutes, risk_final),
  "optimisation_full.csv"
)








