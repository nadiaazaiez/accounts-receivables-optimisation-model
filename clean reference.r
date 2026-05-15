# Data Cleaning
#  Censoring date = 2025-02-28
#  Entry No duplicates kept — same number reused across invoices in BC ERP
#  Rows where both name and customer ID are missing = ERP rows removed
#  open=0 with no payment date = data issue → removed (2,908 rows)
#  Non-numeric payment codes (AHL, NHL etc.) → derived from due - invoice date
#  Cash Account rows removed because point-of-sale transactions, not B2B credit
#  Pre-July 2023 open invoices removed 
#  Open invoices with time_to_payment > 500 days removed
#  exposure = closed_by_amount where available, else total_invoice_amount
#  AR0 invoices flagged, zero payment terms, kept but marked separately


#0. PACKAGES
library(tidyverse)
library(readxl)
library(lubridate)
# All open invoices are right-censored at this date 
CENSORING_DATE <- as.Date("2025-02-28")
cat("Censoring date:", as.character(CENSORING_DATE), "\n\n")


#1. Load
cat("── Loading data ──\n")

df_raw <- read_excel("/Users/nadiaazaiez/Downloads/external ia.xlsx")
cat(sprintf("Raw rows: %d | Columns: %d\n\n", nrow(df_raw), ncol(df_raw)))


#2. standardize column names
df <- df_raw |>
  rename_with(~ str_to_lower(str_replace_all(., " ", "_"))) |>
  rename_with(~ str_replace_all(., "#", "n"))

cat("Columns:\n")
print(names(df))
cat("\n")


#3. REMOVE ROWS WITH NO CUSTOMER IDENTITY
n0 <- nrow(df)
df <- df |>
  filter(!(is.na(name) | name == "") | !(is.na(no) | no == ""))
cat(sprintf("Removed rows with no customer name AND no ID: %d\n\n",
            n0 - nrow(df)))


# 4. REMOVE CASH ACCOUNT ROWS, No payment terms → cannot be used in survival model. 62 rows removed.
n0 <- nrow(df)
df <- df |>
  filter(!str_detect(tolower(name), "cash account"))
cat(sprintf("Removed Cash Account rows (non-B2B): %d\n\n", n0 - nrow(df)))


# 5. ASSIGN UNIQUE INVOICE IDs
df <- df |>
  mutate(
    invoice_id        = paste0("EXT_", row_number()),
    entry_no_original = entry_no
  )


# 6. PARSE DATES
df <- df |>
  mutate(
    invoice_date   = as.Date(document_date),
    due_date_clean = as.Date(due_date),
    payment_date   = as.Date(closed_at_date)
  )

# Business Central uses 1753-01-01 as a null date placeholder so removed
n0 <- nrow(df)
df <- df |>
  filter(is.na(payment_date)  | year(payment_date)  > 1800) |>
  filter(!is.na(invoice_date),   year(invoice_date)  > 1800)
cat(sprintf("Removed BC 1753 placeholder dates: %d rows\n\n",
            n0 - nrow(df)))


#7. EVENT INDICATOR 
# open = 0 with payment date → paid (event = 1)
# open = 1                  
# open = 0 without date      

df <- df |>
  mutate(
    event = case_when(
      open == 0 & !is.na(payment_date) ~ 1L,
      open == 1                         ~ 0L,
      open == 0 & is.na(payment_date)   ~ NA_integer_,
      TRUE                              ~ NA_integer_
    )
  )

cat(sprintf("  Paid     (event=1): %d\n", sum(df$event == 1, na.rm = TRUE)))
cat(sprintf("  Censored (event=0): %d\n", sum(df$event == 0, na.rm = TRUE)))
cat(sprintf("  Data issues (NA)  : %d\n", sum(is.na(df$event))))

n0 <- nrow(df)
df <- df |> filter(!is.na(event))
cat(sprintf("Removed data quality rows (open=0, no date): %d\n\n",
            n0 - nrow(df)))


# 8. PAYMENT TERM DAYS
# step 1: parse numeric days from payment_terms_code (AR10, AR30, AR0 etc.)and step 2: calculate from due_date - document_date (AHL, NHL, MUSG etc.)
# Remove only if both sources fail

df <- df |>
  mutate(
    pt_from_code  = as.integer(str_extract(payment_terms_code,
                                            "(?<=AR)\\d+|^\\d+$")),
    pt_from_dates = as.integer(due_date_clean - invoice_date),

    payment_term_days = case_when(
      !is.na(pt_from_code)  & pt_from_code  >= 0 ~ pt_from_code,
      !is.na(pt_from_dates) & pt_from_dates >= 0 ~ pt_from_dates,
      TRUE ~ NA_integer_
    ),

    pt_source = case_when(
      !is.na(pt_from_code)  & pt_from_code  >= 0 ~ "from_code",
      !is.na(pt_from_dates) & pt_from_dates >= 0 ~ "from_dates",
      TRUE ~ "missing"
    )
  )

cat("Payment term source breakdown:\n")
print(table(df$pt_source))

n0 <- nrow(df)
df <- df |> filter(pt_source != "missing")
cat(sprintf("\nRemoved rows with no derivable payment terms: %d\n\n",
            n0 - nrow(df)))


#9. SURVIVAL VARIABLES

df <- df |>
  mutate(
    
    time_to_payment = case_when(
      event == 1 ~ as.numeric(payment_date   - invoice_date),
      event == 0 ~ as.numeric(CENSORING_DATE - invoice_date)
    ),

    
    days_overdue = case_when(
      event == 1 & !is.na(payment_date) & !is.na(due_date_clean) ~
        as.numeric(payment_date - due_date_clean),
      TRUE ~ NA_real_
    )
  )

cat(sprintf("time_to_payment range: %.0f to %.0f days\n\n",
            min(df$time_to_payment, na.rm = TRUE),
            max(df$time_to_payment, na.rm = TRUE)))


#10. AR0 FLAG 
# AR0 = zero-day payment terms (payment due on receipt)
df <- df |>
  mutate(is_immediate_payment = payment_term_days == 0)

cat("AR0 vs credit breakdown:\n")
cat(sprintf("  AR0 (immediate) : %d (%.1f%%)\n",
            sum(df$is_immediate_payment),
            mean(df$is_immediate_payment) * 100))
cat(sprintf("  Credit (term>0) : %d (%.1f%%)\n",
            sum(!df$is_immediate_payment),
            mean(!df$is_immediate_payment) * 100))
cat("\n")


#11. CLEANING FILTERS 
n0 <- nrow(df)

df <- df |> filter(!is.na(total_invoice_amount), total_invoice_amount > 0)
cat(sprintf("After removing zero/missing amounts        : %d rows (-%d)\n",
            nrow(df), n0 - nrow(df))); n0 <- nrow(df)

df <- df |> filter(time_to_payment > 0)
cat(sprintf("After removing non-positive time           : %d rows (-%d)\n",
            nrow(df), n0 - nrow(df))); n0 <- nrow(df)

df <- df |> filter(is.na(payment_date) | payment_date >= invoice_date)
cat(sprintf("After removing payment before invoice date : %d rows (-%d)\n",
            nrow(df), n0 - nrow(df))); n0 <- nrow(df)

# Pre-July 2023 open invoices: only one month of data before this cutoff.
df <- df |>
  filter(!(event == 0 & invoice_date < as.Date("2023-07-01")))
cat(sprintf("After removing pre-July 2023 open invoices : %d rows (-%d)\n",
            nrow(df), n0 - nrow(df))); n0 <- nrow(df)

# Open invoices > 500 days: implausible under any standard payment terms and retaining them would artificially extend the censoring distribution.
df <- df |>
  filter(!(event == 0 & time_to_payment > 500))
cat(sprintf("After removing 500+ day open invoices      : %d rows (-%d)\n",
            nrow(df), n0 - nrow(df))); n0 <- nrow(df)


# 12. EXPOSURE VARIABLE
df <- df |>
  mutate(
    exposure = case_when(
      event == 1 & !is.na(closed_by_amount) & closed_by_amount > 0
        ~ closed_by_amount,
      TRUE ~ total_invoice_amount
    )
  )


# 13. FINAL SUMMARY
cat("\n── Final dataset ──\n")
cat(sprintf("Final rows        : %d\n",   nrow(df)))
cat(sprintf("Paid (event=1)    : %d (%.1f%%)\n",
            sum(df$event == 1), mean(df$event == 1) * 100))
cat(sprintf("Censored (event=0): %d (%.1f%%)\n",
            sum(df$event == 0), mean(df$event == 0) * 100))
cat(sprintf("Unique customers  : %d\n",   n_distinct(df$name)))
cat(sprintf("Date range        : %s to %s\n",
            min(df$invoice_date), max(df$invoice_date)))
cat(sprintf("Mean TTP (days)   : %.0f\n", mean(df$time_to_payment)))

paid_only <- df |> filter(event == 1, !is.na(days_overdue))
cat(sprintf("\nPayment behaviour (paid invoices only):\n"))
cat(sprintf("  Early (days_overdue < 0) : %d (%.1f%%)\n",
            sum(paid_only$days_overdue < 0),
            mean(paid_only$days_overdue < 0) * 100))
cat(sprintf("  On time (days_overdue=0) : %d (%.1f%%)\n",
            sum(paid_only$days_overdue == 0),
            mean(paid_only$days_overdue == 0) * 100))
cat(sprintf("  Late (days_overdue > 0)  : %d (%.1f%%)\n",
            sum(paid_only$days_overdue > 0),
            mean(paid_only$days_overdue > 0) * 100))

cat(sprintf("\nLate payment rate by term type:\n"))
df |>
  filter(event == 1, !is.na(days_overdue)) |>
  mutate(term_type = if_else(is_immediate_payment,
                              "AR0 (immediate)", "Credit (AR10/AR30+)")) |>
  group_by(term_type) |>
  summarise(
    n         = n(),
    pct_late  = round(mean(days_overdue > 0) * 100, 1),
    avg_delay = round(mean(days_overdue), 1),
    .groups   = "drop"
  ) |>
  print()


#14. SAVE
df_clean <- df |>
  select(
    invoice_id, entry_no_original, name, city,
    invoice_date, due_date_clean, payment_date,
    payment_terms_code, payment_term_days, pt_source,
    is_immediate_payment,
    total_invoice_amount, closed_by_amount, exposure,
    late_payments, amount_to_apply,
    event, time_to_payment, days_overdue
  ) |>
  rename(due_date = due_date_clean)

saveRDS(df_clean, "external_model_ready.rds")
write_csv(df_clean, "external_model_ready.csv")

cat(sprintf("  Final rows   : %d\n",  nrow(df_clean)))
cat(sprintf("  Paid         : %d (%.1f%%)\n",
            sum(df_clean$event==1), mean(df_clean$event==1)*100))
cat(sprintf("  Censored     : %d (%.1f%%)\n",
            sum(df_clean$event==0), mean(df_clean$event==0)*100))
cat(sprintf("  Customers    : %d\n",  n_distinct(df_clean$name)))
cat(sprintf("  Date range   : %s to %s\n",
            min(df_clean$invoice_date), max(df_clean$invoice_date)))
cat(sprintf("  Censoring    : %s\n",  CENSORING_DATE))


# External Validation — Survival Analysis (Cox + Ridge + GBSA)
# File   : external_02_survival.R

#0. PACKAGES
library(tidyverse)
library(survival)
library(survminer)
library(car)
library(glmnet)
library(gbm)
library(timeROC)
library(scales)
library(broom)

PLOT_DIR <- "external_plots"
dir.create(PLOT_DIR, showWarnings = FALSE)

theme_thesis <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey45"))


#1. LOAD DATA

df <- readRDS("external_model_ready.rds")
cat(sprintf("Loaded: %d rows | %d events (%.1f%%)\n\n",
            nrow(df), sum(df$event), mean(df$event)*100))


#2. BEHAVIOURAL FEATURES
# lag() ensures each invoice only sees prior invoices
df_sorted <- df |> arrange(name, invoice_date)

portfolio_avg_delay  <- mean(df$days_overdue, na.rm = TRUE)
portfolio_late_ratio <- mean(df$days_overdue > 0, na.rm = TRUE)

behavioural <- df_sorted |>
  group_by(name) |>
  mutate(
    delay_filled = coalesce(days_overdue, portfolio_avg_delay),
    late_filled  = coalesce(as.numeric(days_overdue > 0), portfolio_late_ratio),
    hist_avg_delay      = lag(cummean(delay_filled)),
    hist_delay_variance = lag(
      cummean(delay_filled^2) - cummean(delay_filled)^2
    ),
    hist_late_ratio = lag(cummean(late_filled)),
    hist_invoice_freq = lag(
      (row_number() - 1) /
        pmax(as.numeric(invoice_date - first(invoice_date)) / 30, 0.01)
    )
  ) |>
  ungroup() |>
  mutate(
    hist_avg_delay      = coalesce(hist_avg_delay,      portfolio_avg_delay),
    hist_delay_variance = coalesce(hist_delay_variance, 0),
    hist_late_ratio     = coalesce(hist_late_ratio,     portfolio_late_ratio),
    hist_invoice_freq   = coalesce(hist_invoice_freq,   0)
  ) |>
  select(invoice_id, hist_avg_delay, hist_delay_variance,
         hist_late_ratio, hist_invoice_freq)

df_prep <- df |>
  left_join(behavioural, by = "invoice_id") |>
  mutate(
    log_invoice_total    = log1p(total_invoice_amount),
    is_immediate_payment = as.integer(is_immediate_payment)
  )

cat(sprintf("Features done. NA check: %d\n\n",
            sum(is.na(df_prep |>
              select(log_invoice_total, payment_term_days,
                     is_immediate_payment, hist_avg_delay,
                     hist_delay_variance, hist_late_ratio,
                     hist_invoice_freq)))))


#3. TEMPORAL TRAIN/TEST SPLIT, 80th percentile of invoice_date
cutoff_date <- as.Date(quantile(as.numeric(df_prep$invoice_date), 0.80),
                        origin = "1970-01-01")
cat(sprintf("Train/test cutoff: %s\n", cutoff_date))

train <- df_prep |> filter(invoice_date <= cutoff_date)
test  <- df_prep |> filter(invoice_date >  cutoff_date)

cat(sprintf("Train: %d rows | %d events (%.1f%%)\n",
            nrow(train), sum(train$event), mean(train$event)*100))
cat(sprintf("Test : %d rows | %d events (%.1f%%)\n\n",
            nrow(test),  sum(test$event),  mean(test$event)*100))


# 4. MODEL FORMULA 
# log_amount_to_apply excluded: zero variance in training set
cox_formula <- Surv(time_to_payment, event) ~
  log_invoice_total +
  payment_term_days +
  is_immediate_payment +
  hist_avg_delay +
  hist_delay_variance +
  hist_late_ratio +
  hist_invoice_freq


#5. VIF

vif_model <- lm(time_to_payment ~
                  log_invoice_total + payment_term_days +
                  is_immediate_payment +
                  hist_avg_delay + hist_delay_variance +
                  hist_late_ratio + hist_invoice_freq,
                data = filter(train, event == 1))

vif_vals <- vif(vif_model)
print(round(vif_vals, 2))
if (any(vif_vals > 5)) {
  cat(sprintf("\n⚠ VIF > 5: %s\n\n",
              paste(names(vif_vals[vif_vals > 5]), collapse=", ")))
} else {
  cat("\n✓ All VIF < 5\n\n")
}

# 6. STANDARD COX MODEL
cox_fit <- coxph(cox_formula, data = train,
                  x = TRUE, y = TRUE, ties = "efron")
print(summary(cox_fit))

hr_table <- tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high, p.value) |>
  rename(HR = estimate, CI_low = conf.low, CI_high = conf.high) |>
  mutate(across(where(is.numeric), ~ round(., 4)))
print(hr_table)
write_csv(hr_table, "external_cox_hazard_ratios.csv")



#7. PH ASSUMPTION TEST
ph_test <- cox.zph(cox_fit)
print(ph_test)
cat(sprintf("\nVariables significant at p<0.05: %d/%d\n",
            sum(ph_test$table[-nrow(ph_test$table), "p"] < 0.05),
            nrow(ph_test$table) - 1))

png(file.path(PLOT_DIR, "external_ph_plots.png"),
    width = 1400, height = 1000, res = 150)
ggcoxzph(ph_test)
dev.off()



#8. RIDGE COX
X_train <- model.matrix(cox_formula, data = train)[, -1]
y_train <- Surv(train$time_to_payment, train$event)
X_test  <- model.matrix(cox_formula, data = test)[, -1]

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

get_cindex <- function(pred, data) {
  conc <- concordance(Surv(time_to_payment, event) ~ I(-pred), data=data)
  c(cindex = conc$concordance,
    ci_low  = conc$concordance - 1.96*sqrt(conc$var),
    ci_high = conc$concordance + 1.96*sqrt(conc$var))
}

cox_pred   <- predict(cox_fit,   newdata=test, type="lp")
ridge_pred <- as.vector(predict(ridge_fit, newx=X_test,
                                 s=ridge_fit$lambda, type="link"))

c_cox   <- get_cindex(cox_pred,   test)
c_ridge <- get_cindex(ridge_pred, test)

cat(sprintf("Standard Cox : %.4f (95%% CI: %.4f-%.4f)\n",
            c_cox["cindex"],   c_cox["ci_low"],   c_cox["ci_high"]))
cat(sprintf("Ridge Cox    : %.4f (95%% CI: %.4f-%.4f)\n\n",
            c_ridge["cindex"], c_ridge["ci_low"], c_ridge["ci_high"]))

# Train gaps
cindex_cox_train   <- concordance(Surv(time_to_payment,event) ~
  I(-predict(cox_fit, newdata=train, type="lp")), data=train)$concordance
cindex_ridge_train <- concordance(Surv(time_to_payment,event) ~
  I(-as.vector(predict(ridge_fit, newx=X_train,
     s=ridge_fit$lambda, type="link"))), data=train)$concordance

cat(sprintf("Train/test gaps:\n"))
cat(sprintf("Cox  : Train=%.4f | Test=%.4f | Gap=%.4f\n",
            cindex_cox_train, c_cox["cindex"],
            cindex_cox_train - c_cox["cindex"]))
cat(sprintf("Ridge: Train=%.4f | Test=%.4f | Gap=%.4f\n\n",
            cindex_ridge_train, c_ridge["cindex"],
            cindex_ridge_train - c_ridge["cindex"]))

# Time-dependent AUC
test_complete <- test |> filter(!is.na(time_to_payment), !is.na(event))
max_t         <- quantile(test_complete$time_to_payment, 0.95)
auc_times     <- c(30, 60, 90)[c(30, 60, 90) < max_t]

get_auc <- function(marker, data, times) {
  tryCatch({
    roc <- timeROC(T=data$time_to_payment, delta=data$event,
                   marker=marker, cause=1, weighting="marginal",
                   times=times, iid=FALSE)
    list(auc=roc$AUC, mean_auc=mean(roc$AUC, na.rm=TRUE))
  }, error=function(e) list(auc=rep(NA, length(times)), mean_auc=NA))
}

auc_cox   <- get_auc(cox_pred[test$invoice_id %in% test_complete$invoice_id],
                      test_complete, auc_times)
auc_ridge <- get_auc(ridge_pred[test$invoice_id %in% test_complete$invoice_id],
                      test_complete, auc_times)

cat(sprintf("Time-dependent AUC (t=30, t=60, t=90, mean):\n"))
cat(sprintf("Cox  : %.4f  %.4f  %.4f  %.4f\n",
            auc_cox$auc[1], auc_cox$auc[2], auc_cox$auc[3], auc_cox$mean_auc))
cat(sprintf("Ridge: %.4f  %.4f  %.4f  %.4f\n\n",
            auc_ridge$auc[1], auc_ridge$auc[2], auc_ridge$auc[3],
            auc_ridge$mean_auc))


#10. AR0 SENSITIVITY ANALYSIS
# Re-fit Cox on credit-only invoices to check if AR0 segment distorts results
cat("── AR0 Sensitivity Analysis ──\n\n")

train_credit <- train |> filter(payment_term_days > 0)
test_credit  <- test  |> filter(payment_term_days > 0)

cox_credit      <- coxph(cox_formula, data=train_credit,
                          x=TRUE, y=TRUE, ties="efron")
cox_pred_credit <- predict(cox_credit, newdata=test_credit, type="lp")
conc_credit     <- concordance(Surv(time_to_payment, event) ~
                     I(-cox_pred_credit), data=test_credit)

cat(sprintf("Full dataset Cox : %.3f (95%% CI: %.3f-%.3f)\n",
            c_cox["cindex"], c_cox["ci_low"], c_cox["ci_high"]))
cat(sprintf("Credit-only Cox  : %.3f\n", conc_credit$concordance))
cat(sprintf("Difference       : %.3f (negligible)\n",
            conc_credit$concordance - c_cox["cindex"]))
cat(sprintf("Credit-only n    : %d train | %d test\n\n",
            nrow(train_credit), nrow(test_credit)))


#11. SAVE 
saveRDS(cox_fit,   "external_cox_fit.rds")
saveRDS(ridge_fit, "external_ridge_fit.rds")
saveRDS(X_train,   "external_X_train.rds")
saveRDS(X_test,    "external_X_test.rds")
saveRDS(train,     "external_train.rds")
saveRDS(test,      "external_test.rds")


#12. GBSA
train_gbm <- as.data.frame(cbind(
  time_to_payment = train$time_to_payment,
  event           = train$event,
  X_train
))
test_gbm <- as.data.frame(cbind(
  time_to_payment = test$time_to_payment,
  event           = test$event,
  X_test
))

tune_grid_full <- expand.grid(
  n.trees           = c(500, 1000, 2000, 3000),
  interaction.depth = c(1, 2, 3, 4),
  shrinkage         = c(0.005, 0.01, 0.05),
  n.minobsinnode    = c(20, 30, 50)
)
cat(sprintf("Grid: %d combinations x 5 folds = %d fits\n\n",
            nrow(tune_grid_full), nrow(tune_grid_full) * 5))

n_train   <- nrow(train_gbm)
n_folds   <- 5
fold_size <- floor(n_train / (n_folds + 1))
cv_results_full <- list()
start_time <- Sys.time()

for (i in seq_len(nrow(tune_grid_full))) {
  params      <- tune_grid_full[i, ]
  fold_cindex <- rep(NA_real_, n_folds)

  for (f in seq_len(n_folds)) {
    train_end <- fold_size * f
    val_start <- train_end + 1
    val_end   <- min(train_end + fold_size, n_train)

    cv_train <- train_gbm[1:train_end, ]
    cv_val   <- train_gbm[val_start:val_end, ]

    if (sum(cv_val$event) == 0 || nrow(cv_train) < 100) next

    tryCatch({
      fit_cv <- gbm(
        formula           = Surv(time_to_payment, event) ~ .,
        data              = cv_train,
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
      fold_cindex[f] <- concordance(
        Surv(time_to_payment, event) ~ I(-pred_cv),
        data=cv_val)$concordance
    }, error=function(e) { fold_cindex[f] <<- NA })
  }

  valid_folds <- sum(!is.na(fold_cindex))
  cv_results_full[[i]] <- data.frame(
    n.trees=params$n.trees, interaction.depth=params$interaction.depth,
    shrinkage=params$shrinkage, n.minobsinnode=params$n.minobsinnode,
    mean_cindex=mean(fold_cindex, na.rm=TRUE),
    sd_cindex=sd(fold_cindex, na.rm=TRUE),
    valid_folds=valid_folds
  )

  
  if (i %% 12 == 0) {
    elapsed <- round(difftime(Sys.time(), start_time, units="mins"), 1)
    cat(sprintf("  %d/%d done | %.1f mins elapsed\n",
                i, nrow(tune_grid_full), elapsed))
    write_csv(bind_rows(cv_results_full[!sapply(cv_results_full, is.null)]),
              "external_gbsa_cv_intermediate.csv")
  }
}

tuning_full <- bind_rows(cv_results_full) |>
  filter(valid_folds >= 3) |>
  arrange(desc(mean_cindex))

cat("\nTop 10 combinations:\n")
print(head(tuning_full, 10))
write_csv(tuning_full, "external_gbsa_cv_full.csv")

best_cv <- tuning_full[1, ]
cat(sprintf("\nCV Best: trees=%d | depth=%d | shrink=%.3f | minobs=%d\n",
            best_cv$n.trees, best_cv$interaction.depth,
            best_cv$shrinkage, best_cv$n.minobsinnode))
cat(sprintf("CV C-Index: %.4f (SD: %.4f)\n\n",
            best_cv$mean_cindex, best_cv$sd_cindex))


#13. REFIT FINAL GBSA WITH CV-SELECTED PARAMS
set.seed(42)
gbsa_fit_cv <- gbm(
  formula           = Surv(time_to_payment, event) ~ .,
  data              = train_gbm,
  distribution      = "coxph",
  n.trees           = best_cv$n.trees,
  interaction.depth = best_cv$interaction.depth,
  shrinkage         = best_cv$shrinkage,
  n.minobsinnode    = best_cv$n.minobsinnode,
  bag.fraction      = 0.7,
  verbose           = FALSE
)

gbsa_pred_cv <- predict(gbsa_fit_cv, newdata=test_gbm,
                         n.trees=best_cv$n.trees, type="link")
c_gbsa_cv    <- get_cindex(gbsa_pred_cv, test)

# Seed stability check confirms results don't depend on random initialisation
set.seed(999)
gbsa_check <- gbm(
  formula           = Surv(time_to_payment, event) ~ .,
  data              = train_gbm,
  distribution      = "coxph",
  n.trees           = best_cv$n.trees,
  interaction.depth = best_cv$interaction.depth,
  shrinkage         = best_cv$shrinkage,
  n.minobsinnode    = best_cv$n.minobsinnode,
  bag.fraction      = 0.7,
  verbose           = FALSE
)
seed_stability <- cor(
  gbsa_pred_cv,
  predict(gbsa_check, newdata=test_gbm,
          n.trees=best_cv$n.trees, type="link"),
  method="spearman"
)

cat(sprintf("CV-tuned GBSA C-Index : %.4f (95%% CI: %.4f-%.4f)\n",
            c_gbsa_cv["cindex"], c_gbsa_cv["ci_low"], c_gbsa_cv["ci_high"]))
cat(sprintf("Seed stability        : %.4f\n\n", seed_stability))

# AUC for CV-tuned GBSA
auc_gbsa <- get_auc(
  gbsa_pred_cv[test$invoice_id %in% test_complete$invoice_id],
  test_complete, auc_times)
cat(sprintf("GBSA AUC: t30=%.4f | t60=%.4f | t90=%.4f | mean=%.4f\n\n",
            auc_gbsa$auc[1], auc_gbsa$auc[2], auc_gbsa$auc[3],
            auc_gbsa$mean_auc))

# Train gap
cindex_gbsa_train <- concordance(Surv(time_to_payment, event) ~
  I(-predict(gbsa_fit_cv, newdata=train_gbm,
             n.trees=best_cv$n.trees, type="link")),
  data=train)$concordance
cat(sprintf("GBSA train gap: %.4f\n\n",
            cindex_gbsa_train - c_gbsa_cv["cindex"]))


#14. VARIABLE IMPORTANCE 

imp <- summary(gbsa_fit_cv, n.trees=best_cv$n.trees, plotit=FALSE) |>
  as_tibble() |>
  rename(variable=var, importance=rel.inf) |>
  arrange(desc(importance))
print(imp, n=10)
write_csv(imp, "external_gbsa_importance.csv")



#15. SAVE ALL OUTPUTS
saveRDS(gbsa_fit_cv, "external_gbsa_cv_fit.rds")

results <- tibble(
  model     = c("Standard Cox", "Ridge Cox", "GBSA"),
  cindex    = c(c_cox["cindex"],    c_ridge["cindex"],    c_gbsa_cv["cindex"]),
  ci_low    = c(c_cox["ci_low"],    c_ridge["ci_low"],    c_gbsa_cv["ci_low"]),
  ci_high   = c(c_cox["ci_high"],   c_ridge["ci_high"],   c_gbsa_cv["ci_high"]),
  mean_auc  = c(auc_cox$mean_auc,   auc_ridge$mean_auc,   auc_gbsa$mean_auc),
  train_gap = c(cindex_cox_train   - c_cox["cindex"],
                cindex_ridge_train - c_ridge["cindex"],
                cindex_gbsa_train  - c_gbsa_cv["cindex"])
) |> mutate(across(where(is.numeric), ~ round(., 4)))

write_csv(results, "external_model_results.csv")

cat(sprintf("Train: %d | Test: %d\n",         nrow(train), nrow(test)))
cat(sprintf("Standard Cox C-Index : %.4f\n",  c_cox["cindex"]))
cat(sprintf("Ridge Cox C-Index    : %.4f\n",  c_ridge["cindex"]))
cat(sprintf("GBSA C-Index (CV)    : %.4f\n",  c_gbsa_cv["cindex"]))
cat(sprintf("GBSA Mean AUC        : %.4f\n",  auc_gbsa$mean_auc))
cat(sprintf("Seed stability       : %.4f\n",  seed_stability))


# Prescriptive Optimisation Pipeline
# File   : external_03_prescriptive.R
#   4 FTE × 40h = 160h/week total capacity
#   Automated dunning: 12h/week fixed → excluded from knapsack
#   Personalised knapsack capacity: 148h/week
#   Staff cost: €25/hour
#   Med risk → 15 min per intervention
#   High risk → 60 min per intervention
#   Low risk → automated dunning only (not in knapsack)
#   - GBSA CV-tuned model used for survival predictions
#   - RMST horizon L = 90th percentile (102 days)Changed from 75th (55d) to 90th based on sensitivity analysis because bias reduces from 10.1d to 4.4d while curve is still estimable

#0. PACKAGES
library(tidyverse)
library(survival)
library(gbm)
library(glmnet)
library(scales)

PLOT_DIR <- "external_plots"
dir.create(PLOT_DIR, showWarnings = FALSE)

theme_p <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey45"))

set.seed(42)


#1. PARAMETERS
W_fte           <- 25
hours_automated <- 12
M_knapsack      <- 148
t_med_hours     <- 15 / 60
t_high_hours    <- 60 / 60

cat(sprintf("Capacity (personalised) : %.0f hrs/week\n", M_knapsack))
cat(sprintf("Staff cost              : EUR%.0f/hr\n", W_fte))
cat(sprintf("Med  : %.0f min = EUR%.2f | High: %.0f min = EUR%.2f\n\n",
            t_med_hours*60,  t_med_hours*W_fte,
            t_high_hours*60, t_high_hours*W_fte))


# 2. LOAD DATA 
df <- readRDS("external_model_ready.rds")
cat(sprintf("Loaded: %d invoices | %d customers | %.1f%% paid\n\n",
            nrow(df), n_distinct(df$name), mean(df$event) * 100))


#3. BEHAVIOURAL FEATURES


port_avg_delay  <- mean(df$days_overdue,     na.rm = TRUE)
port_late_ratio <- mean(df$days_overdue > 0, na.rm = TRUE)
port_variance   <- var(df$days_overdue,      na.rm = TRUE)

df_sorted <- df |>
  arrange(name, invoice_date) |>
  group_by(name) |>
  mutate(
    delay_obs = coalesce(days_overdue, port_avg_delay),
    late_obs  = coalesce(as.numeric(days_overdue > 0), port_late_ratio),
    hist_avg_delay      = lag(cummean(delay_obs)),
    hist_delay_variance = pmax(lag(cummean(delay_obs^2)) -
                                 lag(cummean(delay_obs))^2, 0),
    hist_late_ratio     = lag(cummean(late_obs)),
    hist_invoice_freq   = lag(
      (row_number() - 1) /
        pmax(as.numeric(invoice_date - first(invoice_date)) / 30, 0.01)
    )
  ) |>
  ungroup() |>
  mutate(
    hist_avg_delay       = coalesce(hist_avg_delay,      port_avg_delay),
    hist_delay_variance  = coalesce(hist_delay_variance, port_variance),
    hist_late_ratio      = coalesce(hist_late_ratio,     port_late_ratio),
    hist_invoice_freq    = coalesce(hist_invoice_freq,   0),
    log_invoice_total    = log1p(total_invoice_amount),
    is_immediate_payment = as.integer(payment_term_days == 0)
  )

#4. TEMPORAL SPLIT
cutoff_date <- as.Date(quantile(as.numeric(df_sorted$invoice_date), 0.80),
                        origin = "1970-01-01")
cat(sprintf("Train/test cutoff: %s\n", cutoff_date))

train <- df_sorted |> filter(invoice_date <= cutoff_date)
test  <- df_sorted |> filter(invoice_date >  cutoff_date)

cat(sprintf("Train: %d rows (%.1f%% paid) | Test: %d rows (%.1f%% paid)\n\n",
            nrow(train), mean(train$event)*100,
            nrow(test),  mean(test$event)*100))


#5. FEATURE MATRICES 
cox_formula <- Surv(time_to_payment, event) ~
  log_invoice_total + payment_term_days + is_immediate_payment +
  hist_avg_delay + hist_delay_variance + hist_late_ratio + hist_invoice_freq

X_train <- model.matrix(cox_formula, data = train)[, -1]
y_train <- Surv(train$time_to_payment, train$event)
X_test  <- model.matrix(cox_formula, data = test)[, -1]
X_full  <- model.matrix(cox_formula, data = df_sorted)[, -1]

align_cols <- function(X, ref_cols) {
  missing <- setdiff(ref_cols, colnames(X))
  for (col in missing) {
    X <- cbind(X, matrix(0, nrow=nrow(X), ncol=1,
                           dimnames=list(NULL, col)))
  }
  X[, ref_cols, drop=FALSE]
}

X_test <- align_cols(X_test, colnames(X_train))
X_full <- align_cols(X_full, colnames(X_train))

train_gbm <- as.data.frame(cbind(time_to_payment=train$time_to_payment,
                                   event=train$event, X_train))
test_gbm  <- as.data.frame(cbind(time_to_payment=test$time_to_payment,
                                   event=test$event,  X_test))
full_gbm  <- as.data.frame(cbind(time_to_payment=df_sorted$time_to_payment,
                                   event=df_sorted$event, X_full))


#6. LOAD GBSA

if (!file.exists("external_gbsa_cv_fit.rds")) {
  stop("external_gbsa_cv_fit.rds not found. Run external_02_survival.R first.")
}

gbsa_fit    <- readRDS("external_gbsa_cv_fit.rds")
tuning_full <- read_csv("external_gbsa_cv_full.csv", show_col_types=FALSE)
best_params <- tuning_full[1, ]

cat(sprintf("CV-best: trees=%d | depth=%d | shrink=%.3f | minobs=%d | C=%.4f\n\n",
            best_params$n.trees, best_params$interaction.depth,
            best_params$shrinkage, best_params$n.minobsinnode,
            best_params$mean_cindex))

gbsa_pred_test <- predict(gbsa_fit, newdata=test_gbm,
                           n.trees=best_params$n.trees, type="link")
c_gbsa_test <- concordance(Surv(time_to_payment,event) ~
                I(-gbsa_pred_test), data=test)$concordance
cat(sprintf("GBSA test C-Index: %.4f\n\n", c_gbsa_test))


# 7. RMST EXPECTED PAYMENT TIME


lp_full   <- predict(gbsa_fit, newdata=full_gbm,
                      n.trees=best_params$n.trees, type="link")
proxy_df  <- df_sorted |> mutate(gbsa_lp = lp_full)
proxy_cox <- coxph(Surv(time_to_payment, event) ~ gbsa_lp, data=proxy_df)
km_full   <- survfit(proxy_cox, newdata=proxy_df)

L <- quantile(df_sorted$time_to_payment, 0.90, na.rm=TRUE)
cat(sprintf("RMST horizon L = %.1f days (90th percentile)\n", L))

compute_rmst <- function(surv_mat, times, L) {
  n_inv  <- ncol(surv_mat)
  rmst   <- numeric(n_inv)
  t_grid <- c(0, times[times <= L])
  for (j in seq_len(n_inv)) {
    s <- c(1, surv_mat[times <= L, j])
    rmst[j] <- sum(diff(t_grid) * (head(s,-1) + tail(s,-1)) / 2)
  }
  rmst
}

Y_i <- compute_rmst(km_full$surv, km_full$time, L)
cat(sprintf("Y_i: mean=%.1f | median=%.1f | range %.1f-%.1f days\n\n",
            mean(Y_i), median(Y_i), min(Y_i), max(Y_i)))

df_scored <- df_sorted |>
  mutate(Y_i=Y_i, predicted_delay=Y_i-payment_term_days, gbsa_lp=lp_full)


# 8. CUSTOMER BEHAVIOURAL BASELINES 
port_mu    <- port_avg_delay
port_sigma <- max(sqrt(port_variance), 5)

baseline_full <- df_scored |>
  arrange(name, invoice_date) |>
  group_by(name) |>
  mutate(
    is_valid    = as.integer(event == 1 & !is.na(days_overdue)),
    n_prior     = lag(cumsum(is_valid), default=0),
    sum_d       = lag(cumsum(if_else(is_valid==1, days_overdue, 0)), default=0),
    sum_d2      = lag(cumsum(if_else(is_valid==1, days_overdue^2, 0)), default=0),
    mu_c_raw    = if_else(n_prior >= 1, sum_d/n_prior, NA_real_),
    var_c_raw   = if_else(n_prior >= 2,
                          (sum_d2/n_prior) - (sum_d/n_prior)^2, NA_real_),
    sigma_c_raw = sqrt(pmax(coalesce(var_c_raw, 0), 0)),
    mu_c        = coalesce(mu_c_raw,    port_mu),
    sigma_c     = pmax(coalesce(sigma_c_raw, port_sigma), 5),
    M_c = mu_c, Q_c = sigma_c
  ) |>
  ungroup() |>
  select(invoice_id, mu_c, sigma_c, M_c, Q_c, n_prior)

cat(sprintf("Baselines done: %s invoices\n\n",
            format(nrow(baseline_full), big.mark=",")))


#Z-SCORES

zscore_data <- df_scored |>
  left_join(baseline_full |> select(invoice_id, mu_c, sigma_c, M_c, Q_c, n_prior),
            by="invoice_id") |>
  mutate(
    mu_c    = coalesce(mu_c,    port_mu),
    sigma_c = coalesce(sigma_c, port_sigma),
    M_c     = coalesce(M_c,     port_mu),
    Q_c     = coalesce(Q_c,     port_sigma),
    n_prior = coalesce(n_prior, 0L),
    z_traditional   = (predicted_delay - mu_c) / sigma_c,
    z_robust        = (predicted_delay - M_c)  / Q_c,
    vol_traditional = abs(z_traditional),
    vol_robust      = abs(z_robust),
    tier_trad = case_when(vol_traditional < 1 ~ "Low",
                            vol_traditional < 2 ~ "Med", TRUE ~ "High"),
    tier_rob  = case_when(vol_robust      < 1 ~ "Low",
                            vol_robust      < 2 ~ "Med", TRUE ~ "High")
  )

cat("  Traditional:\n"); print(table(zscore_data$tier_trad))
cat("  Robust:\n");      print(table(zscore_data$tier_rob))
cat("\n")


#10. FORMULATION COMPARISON

cmp <- zscore_data |>
  filter(event==1, !is.na(days_overdue)) |>
  mutate(
    actually_late = days_overdue > 0,
    correct_trad  = ((z_traditional > 0) == actually_late),
    correct_rob   = ((z_robust      > 0) == actually_late)
  )

acc_trad   <- mean(cmp$correct_trad,  na.rm=TRUE)
acc_robust <- mean(cmp$correct_rob,   na.rm=TRUE)

cat(sprintf("Traditional accuracy : %.1f%%\n", acc_trad   * 100))
cat(sprintf("Robust accuracy      : %.1f%%\n", acc_robust * 100))
cat(sprintf("Winner               : %s (+%.1f pp)\n\n",
            ifelse(acc_robust >= acc_trad, "Robust", "Traditional"),
            abs(acc_robust - acc_trad) * 100))

selected_z <- if (acc_robust >= acc_trad) "robust" else "traditional"

zscore_data <- zscore_data |>
  mutate(
    z_final    = if (selected_z=="robust") z_robust else z_traditional,
    vol_final  = abs(z_final),
    risk_final = if (selected_z=="robust") tier_rob  else tier_trad
  )


#11. SURVIVAL PROBABILITY 

actionable <- zscore_data |> filter(predicted_delay > 0 | event == 0)
cat(sprintf("Actionable: %d / %d invoices\n", nrow(actionable), nrow(zscore_data)))

beta_lp     <- coef(proxy_cox)["gbsa_lp"]
km_baseline <- survfit(proxy_cox, newdata=data.frame(gbsa_lp=0))

get_s0 <- function(t, km) {
  idx <- suppressWarnings(max(which(km$time <= t)))
  if (is.infinite(idx)) 1.0 else as.numeric(km$surv)[idx]
}

unique_terms <- sort(unique(actionable$payment_term_days))
s0_lookup <- tibble(
  payment_term_days = unique_terms,
  s0 = sapply(unique_terms, get_s0, km=km_baseline)
)

X_action <- model.matrix(cox_formula, data=actionable)[, -1]
X_action <- align_cols(X_action, colnames(X_train))
action_gbm <- as.data.frame(cbind(
  time_to_payment=actionable$time_to_payment,
  event=actionable$event, X_action))

lp_action <- predict(gbsa_fit, newdata=action_gbm,
                      n.trees=best_params$n.trees, type="link")

actionable <- actionable |>
  mutate(lp_action_vec=lp_action) |>
  left_join(s0_lookup, by="payment_term_days") |>
  mutate(
    s0  = coalesce(s0, 1.0),
    P_i = pmin(pmax(s0^exp(beta_lp * lp_action_vec), 0), 1)
  )

cat(sprintf("P_i: mean=%.3f | range %.3f-%.3f\n\n",
            mean(actionable$P_i), min(actionable$P_i), max(actionable$P_i)))


#12. R_i AND INTERVENTION COSTS
actionable <- actionable |>
  mutate(
    R_i         = total_invoice_amount * P_i * (1 + vol_final),
    fcf_gap     = (total_invoice_amount * predicted_delay) / 365,
    in_knapsack = risk_final %in% c("Med","High"),
    t_i_hours   = case_when(risk_final=="High" ~ t_high_hours,
                             risk_final=="Med"  ~ t_med_hours,
                             TRUE               ~ NA_real_),
    K_i         = t_i_hours * W_fte,
    net_benefit = R_i - K_i,
    efficiency  = net_benefit / t_i_hours
  )

cat(sprintf("R_i: mean=EUR%.0f | total=EUR%s\n",
            mean(actionable$R_i),
            format(round(sum(actionable$R_i)), big.mark=",")))
cat(sprintf("Knapsack eligible: %d Med + %d High\n\n",
            sum(actionable$risk_final=="Med"),
            sum(actionable$risk_final=="High")))


#13. GREEDY KNAPSACK 
knapsack <- function(items, capacity_hours) {
  eligible <- items |>
    filter(in_knapsack, !is.na(net_benefit), net_benefit > 0) |>
    arrange(desc(efficiency))
  selected <- character(0); total_R <- 0; total_K <- 0
  total_t  <- 0; remaining <- capacity_hours
  for (i in seq_len(nrow(eligible))) {
    item <- eligible[i,]
    if (item$t_i_hours <= remaining) {
      selected  <- c(selected, item$invoice_id)
      total_R   <- total_R  + item$R_i
      total_K   <- total_K  + item$K_i
      total_t   <- total_t  + item$t_i_hours
      remaining <- remaining - item$t_i_hours
    }
  }
  list(selected_ids=selected, n_selected=length(selected),
       total_R=total_R, total_K=total_K,
       net_value=total_R-total_K, hours_used=total_t,
       utilisation=total_t/capacity_hours)
}


#14. SCENARIO ANALYSIS

scenarios <- tibble(
  scenario       = c("Tight (37h)", "Moderate (74h)",
                     "Full (148h)", "Uncapped"),
  capacity_hours = c(M_knapsack*0.25, M_knapsack*0.50, M_knapsack, 999999)
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
    total_R   = paste0("EUR", format(round(total_R/1e6, 2), nsmall=2), "M"),
    net_value = paste0("EUR", format(round(net_value/1e6, 2), nsmall=2), "M"),
    total_K   = paste0("EUR", format(round(total_K), big.mark=",")),
    utilisation = paste0(round(utilisation*100,1), "%")
  ))

write_csv(scenario_results |> select(-utilisation), "external_scenario_results.csv")
cat("Saved: external_scenario_results.csv\n\n")


#15. BENCHMARK 

naive_sorted <- actionable |>
  filter(in_knapsack) |>
  mutate(age = pmax(time_to_payment - payment_term_days, 0)) |>
  arrange(desc(age))

naive_R <- 0; naive_cap <- M_knapsack; naive_n <- 0
for (i in seq_len(nrow(naive_sorted))) {
  item <- naive_sorted[i,]
  if (item$t_i_hours <= naive_cap) {
    naive_R   <- naive_R   + item$R_i
    naive_cap <- naive_cap - item$t_i_hours
    naive_n   <- naive_n   + 1
  }
}

risk_result <- knapsack(actionable, M_knapsack)
improvement <- (risk_result$total_R - naive_R) / naive_R * 100

cat(sprintf("Naive  : n=%d | EUR%sM\n",
            naive_n, format(round(naive_R/1e6, 2), nsmall=2)))
cat(sprintf("Model  : n=%d | EUR%sM\n",
            risk_result$n_selected,
            format(round(risk_result$total_R/1e6, 2), nsmall=2)))
cat(sprintf("Improvement: +%.1f%%\n\n", improvement))


#16. DIRECTIONAL ACCURACY

paid_check <- actionable |>
  filter(event==1, !is.na(days_overdue)) |>
  mutate(
    correct_direction = case_when(
      predicted_delay > 0 & days_overdue > 0  ~ TRUE,
      predicted_delay > 0 & days_overdue <= 0 ~ FALSE,
      TRUE ~ NA
    )
  )

cat(sprintf("Correctly predicted late : %d (%.1f%%)\n",
            sum(paid_check$correct_direction==TRUE,  na.rm=TRUE),
            mean(paid_check$correct_direction==TRUE,  na.rm=TRUE)*100))
cat(sprintf("Incorrectly predicted    : %d (%.1f%%)\n",
            sum(paid_check$correct_direction==FALSE, na.rm=TRUE),
            mean(paid_check$correct_direction==FALSE, na.rm=TRUE)*100))


#17. SAVE
saveRDS(zscore_data, "external_zscore_results.rds")
saveRDS(actionable,  "external_optimisation_full.rds")

write_csv(
  zscore_data |> select(invoice_id, name, city, invoice_date, due_date,
                          total_invoice_amount, payment_term_days, event,
                          time_to_payment, predicted_delay,
                          z_traditional, z_robust, z_final, risk_final),
  "external_zscore_results.csv"
)
write_csv(scenario_results |> select(-utilisation),
          "external_scenario_results.csv")


cat(sprintf("  Full portfolio scored  : %s\n",
            format(nrow(zscore_data), big.mark=",")))
cat(sprintf("  RMST horizon L        : %.1f days\n", L))
cat(sprintf("  Z-score formulation   : %s\n", selected_z))
cat(sprintf("  Directional accuracy  : %.1f%%\n",
            mean(paid_check$correct_direction==TRUE, na.rm=TRUE)*100))
cat(sprintf("  vs Naive improvement  : +%.1f%%\n", improvement))

#Horizon Sensitivity Analysis
# File   : external_frozen_time_demo.R

#0. PACKAGES 
library(tidyverse)
library(scales)

PLOT_DIR <- "external_plots"
dir.create(PLOT_DIR, showWarnings = FALSE)

theme_p <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey45"),
        panel.grid.minor = element_blank())


# 1. PARAMETERS 
frozen_date  <- as.Date("2024-10-10")
W_fte        <- 25
t_med_hours  <- 15 / 60
t_high_hours <- 60 / 60
M_moderate   <- 74  

cat(sprintf("Frozen date : %s\n",   frozen_date))
cat(sprintf("Capacity    : %.0f hours (moderate scenario)\n\n", M_moderate))


#2. LOAD 
cat("── Loading model outputs ──\n")

actionable <- readRDS("external_optimisation_full.rds")
df_clean   <- readRDS("external_model_ready.rds")


cat(sprintf("Actionable invoices loaded: %s\n\n",
            format(nrow(actionable), big.mark=",")))


#3. SNAPSHOT AT FROZEN DATE 
snap_ks <- actionable |>
  filter(
    invoice_date <= frozen_date,
    event == 0 | (event == 1 & !is.na(payment_date) &
                    payment_date > frozen_date)
  ) |>
  mutate(
    t_i_hours   = case_when(risk_final == "High" ~ t_high_hours,
                             risk_final == "Med"  ~ t_med_hours,
                             TRUE ~ NA_real_),
    K_i_snap    = t_i_hours * W_fte,
    net_benefit = R_i - K_i_snap,
    efficiency  = net_benefit / t_i_hours,
    in_knapsack = risk_final %in% c("Med","High")
  )

cat(sprintf("Invoices unpaid at %s : %s | total value EUR%s\n\n",
            frozen_date,
            format(nrow(snap_ks), big.mark=","),
            format(round(sum(snap_ks$total_invoice_amount)), big.mark=",")))


#4. GREEDY KNAPSACK FUNCTION 
run_knapsack <- function(items, capacity) {
  eligible <- items |>
    filter(in_knapsack, !is.na(net_benefit), net_benefit > 0) |>
    arrange(desc(efficiency))
  selected  <- character(0)
  remaining <- capacity
  for (i in seq_len(nrow(eligible))) {
    item <- eligible[i,]
    if (item$t_i_hours <= remaining) {
      selected  <- c(selected, item$invoice_id)
      remaining <- remaining - item$t_i_hours
    }
  }
  list(
    n           = length(selected),
    total_R     = sum(items$R_i[items$invoice_id %in% selected]),
    total_value = sum(items$total_invoice_amount[
                        items$invoice_id %in% selected])
  )
}


# 5. HORIZON SENSITIVITY ANALYSIS 
cat(sprintf("%-15s %8s %15s %15s\n",
            "Horizon", "n selected", "Ri protected", "Invoice value"))
cat(paste(rep("-", 57), collapse=""), "\n")

# No filter
r0 <- run_knapsack(snap_ks, M_moderate)
cat(sprintf("%-15s %8d %15s %15s\n", "No filter", r0$n,
            paste0("EUR", format(round(r0$total_R),     big.mark=",")),
            paste0("EUR", format(round(r0$total_value), big.mark=","))))

# Horizon windows
for (h in c(60, 30, 14, 7)) {
  snap_h <- snap_ks |> filter(predicted_delay <= h)
  r      <- run_knapsack(snap_h, M_moderate)
  cat(sprintf("%-15s %8d %15s %15s\n",
              paste0("<=", h, " days"), r$n,
              paste0("EUR", format(round(r$total_R),     big.mark=",")),
              paste0("EUR", format(round(r$total_value), big.mark=","))))
}


# 6 HORIZON SENSITIVITY BAR CHART 
# Hardcoded values from verified R output 
horizon_results <- tibble(
  Horizon = c("No filter", "<=60 days", "<=30 days", "<=14 days", "<=7 days"),
  Ri      = c(4840533, 4777086, 1757390, 1512146, 684894)
) |>
  mutate(Horizon = factor(Horizon,
                           levels = c("No filter", "<=60 days", "<=30 days",
                                      "<=14 days", "<=7 days")))

p_horizon <- ggplot(horizon_results, aes(x=Horizon, y=Ri)) +
  geom_col(fill="#4C9BE8", width=0.6) +
  geom_text(aes(label=paste0("EUR", format(round(Ri/1e6, 2), nsmall=2), "M")),
            vjust=-0.4, size=3.5) +
  scale_y_continuous(
    labels = label_dollar(prefix="EUR", big.mark=","),
    expand = expansion(mult=c(0, 0.15))) +
  labs(x = "Predictive Horizon Filter",
       y = expression(italic(R)[italic(i)]~"Protected (EUR)"),
       title = NULL, subtitle = NULL) +
  theme_p

png(file.path(PLOT_DIR, "external_horizon_sensitivity.png"),
    width=900, height=600, res=150)
print(p_horizon)
dev.off()
