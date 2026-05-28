
table(analysis_data_complete$gender)

# ============================================================
# 05b_xgboost_count_tuned.R
# XGBoost tuned model: chronic condition count
# WITHOUT class weighting + SHAP analysis
# ============================================================

library(tidyverse)
library(xgboost)
library(caret)
library(pROC)

# Load data ---------------------------------------------------------------

analysis_data_complete <- readRDS("analysis_data_complete.rds")

# Define variables --------------------------------------------------------

common_predictors <- c(
  "gender", "age2013", "marital_3cat", "employment_3cat",
  "education_3cat", "smoke_3cat", "alcohol_per_week",
  "vig_activity_3cat", "mod_activity_3cat"
)

# Create modelling dataset ------------------------------------------------

xgb_data_count <- analysis_data_complete %>%
  dplyr::select(
    eurodcat_w6,
    all_of(common_predictors),
    chronic_cat
  ) %>%
  mutate(
    eurodcat_w6 = factor(eurodcat_w6, levels = c("no", "yes"))
  ) %>%
  drop_na()

print(table(xgb_data_count$eurodcat_w6, useNA = "ifany"))
print(table(xgb_data_count$chronic_cat, useNA = "ifany"))

# Train-test split --------------------------------------------------------

set.seed(123)

index <- createDataPartition(
  xgb_data_count$eurodcat_w6,
  p = 0.8,
  list = FALSE
)

train_data <- xgb_data_count[index, ]
test_data  <- xgb_data_count[-index, ]

print(table(train_data$eurodcat_w6))
print(table(test_data$eurodcat_w6))

# Model matrices ----------------------------------------------------------

x_train <- model.matrix(eurodcat_w6 ~ . - 1, data = train_data)
x_test  <- model.matrix(eurodcat_w6 ~ . - 1, data = test_data)

y_train <- ifelse(train_data$eurodcat_w6 == "yes", 1, 0)
y_test  <- ifelse(test_data$eurodcat_w6 == "yes", 1, 0)

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest  <- xgb.DMatrix(data = x_test, label = y_test)

# Tuning grid -------------------------------------------------------------

xgb_grid <- expand.grid(
  max_depth = c(2, 3, 4),
  eta = c(0.03, 0.05, 0.10),
  gamma = c(0, 0.5),
  colsample_bytree = c(0.8, 1.0),
  min_child_weight = c(1, 3),
  subsample = c(0.8, 1.0),
  nrounds = c(100, 200)
)

# 10-fold CV tuning -------------------------------------------------------

tuning_results <- list()

set.seed(123)

for (i in seq_len(nrow(xgb_grid))) {
  
  cat("Running tuning model", i, "of", nrow(xgb_grid), "\n")
  
  params_cv <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = xgb_grid$max_depth[i],
    eta = xgb_grid$eta[i],
    gamma = xgb_grid$gamma[i],
    colsample_bytree = xgb_grid$colsample_bytree[i],
    min_child_weight = xgb_grid$min_child_weight[i],
    subsample = xgb_grid$subsample[i],
    seed = 123,
    nthread = 1
  )
  
  cv_model <- xgb.cv(
    params = params_cv,
    data = dtrain,
    nrounds = xgb_grid$nrounds[i],
    nfold = 10,
    stratified = TRUE,
    early_stopping_rounds = 10,
    maximize = TRUE,
    verbose = 1
  )
  
  best_iter <- which.max(cv_model$evaluation_log$test_auc_mean)
  
  tuning_results[[i]] <- xgb_grid[i, ] %>%
    mutate(
      best_iteration = best_iter,
      best_auc = cv_model$evaluation_log$test_auc_mean[best_iter],
      best_auc_sd = cv_model$evaluation_log$test_auc_std[best_iter]
    )
}

tuning_results <- bind_rows(tuning_results) %>%
  arrange(desc(best_auc))

print(tuning_results)

# Final tuned model -------------------------------------------------------

best_params <- tuning_results %>% slice(1)
print(best_params)

params_final <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  gamma = best_params$gamma,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = best_params$min_child_weight,
  subsample = best_params$subsample,
  seed = 123,
  nthread = 1
)

set.seed(123)

xgb_count_tuned <- xgb.train(
  params = params_final,
  data = dtrain,
  nrounds = best_params$best_iteration,
  verbose = 1
)

# Determine Youden threshold on TRAINING set ------------------------------

train_pred_probs <- predict(
  xgb_count_tuned,
  newdata = dtrain
)

train_observed <- factor(
  ifelse(y_train == 1, "yes", "no"),
  levels = c("no", "yes")
)

roc_train <- roc(
  response = train_observed,
  predictor = train_pred_probs,
  levels = c("no", "yes"),
  direction = "<"
)

youden_coords_train <- coords(
  roc_train,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

print(youden_coords_train)

youden_threshold <- as.numeric(youden_coords_train["threshold"])
print(youden_threshold)

# Predict on TEST set -----------------------------------------------------

pred_probs <- predict(
  xgb_count_tuned,
  newdata = dtest
)

observed <- factor(
  ifelse(y_test == 1, "yes", "no"),
  levels = c("no", "yes")
)

# Classification using default threshold 0.50 -----------------------------

pred_class_050 <- factor(
  ifelse(pred_probs >= 0.5, "yes", "no"),
  levels = c("no", "yes")
)

cm_050 <- confusionMatrix(
  data = pred_class_050,
  reference = observed,
  positive = "yes"
)

print(cm_050)

# Classification using Youden threshold ----------------------------------

pred_class_youden <- factor(
  ifelse(pred_probs >= youden_threshold, "yes", "no"),
  levels = c("no", "yes")
)

cm_youden <- confusionMatrix(
  data = pred_class_youden,
  reference = observed,
  positive = "yes"
)

print(cm_youden)

# ROC and AUC on TEST set -------------------------------------------------

roc_test <- roc(
  response = observed,
  predictor = pred_probs,
  levels = c("no", "yes"),
  direction = "<"
)

auc_value <- as.numeric(auc(roc_test))
print(auc_value)

auc_ci_boot <- ci.auc(
  roc_test,
  method = "bootstrap",
  boot.n = 2000
)

print(auc_ci_boot)

# Helper function for performance metrics --------------------------------

extract_performance <- function(cm, model_name, threshold_label, threshold_value, auc_value) {
  
  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])
  
  f1_score <- ifelse(
    is.na(precision) | is.na(recall) | (precision + recall == 0),
    NA,
    2 * ((precision * recall) / (precision + recall))
  )
  
  tibble(
    model = model_name,
    threshold_type = threshold_label,
    threshold = threshold_value,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    balanced_accuracy = as.numeric(cm$byClass["Balanced Accuracy"]),
    sensitivity = as.numeric(cm$byClass["Sensitivity"]),
    specificity = as.numeric(cm$byClass["Specificity"]),
    precision = precision,
    f1_score = f1_score,
    cohens_kappa = as.numeric(cm$overall["Kappa"]),
    auc = auc_value
  )
}

# Performance tables ------------------------------------------------------

model_performance_xgb_count_050 <- extract_performance(
  cm = cm_050,
  model_name = "XGBoost - chronic condition count",
  threshold_label = "Default 0.50",
  threshold_value = 0.5,
  auc_value = auc_value
)

model_performance_xgb_count_youden <- extract_performance(
  cm = cm_youden,
  model_name = "XGBoost - chronic condition count",
  threshold_label = "Youden",
  threshold_value = youden_threshold,
  auc_value = auc_value
)

model_performance_xgb_count_comparison <- bind_rows(
  model_performance_xgb_count_050,
  model_performance_xgb_count_youden
)

print(model_performance_xgb_count_comparison)

# Variable importance -----------------------------------------------------

varimp_xgb_count <- xgb.importance(
  feature_names = colnames(x_train),
  model = xgb_count_tuned
) %>%
  as_tibble()

print(varimp_xgb_count)

# SHAP analysis -----------------------------------------------------------

shap_values <- predict(
  xgb_count_tuned,
  newdata = dtest,
  predcontrib = TRUE
)

shap_values <- as.data.frame(shap_values)

# Remove intercept / bias term
shap_feature_values <- shap_values %>%
  dplyr::select(-`(Intercept)`)

# Mean absolute SHAP values = global feature importance
shap_importance <- shap_feature_values %>%
  summarise(across(everything(), ~ mean(abs(.x), na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "mean_abs_shap"
  ) %>%
  arrange(desc(mean_abs_shap))

print(shap_importance)

# Clean feature names -----------------------------------------------------

feature_labels <- c(
  gender1 = "Male sex",
  gender2 = "Female sex",
  age2013 = "Age",
  marital_3catnot_married = "Not married",
  marital_3catwidowed = "Widowed",
  marital_3catmissing = "Marital status missing",
  employment_3catretired = "Retired",
  employment_3catnot_working = "Not working",
  education_3catmid = "Medium education",
  education_3cathigh = "High education",
  smoke_3catsmoking = "Current smoking",
  smoke_3catunknown = "Smoking status unknown",
  alcohol_per_week = "Alcohol consumption",
  vig_activity_3catmoderate = "Moderate vigorous activity",
  vig_activity_3cathigh = "High vigorous activity",
  mod_activity_3catmoderate = "Moderate physical activity",
  mod_activity_3cathigh = "High physical activity",
  chronic_cat1 = "1 chronic condition",
  chronic_cat2 = "2 chronic conditions",
  chronic_cat3 = "3 chronic conditions",
  `chronic_cat4+` = "4+ chronic conditions"
)

# Top SHAP features -------------------------------------------------------

shap_top20 <- shap_importance %>%
  slice_max(mean_abs_shap, n = 20) %>%
  mutate(
    feature_clean = recode(feature, !!!feature_labels),
    feature_clean = reorder(feature_clean, mean_abs_shap)
  )

# SHAP feature importance plot -------------------------------------------

p_shap <- ggplot(
  shap_top20,
  aes(x = feature_clean, y = mean_abs_shap, fill = mean_abs_shap)
) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_fill_gradient(
    low = "#90CAF9",
    high = "#1565C0"
  ) +
  labs(
    title = "SHAP Feature Importance for the XGBoost Count Model",
    x = NULL,
    y = "Mean absolute SHAP value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 20)
  )

print(p_shap)

ggsave(
  filename = "shap_importance_xgb_count.png",
  plot = p_shap,
  width = 10,
  height = 7,
  dpi = 300
)

# Save outputs ------------------------------------------------------------

saveRDS(xgb_count_tuned, "xgb_count_tuned_model.rds")
saveRDS(tuning_results, "tuning_results_xgb_count.rds")
saveRDS(best_params, "best_params_xgb_count.rds")

saveRDS(youden_coords_train, "youden_coords_train_xgb_count.rds")
saveRDS(youden_threshold, "youden_threshold_xgb_count.rds")

saveRDS(model_performance_xgb_count_youden, "model_performance_xgb_count_youden.rds")
saveRDS(model_performance_xgb_count_050, "model_performance_xgb_count_050.rds")
saveRDS(model_performance_xgb_count_comparison, "model_performance_xgb_count_comparison.rds")

saveRDS(cm_youden, "confusion_matrix_xgb_count_youden.rds")
saveRDS(cm_050, "confusion_matrix_xgb_count_050.rds")
saveRDS(roc_test, "roc_xgb_count.rds")
saveRDS(auc_ci_boot, "auc_ci_xgb_count.rds")
saveRDS(varimp_xgb_count, "varimp_xgb_count.rds")

saveRDS(shap_values, "shap_values_xgb_count.rds")
saveRDS(shap_importance, "shap_importance_xgb_count.rds")

print(best_params_xgb_lca)