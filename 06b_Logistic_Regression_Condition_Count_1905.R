

# ============================================================
# 05b_logistic_regression_count.R
# Logistic regression model: chronic condition count
# ============================================================

library(tidyverse)
library(caret)
library(pROC)
library(broom)
library(flextable)
library(officer)

analysis_data_complete <- readRDS("analysis_data_complete.rds")

table(analysis_data_complete$gender)

common_predictors <- c(
  "gender", "age2013", "marital_3cat", "employment_3cat",
  "education_3cat", "smoke_3cat", "alcohol_per_week",
  "vig_activity_3cat", "mod_activity_3cat"
)

glm_data_count <- analysis_data_complete %>%
  dplyr::select(
    eurodcat_w6,
    all_of(common_predictors),
    chronic_cat
  )

print(table(glm_data_count$eurodcat_w6, useNA = "ifany"))
print(table(glm_data_count$chronic_cat, useNA = "ifany"))

# Train-test split --------------------------------------------------------

set.seed(123)

index <- createDataPartition(
  glm_data_count$eurodcat_w6,
  p = 0.8,
  list = FALSE
)

train_data <- glm_data_count[index, ]
test_data  <- glm_data_count[-index, ]

print(table(train_data$eurodcat_w6))
print(table(test_data$eurodcat_w6))

# Class weights -----------------------------------------------------------

class_weights <- ifelse(
  train_data$eurodcat_w6 == "yes",
  sum(train_data$eurodcat_w6 == "no") / sum(train_data$eurodcat_w6 == "yes"),
  1
)

table(train_data$eurodcat_w6)
table(class_weights)


# Model training ----------------------------------------------------------

train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

set.seed(123)

glm_cv_count <- train(
  eurodcat_w6 ~ .,
  data = train_data,
  method = "glm",
  family = binomial,
  weights = class_weights,
  trControl = train_control,
  metric = "ROC"
)

# Determine Youden threshold on TRAINING set ------------------------------

train_pred_probs <- predict(
  glm_cv_count,
  newdata = train_data,
  type = "prob"
)[, "yes"]

roc_train <- roc(
  response = train_data$eurodcat_w6,
  predictor = train_pred_probs,
  levels = c("no", "yes")
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
  glm_cv_count,
  newdata = test_data,
  type = "prob"
)[, "yes"]

observed <- factor(
  test_data$eurodcat_w6,
  levels = c("no", "yes")
)

# Classification using default threshold 0.50 -----------------------------

pred_class_050 <- ifelse(pred_probs >= 0.5, "yes", "no")

pred_class_050 <- factor(
  pred_class_050,
  levels = c("no", "yes")
)

cm_050 <- confusionMatrix(
  pred_class_050,
  observed,
  positive = "yes"
)

print(cm_050)

# Classification using Youden threshold ----------------------------------

pred_class_youden <- ifelse(pred_probs >= youden_threshold, "yes", "no")

pred_class_youden <- factor(
  pred_class_youden,
  levels = c("no", "yes")
)

cm_youden <- confusionMatrix(
  pred_class_youden,
  observed,
  positive = "yes"
)

print(cm_youden)

# ROC and AUC on TEST set -------------------------------------------------

roc_test <- roc(
  response = observed,
  predictor = pred_probs,
  levels = c("no", "yes")
)

auc_value <- as.numeric(auc(roc_test))
print(auc_value)

auc_ci_boot <- ci.auc(
  roc_test,
  method = "bootstrap",
  boot.n = 2000
)

print(auc_ci_boot)

# Helper function for performance metrics ---------------------------------

extract_performance <- function(cm, model_name, threshold_label, threshold_value, auc_value) {
  
  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
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

model_performance_glm_count <- extract_performance(
  cm = cm_youden,
  model_name = "Logistic regression - chronic condition count",
  threshold_label = "Youden",
  threshold_value = youden_threshold,
  auc_value = auc_value
)

model_performance_glm_count_050 <- extract_performance(
  cm = cm_050,
  model_name = "Logistic regression - chronic condition count",
  threshold_label = "Default 0.50",
  threshold_value = 0.5,
  auc_value = auc_value
)

model_performance_glm_count_comparison <- bind_rows(
  model_performance_glm_count_050,
  model_performance_glm_count
)

print(model_performance_glm_count_comparison)

# Coefficients / adjusted odds ratios -------------------------------------

glm_count_results <- broom::tidy(
  glm_cv_count$finalModel,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(glm_count_results, n = Inf)

# Export coefficient table to Word ----------------------------------------

ft_count <- flextable(glm_count_results) %>%
  autofit()

save_as_docx(
  "Logistic regression - chronic condition count" = ft_count,
  path = "glm_count_results.docx"
)

View(model_performance_glm_count_comparison)

# Save outputs ------------------------------------------------------------

saveRDS(glm_cv_count, "glm_cv_count.rds")

saveRDS(youden_coords_train, "youden_coords_train_glm_count.rds")
saveRDS(youden_threshold, "youden_threshold_glm_count.rds")

saveRDS(model_performance_glm_count, "model_performance_glm_count.rds")
saveRDS(model_performance_glm_count_050, "model_performance_glm_count_050.rds")
saveRDS(model_performance_glm_count_comparison, "model_performance_glm_count_comparison.rds")

saveRDS(cm_youden, "confusion_matrix_glm_count_youden.rds")
saveRDS(cm_050, "confusion_matrix_glm_count_050.rds")

saveRDS(roc_test, "roc_glm_count.rds")
saveRDS(auc_ci_boot, "auc_ci_boot_glm_count.rds")

saveRDS(glm_count_results, "glm_count_results.rds")