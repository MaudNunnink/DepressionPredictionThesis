

# ============================================================
# Sex-stratified subgroup analysis
# Logistic regression model: chronic condition count
# gender: 1 = male, 2 = female
# ============================================================

library(tidyverse)
library(caret)
library(pROC)
library(broom)

analysis_data_complete <- readRDS("analysis_data_complete.rds")

common_predictors <- c(
  "age2013", "marital_3cat", "employment_3cat",
  "education_3cat", "smoke_3cat", "alcohol_per_week",
  "vig_activity_3cat", "mod_activity_3cat"
)

extract_performance <- function(cm, subgroup_name, auc_value) {
  
  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  tibble(
    subgroup = subgroup_name,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    balanced_accuracy = as.numeric(cm$byClass["Balanced Accuracy"]),
    sensitivity = recall,
    specificity = as.numeric(cm$byClass["Specificity"]),
    precision = precision,
    f1_score = f1_score,
    cohens_kappa = as.numeric(cm$overall["Kappa"]),
    auc = auc_value
  )
}

run_sex_subgroup_lr_count <- function(data, sex_value, subgroup_name) {
  
  subgroup_data <- data %>%
    filter(gender == sex_value) %>%
    select(
      eurodcat_w6,
      all_of(common_predictors),
      chronic_cat
    ) %>%
    drop_na()
  
  print(paste("Subgroup:", subgroup_name))
  print(table(subgroup_data$eurodcat_w6, useNA = "ifany"))
  
  set.seed(123)
  
  index <- createDataPartition(
    subgroup_data$eurodcat_w6,
    p = 0.8,
    list = FALSE
  )
  
  train_data <- subgroup_data[index, ]
  test_data  <- subgroup_data[-index, ]
  
  class_weights <- ifelse(
    train_data$eurodcat_w6 == "yes",
    sum(train_data$eurodcat_w6 == "no") / sum(train_data$eurodcat_w6 == "yes"),
    1
  )
  
  train_control <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  set.seed(123)
  
  model <- train(
    eurodcat_w6 ~ .,
    data = train_data,
    method = "glm",
    family = binomial,
    weights = class_weights,
    trControl = train_control,
    metric = "ROC"
  )
  
  pred_probs <- predict(
    model,
    newdata = test_data,
    type = "prob"
  )[, "yes"]
  
  observed <- factor(test_data$eurodcat_w6, levels = c("no", "yes"))
  
  pred_class <- factor(
    ifelse(pred_probs >= 0.5, "yes", "no"),
    levels = c("no", "yes")
  )
  
  cm <- confusionMatrix(
    pred_class,
    observed,
    positive = "yes"
  )
  
  roc_test <- roc(
    response = observed,
    predictor = pred_probs,
    levels = c("no", "yes")
  )
  
  auc_value <- as.numeric(auc(roc_test))
  
  auc_ci_boot <- ci.auc(
    roc_test,
    method = "bootstrap",
    boot.n = 2000
  )
  
  performance <- extract_performance(
    cm = cm,
    subgroup_name = subgroup_name,
    auc_value = auc_value
  ) %>%
    mutate(
      n_total = nrow(subgroup_data),
      n_train = nrow(train_data),
      n_test = nrow(test_data),
      auc_ci_lower = as.numeric(auc_ci_boot[1]),
      auc_ci_upper = as.numeric(auc_ci_boot[3])
    )
  
  odds_ratios <- broom::tidy(
    model$finalModel,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    mutate(
      subgroup = subgroup_name,
      across(where(is.numeric), ~ round(.x, 3))
    )
  
  list(
    model = model,
    performance = performance,
    odds_ratios = odds_ratios,
    confusion_matrix = cm,
    roc = roc_test,
    auc_ci_boot = auc_ci_boot
  )
}

# Run subgroup models
male_results <- run_sex_subgroup_lr_count(
  data = analysis_data_complete,
  sex_value = 1,
  subgroup_name = "Male"
)

female_results <- run_sex_subgroup_lr_count(
  data = analysis_data_complete,
  sex_value = 2,
  subgroup_name = "Female"
)

# Combine outputs
sex_subgroup_performance <- bind_rows(
  male_results$performance,
  female_results$performance
)

sex_subgroup_odds_ratios <- bind_rows(
  male_results$odds_ratios,
  female_results$odds_ratios
)

print(sex_subgroup_performance)
print(sex_subgroup_odds_ratios, n = Inf)

# Separate tables for males and females -----------------------------------

male_odds_ratios <- sex_subgroup_odds_ratios %>%
  filter(subgroup == "Male")

female_odds_ratios <- sex_subgroup_odds_ratios %>%
  filter(subgroup == "Female")

print(male_odds_ratios, n = Inf)
print

library(flextable)
library(officer)

# Male table
ft_male <- flextable(male_odds_ratios) %>%
  autofit()

save_as_docx(
  "Male subgroup logistic regression results" = ft_male,
  path = "male_subgroup_odds_ratios.docx"
)

# Female table
ft_female <- flextable(female_odds_ratios) %>%
  autofit()

save_as_docx(
  "Female subgroup logistic regression results" = ft_female,
  path = "female_subgroup_odds_ratios.docx"
)

# Save outputs
saveRDS(sex_subgroup_performance, "sex_subgroup_performance_lr_count.rds")
saveRDS(sex_subgroup_odds_ratios, "sex_subgroup_odds_ratios_lr_count.rds")
saveRDS(male_results, "male_lr_count_results.rds")
saveRDS(female_results, "female_lr_count_results.rds")