

# ============================================================
# 0. Packages
# ============================================================

library(dplyr)
library(tidyr)
library(poLCA)
library(ggplot2)
library(tibble)
library(haven)


# ============================================================
# 1. Load data and preserve identifiers
# ============================================================

panel_full <- readRDS("panel_full.rds")



# Verify that original SHARE participant IDs are retained
stopifnot("mergeid" %in% names(panel_full))
stopifnot(anyDuplicated(panel_full$mergeid) == 0)


# ============================================================
# 2. Recode chronic condition indicators
# ============================================================

panel_full <- panel_full %>%
  mutate(across(
    c(
      ph006d1, ph006d2, ph006d3, ph006d4, ph006d5,
      ph006d6, ph006d10, ph006d11, ph006d12, ph006d13,
      ph006d14, ph006d16, ph006d18, ph006d19, ph006d20,
      ph006dot
    ),
    ~ {
      x <- haven::zap_labels(.x)
      x <- haven::zap_missing(x)
      x <- as.numeric(x)
      
      case_when(
        x == 1 ~ 1,
        x == 0 ~ 0,
        TRUE ~ NA_real_
      )
    }
  ))

# ============================================================
# 3. Rename chronic condition variables
# ============================================================

panel_full <- panel_full %>%
  rename(
    heart_attack = ph006d1,
    high_blood_pressure = ph006d2,
    high_blood_cholesterol = ph006d3,
    stroke = ph006d4,
    diabetes = ph006d5,
    chronic_lung_disease = ph006d6,
    cancer = ph006d10,
    ulcer = ph006d11,
    parkinson = ph006d12,
    cataracts = ph006d13,
    hip_fracture = ph006d14,
    alzheimers = ph006d16,
    other_affective = ph006d18,
    RA = ph006d19,
    OA = ph006d20,
    other_chronic = ph006dot
  )


# ============================================================
# 4. Create LCA dataset
# ============================================================

chronic_vars <- c(
  "heart_attack",
  "high_blood_pressure",
  "high_blood_cholesterol",
  "stroke",
  "diabetes",
  "chronic_lung_disease",
  "cancer",
  "ulcer",
  "parkinson",
  "cataracts",
  "hip_fracture",
  "alzheimers",
  "other_affective",
  "RA",
  "OA",
  "other_chronic"
)

lca_data <- panel_full %>%
  dplyr::select(mergeid, all_of(chronic_vars)) %>%
  drop_na()

# Restrict LCA sample to participants with complete data on variables
# required for downstream predictive modelling.
model_complete_vars <- c(
  "ep005_",
  "isced1997_r",
  "br010_",
  "br015_",
  "br016_"

)

lca_data <- panel_full %>%
  filter(if_all(all_of(model_complete_vars), ~ !is.na(.x))) %>%
  dplyr::select(mergeid, all_of(chronic_vars)) %>%
  drop_na()



# ============================================================
# 5. Prepare poLCA input
# ============================================================

lca_vars <- lca_data %>%
  dplyr::select(-mergeid) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ .x + 1
    )
  )


# ============================================================
# 6. Fit LCA models
# ============================================================

lca_formula <- cbind(
  heart_attack,
  high_blood_pressure,
  high_blood_cholesterol,
  stroke,
  diabetes,
  chronic_lung_disease,
  cancer,
  ulcer,
  parkinson,
  cataracts,
  hip_fracture,
  alzheimers,
  other_affective,
  RA,
  OA,
  other_chronic
) ~ 1

set.seed(123)

lca_2 <- poLCA(
  lca_formula,
  data = lca_vars,
  nclass = 2,
  maxiter = 1000,
  nrep = 50,
  verbose = FALSE
)

lca_3 <- poLCA(
  lca_formula,
  data = lca_vars,
  nclass = 3,
  maxiter = 1000,
  nrep = 50,
  verbose = FALSE
)

lca_4 <- poLCA(
  lca_formula,
  data = lca_vars,
  nclass = 4,
  maxiter = 1000,
  nrep = 50,
  verbose = FALSE
)

# ============================================================
# 7. Compare LCA model fit
# ============================================================

calc_entropy <- function(posterior) {
  eps <- 1e-12
  entropy_individual <- -rowSums(posterior * log(posterior + eps))
  max_entropy <- log(ncol(posterior))
  1 - mean(entropy_individual / max_entropy)
}

model_fit <- tibble(
  n_classes = c(2, 3, 4),
  log_likelihood = c(lca_2$llik, lca_3$llik, lca_4$llik),
  AIC = c(lca_2$aic, lca_3$aic, lca_4$aic),
  BIC = c(lca_2$bic, lca_3$bic, lca_4$bic),
  entropy = c(
    calc_entropy(lca_2$posterior),
    calc_entropy(lca_3$posterior),
    calc_entropy(lca_4$posterior)
  )
)

print(model_fit)


#saveRDS(model_fit, "model_fit_lca.rds")


#convergence_check <- tibble(
#  n_classes = c(2, 3, 4),
#  iterations = c(lca_2$numiter, lca_3$numiter, lca_4$numiter),
#  reached_maxiter = c(
#    lca_2$numiter >= 1000,
#   lca_3$numiter >= 1000,
#    lca_4$numiter >= 1000
#  ),
#  estimation_flag = c(lca_2$eflag, lca_3$eflag, lca_4$eflag)
#)

# convergence_check

# ============================================================
# 8. Select final LCA solution
# ============================================================

# The 3-class solution was selected based on entropy, convergence,
# and substantive interpretability.
final_lca <- lca_3




# ============================================================
# 9. Add modal class assignment and posterior probabilities
# ============================================================

lca_data <- lca_data %>%
  mutate(
    lca_class_3 = final_lca$predclass,
    class1_prob = final_lca$posterior[, 1],
    class2_prob = final_lca$posterior[, 2],
    class3_prob = final_lca$posterior[, 3]
  )


# ============================================================
# 10. Add interpretable class labels
# ============================================================

lca_data <- lca_data %>%
  mutate(
    lca_label = case_when(
      lca_class_3 == 1 ~ "Cardiometabolic",
      lca_class_3 == 2 ~ "Musculoskeletal",
      lca_class_3 == 3 ~ "Low morbidity",
      TRUE ~ NA_character_
    )
  )


# ============================================================
# 11. Summarise class distributions
# ============================================================

posterior_class_distribution <- tibble(
  class = 1:3,
  posterior_proportion = final_lca$P,
  posterior_percent = round(final_lca$P * 100, 1)
)

modal_class_distribution <- lca_data %>%
  count(lca_class_3, lca_label, name = "n") %>%
  mutate(
    modal_proportion = n / sum(n),
    modal_percent = round(modal_proportion * 100, 1)
  )

print(posterior_class_distribution)
print(modal_class_distribution)

# ============================================================
# 12. Extract conditional item-response probabilities
# ============================================================

# Prevent scientific notation in printed output
options(scipen = 999)

lca_item_probs <- do.call(
  rbind,
  lapply(names(final_lca$probs), function(variable) {
    tibble(
      variable = variable,
      cardiometabolic = final_lca$probs[[variable]][1, 2],
      musculoskeletal = final_lca$probs[[variable]][2, 2],
      low_morbidity = final_lca$probs[[variable]][3, 2]
    )
  })
) %>%
  mutate(
    across(
      c(cardiometabolic, musculoskeletal, low_morbidity),
      ~ round(.x, 3)
    )
  )

print(lca_item_probs)

# ============================================================
# 13. Plot conditional item-response probabilities
# ============================================================

lca_plot_data <- lca_item_probs %>%
  pivot_longer(
    cols = c(cardiometabolic, musculoskeletal, low_morbidity),
    names_to = "latent_class",
    values_to = "probability"
  ) %>%
  mutate(
    latent_class = recode(
      latent_class,
      cardiometabolic = "Cardiometabolic",
      musculoskeletal = "Musculoskeletal",
      low_morbidity = "Low morbidity"
    )
  )

lca_profile_plot <- ggplot(
  lca_plot_data,
  aes(
    x = variable,
    y = probability,
    group = latent_class,
    color = latent_class
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    title = "Conditional probabilities of chronic conditions across latent classes",
    x = "Chronic conditions",
    y = "Conditional item-response probability"
  )

print(lca_profile_plot)

ggsave(
  "lca_item_response_probability_plot2.png",
  plot = lca_profile_plot,
  width = 14,
  height = 8,
  dpi = 300
)


# ============================================================
# 14. Merge latent class features back to full panel
# ============================================================

panel_full_with_latent_features <- panel_full %>%
  left_join(
    lca_data %>%
      dplyr::select(
        mergeid,
        lca_class_3,
        lca_label,
        class1_prob,
        class2_prob,
        class3_prob
      ),
    by = "mergeid"
  )




# ============================================================
# 15. Validate merge
# ============================================================

# The full panel contains all eligible participants before complete-case
# restriction for LCA and downstream predictive modelling.
# Therefore, participants outside the LCA/modelling sample are expected
# to have missing latent class features after the merge.

merge_check <- tibble(
  check = c(
    "Rows in original panel",
    "Rows after merge",
    "Participants in LCA sample",
    "Participants with assigned latent class",
    "Participants outside LCA sample"
  ),
  value = c(
    nrow(panel_full),
    nrow(panel_full_with_latent_features),
    nrow(lca_data),
    sum(!is.na(panel_full_with_latent_features$lca_class_3)),
    sum(is.na(panel_full_with_latent_features$lca_class_3))
  )
)

print(merge_check)

stopifnot(nrow(panel_full) == nrow(panel_full_with_latent_features))
stopifnot(sum(!is.na(panel_full_with_latent_features$lca_class_3)) == nrow(lca_data))

table(panel_full_with_latent_features$lca_label, useNA = "ifany")
