


# ============================================================
# 03_latent_class_analysis.R
# Latent Class Analysis based on chronic disease indicators
# ============================================================

# Load packages -----------------------------------------------------------

library(tidyverse)
library(poLCA)


# Load preprocessed complete analysis data -------------------------------

analysis_data_complete <- readRDS("analysis_data_complete.rds")

stopifnot("mergeid" %in% names(analysis_data_complete))
stopifnot(anyDuplicated(analysis_data_complete$mergeid) == 0)


# Define chronic disease variables ---------------------------------------

chronic_disease_vars <- c(
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
  "rheumatoid_arthritis",
  "osteoarthritis",
  "other_chronic"
)


# Create LCA dataset ------------------------------------------------------

lca_data <- analysis_data_complete %>%
  dplyr::select(mergeid, all_of(chronic_disease_vars))


# Prepare poLCA input -----------------------------------------------------
# poLCA requires categorical indicators coded as positive integers.
# Chronic disease indicators are coded 0/1, so they are converted to 1/2.

lca_vars <- lca_data %>%
  dplyr::select(-mergeid) %>%
  mutate(across(everything(), ~ as.integer(.x) + 1))


# Define LCA formula ------------------------------------------------------

lca_formula <- as.formula(
  paste0(
    "cbind(",
    paste(chronic_disease_vars, collapse = ", "),
    ") ~ 1"
  )
)


# Fit LCA models ----------------------------------------------------------

set.seed(123)

lca_2 <- poLCA(
  formula = lca_formula,
  data = lca_vars,
  nclass = 2,
  maxiter = 1000,
  nrep = 50,
  verbose = FALSE
)

lca_3 <- poLCA(
  formula = lca_formula,
  data = lca_vars,
  nclass = 3,
  maxiter = 1000,
  nrep = 50,
  verbose = FALSE
)

lca_4 <- poLCA(
  formula = lca_formula,
  data = lca_vars,
  nclass = 4,
  maxiter = 1000,
  nrep = 50,
  verbose = FALSE
)


# Compare model fit -------------------------------------------------------

calc_entropy <- function(posterior) {
  eps <- 1e-12
  entropy_individual <- -rowSums(posterior * log(posterior + eps))
  max_entropy <- log(ncol(posterior))
  1 - mean(entropy_individual / max_entropy)
}

model_fit_lca <- tibble(
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

print(model_fit_lca)


# Select final LCA model --------------------------------------------------

final_lca <- lca_3


# Add class assignment and posterior probabilities ------------------------

lca_results <- lca_data %>%
  mutate(
    lca_class_3 = final_lca$predclass,
    class1_prob = final_lca$posterior[, 1],
    class2_prob = final_lca$posterior[, 2],
    class3_prob = final_lca$posterior[, 3]
  )


# Add interpretable class labels -----------------------------------------

lca_results <- lca_results %>%
  mutate(
    lca_label = case_when(
      lca_class_3 == 1 ~ "Cardiometabolic",
      lca_class_3 == 2 ~ "Musculoskeletal",
      lca_class_3 == 3 ~ "Low morbidity",
      TRUE ~ NA_character_
    )
  )


# Summarise class distributions ------------------------------------------

posterior_class_distribution <- tibble(
  class = 1:3,
  posterior_proportion = final_lca$P,
  posterior_percent = round(final_lca$P * 100, 1)
)

modal_class_distribution <- lca_results %>%
  count(lca_class_3, lca_label, name = "n") %>%
  mutate(
    modal_proportion = n / sum(n),
    modal_percent = round(modal_proportion * 100, 1)
  )

print(posterior_class_distribution)
print(modal_class_distribution)


# Extract conditional item-response probabilities -------------------------

lca_item_probs <- map_dfr(
  names(final_lca$probs),
  function(variable) {
    tibble(
      variable = variable,
      cardiometabolic = final_lca$probs[[variable]][1, 2],
      musculoskeletal = final_lca$probs[[variable]][2, 2],
      low_morbidity = final_lca$probs[[variable]][3, 2]
    )
  }
) %>%
  mutate(
    across(
      c(cardiometabolic, musculoskeletal, low_morbidity),
      ~ round(.x, 3)
    )
  )

print(lca_item_probs)


# Plot conditional item-response probabilities ----------------------------

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
    legend.title = element_blank()
  ) +
  labs(
    title = "Conditional probabilities of chronic conditions across latent classes",
    x = "Chronic conditions",
    y = "Conditional item-response probability"
  )

print(lca_profile_plot)

ggsave(
  "lca_item_response_probability_plot.png",
  plot = lca_profile_plot,
  width = 14,
  height = 8,
  dpi = 300
)


# Merge LCA features back to analysis data --------------------------------

analysis_data_lca <- analysis_data_complete %>%
  left_join(
    lca_results %>%
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


# Validate merge ----------------------------------------------------------

stopifnot(nrow(analysis_data_lca) == nrow(analysis_data_complete))
stopifnot(sum(!is.na(analysis_data_lca$lca_class_3)) == nrow(lca_results))

table(analysis_data_lca$lca_label, useNA = "ifany")


# Save outputs ------------------------------------------------------------

saveRDS(analysis_data_lca, "analysis_data_lca.rds")
saveRDS(model_fit_lca, "model_fit_lca.rds")
saveRDS(lca_item_probs, "lca_item_probs.rds")
saveRDS(posterior_class_distribution, "posterior_class_distribution.rds")
saveRDS(modal_class_distribution, "modal_class_distribution.rds")
