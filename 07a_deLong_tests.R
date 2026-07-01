

# ============================================================
# DeLong tests for AUC-ROC comparisons
# ============================================================

library(pROC)
library(tidyverse)
library(flextable)
library(officer)

# ------------------------------------------------------------
# 1. Load ROC objects
# ------------------------------------------------------------

roc_glm_chronic <- readRDS("roc_glm_chronic.rds")
roc_glm_count   <- readRDS("roc_glm_count.rds")
roc_glm_lca     <- readRDS("roc_glm_lca.rds")

roc_xgb_chronic <- readRDS("roc_xgb_chronic.rds")
roc_xgb_count   <- readRDS("roc_xgb_count.rds")
roc_xgb_lca     <- readRDS("roc_xgb_lca.rds")


# ------------------------------------------------------------
# 2. Optional check: inspect AUCs
# ------------------------------------------------------------

auc_check <- tibble(
  Model = c(
    "LR-Chronic",
    "LR-Count",
    "LR-LCA",
    "XGB-Chronic",
    "XGB-Count",
    "XGB-LCA"
  ),
  AUC = c(
    as.numeric(auc(roc_glm_chronic)),
    as.numeric(auc(roc_glm_count)),
    as.numeric(auc(roc_glm_lca)),
    as.numeric(auc(roc_xgb_chronic)),
    as.numeric(auc(roc_xgb_count)),
    as.numeric(auc(roc_xgb_lca))
  )
) %>%
  mutate(AUC = round(AUC, 3))

print(auc_check)


# ------------------------------------------------------------
# 3. Helper function for DeLong comparisons
# ------------------------------------------------------------

delong_compare <- function(roc1, roc2, comparison, section) {
  
  # Check whether paired DeLong test is appropriate
  if (!pROC::are.paired(roc1, roc2)) {
    warning(
      paste0(
        "ROC curves for ", comparison,
        " do not appear to be paired. Check whether the same test set was used."
      )
    )
  }
  
  test <- roc.test(
    roc1,
    roc2,
    method = "delong",
    paired = TRUE
  )
  
  auc1 <- as.numeric(auc(roc1))
  auc2 <- as.numeric(auc(roc2))
  
  tibble(
    Section = section,
    Comparison = comparison,
    `AUC difference` = abs(auc1 - auc2),
    `p-value` = test$p.value
  )
}


# ------------------------------------------------------------
# 4. Run DeLong tests
# ------------------------------------------------------------

delong_results <- bind_rows(
  
  # LR vs XGBoost
  delong_compare(
    roc_glm_chronic,
    roc_xgb_chronic,
    "LR-Chronic vs XGB-Chronic",
    "LR vs XGBoost"
  ),
  delong_compare(
    roc_glm_count,
    roc_xgb_count,
    "LR-Count vs XGB-Count",
    "LR vs XGBoost"
  ),
  delong_compare(
    roc_glm_lca,
    roc_xgb_lca,
    "LR-LCA vs XGB-LCA",
    "LR vs XGBoost"
  ),
  
  # Representations within LR
  delong_compare(
    roc_glm_count,
    roc_glm_chronic,
    "LR-Count vs LR-Chronic",
    "Representations (LR)"
  ),
  delong_compare(
    roc_glm_count,
    roc_glm_lca,
    "LR-Count vs LR-LCA",
    "Representations (LR)"
  ),
  delong_compare(
    roc_glm_chronic,
    roc_glm_lca,
    "LR-Chronic vs LR-LCA",
    "Representations (LR)"
  ),
  
  # Representations within XGBoost
  delong_compare(
    roc_xgb_chronic,
    roc_xgb_count,
    "XGB-Chronic vs XGB-Count",
    "Representations (XGBoost)"
  ),
  delong_compare(
    roc_xgb_chronic,
    roc_xgb_lca,
    "XGB-Chronic vs XGB-LCA",
    "Representations (XGBoost)"
  ),
  delong_compare(
    roc_xgb_count,
    roc_xgb_lca,
    "XGB-Count vs XGB-LCA",
    "Representations (XGBoost)"
  )
)


# ------------------------------------------------------------
# 5. Format table
# ------------------------------------------------------------

delong_results_formatted <- delong_results %>%
  mutate(
    `AUC difference` = round(`AUC difference`, 3),
    `p-value` = case_when(
      `p-value` < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", `p-value`)
    )
  )

print(delong_results_formatted)


# ------------------------------------------------------------
# 6. Save as CSV
# ------------------------------------------------------------

write.csv(
  delong_results_formatted,
  "delong_auc_comparisons.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 7. Optional: save as Word table
# ------------------------------------------------------------

ft_delong <- delong_results_formatted %>%
  flextable() %>%
  merge_v(j = "Section") %>%
  valign(j = "Section", valign = "top") %>%
  autofit()

save_as_docx(
  "DeLong tests for AUC-ROC comparisons" = ft_delong,
  path = "delong_auc_comparisons.docx"
)
