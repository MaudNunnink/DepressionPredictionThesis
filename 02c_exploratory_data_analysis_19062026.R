# ============================================================
# 02c_exploratory_data_analysis.R
# Descriptive statistics, exploratory figures, and Table 1
# ============================================================

library(tidyverse)
library(haven)

# Create output folders ---------------------------------------------------

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/figures", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE)


# Load data ---------------------------------------------------------------

panel_full <- readRDS("panel_full.rds")
analysis_data_complete <- readRDS("analysis_data_complete.rds")


# Keep only participants included in the final analytical sample -----------

eda_data <- panel_full %>%
  filter(mergeid %in% analysis_data_complete$mergeid)


# Rename variables needed for EDA -----------------------------------------

eda_data <- eda_data %>%
  rename(
    heart_attack           = ph006d1,
    high_blood_pressure    = ph006d2,
    high_blood_cholesterol = ph006d3,
    stroke                 = ph006d4,
    diabetes               = ph006d5,
    chronic_lung_disease   = ph006d6,
    cancer                 = ph006d10,
    ulcer                  = ph006d11,
    parkinson              = ph006d12,
    cataracts              = ph006d13,
    hip_fracture           = ph006d14,
    alzheimers             = ph006d16,
    other_affective        = ph006d18,
    rheumatoid_arthritis   = ph006d19,
    osteoarthritis         = ph006d20,
    other_chronic          = ph006dot,
    employment_status      = ep005_,
    educational_level      = isced1997_r
  ) %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels))


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


# Prepare EDA variables ---------------------------------------------------

eda_data <- eda_data %>%
  mutate(
    gender = case_when(
      gender %in% c(1, "1") ~ "Male",
      gender %in% c(2, "2") ~ "Female",
      TRUE ~ as.character(gender)
    ),
    gender = factor(gender, levels = c("Male", "Female")),

    age_group = case_when(
      age2013 >= 50 & age2013 <= 64 ~ "50-64",
      age2013 >= 65 & age2013 <= 74 ~ "65-74",
      age2013 >= 75 ~ "75+",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("50-64", "65-74", "75+")),

    living_arrangement = case_when(
      hhsize == 1 ~ "Living alone",
      hhsize > 1 ~ "Living with others",
      TRUE ~ NA_character_
    ),
    living_arrangement = factor(
      living_arrangement,
      levels = c("Living alone", "Living with others")
    ),

    chronicw5 = na_if(chronicw5, -1),
    chronicw5 = na_if(chronicw5, -2),

    chronic_cat = case_when(
      chronicw5 == 0 ~ "0",
      chronicw5 == 1 ~ "1",
      chronicw5 == 2 ~ "2",
      chronicw5 == 3 ~ "3",
      chronicw5 >= 4 ~ "4+",
      TRUE ~ NA_character_
    ),
    chronic_cat = factor(chronic_cat, levels = c("0", "1", "2", "3", "4+")),

    chronic_cat_table1 = case_when(
      chronicw5 == 0 ~ "0 conditions",
      chronicw5 == 1 ~ "1 condition",
      chronicw5 >= 2 ~ "2+ conditions",
      TRUE ~ NA_character_
    ),
    chronic_cat_table1 = factor(
      chronic_cat_table1,
      levels = c("0 conditions", "1 condition", "2+ conditions")
    ),

    educational_level = na_if(educational_level, 97),

    education_3cat = case_when(
      educational_level %in% c(0, 1, 2) ~ "Low",
      educational_level %in% c(3, 4)    ~ "Medium",
      educational_level %in% c(5, 6)    ~ "High",
      TRUE ~ NA_character_
    ),
    education_3cat = factor(
      education_3cat,
      levels = c("Low", "Medium", "High")
    ),

    employment_status = as.numeric(as.character(employment_status)),
    employment_status = na_if(employment_status, 97),

    employment_3cat = case_when(
      employment_status == 2 ~ "Employed",
      employment_status == 1 ~ "Retired",
      employment_status %in% c(3, 4, 5) ~ "Other",
      TRUE ~ NA_character_
    ),
    employment_3cat = factor(
      employment_3cat,
      levels = c("Employed", "Retired", "Other")
    ),

    eurodcat_w5_num = case_when(
      eurodcat_w5 %in% c(1, "1", "yes", "Yes") ~ 1,
      eurodcat_w5 %in% c(0, "0", "no", "No") ~ 0,
      TRUE ~ NA_real_
    ),

    eurodcat_w6_num = case_when(
      eurodcat_w6 %in% c(1, "1", "yes", "Yes") ~ 1,
      eurodcat_w6 %in% c(0, "0", "no", "No") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    across(all_of(chronic_disease_vars), ~ na_if(.x, -1)),
    across(all_of(chronic_disease_vars), ~ na_if(.x, -2))
  )


# Add EURO-D Wave 6 score if available ------------------------------------
# The binary EURO-D variable is already available as eurodcat_w6_num.
# The raw EURO-D score is not currently known in this dataframe.
# This block searches for a likely score variable.
# If no score variable is found, the Table 1 row will show "-".

eurod_score_candidates <- c(
  "eurod_w6",
  "eurodscore_w6",
  "eurod_score_w6",
  "eurod_sum_w6",
  "eurod12_w6",
  "eurod_w6_score",
  "eurod_score",
  "eurod"
)

eurod_score_var_eda <- intersect(eurod_score_candidates, names(eda_data))[1]
eurod_score_var_analysis <- intersect(eurod_score_candidates, names(analysis_data_complete))[1]

if (!is.na(eurod_score_var_eda)) {

  eda_data <- eda_data %>%
    mutate(eurod_w6_score = as.numeric(.data[[eurod_score_var_eda]]))

} else if (!is.na(eurod_score_var_analysis)) {

  eurod_score_lookup <- analysis_data_complete %>%
    select(
      mergeid,
      eurod_w6_score = all_of(eurod_score_var_analysis)
    ) %>%
    mutate(eurod_w6_score = as.numeric(eurod_w6_score))

  eda_data <- eda_data %>%
    left_join(eurod_score_lookup, by = "mergeid")

} else {

  eda_data <- eda_data %>%
    mutate(eurod_w6_score = NA_real_)

  message(
    "No raw EURO-D Wave 6 score variable found. ",
    "The mean EURO-D score row in Table 1 will show '-'."
  )
}


# Sample characteristics --------------------------------------------------

sample_summary <- tibble(
  n = nrow(eda_data),
  percent_female = mean(eda_data$gender == "Female", na.rm = TRUE) * 100,
  mean_age = mean(eda_data$age2013, na.rm = TRUE),
  sd_age = sd(eda_data$age2013, na.rm = TRUE),
  median_age = median(eda_data$age2013, na.rm = TRUE),
  percent_living_alone = mean(
    eda_data$living_arrangement == "Living alone",
    na.rm = TRUE
  ) * 100
)

age_group_table <- eda_data %>%
  count(age_group) %>%
  mutate(percent = n / sum(n) * 100)

living_arrangement_table <- eda_data %>%
  count(living_arrangement) %>%
  mutate(percent = n / sum(n) * 100)

education_table <- eda_data %>%
  count(education_3cat) %>%
  mutate(percent = n / sum(n) * 100)

employment_table <- eda_data %>%
  count(employment_3cat) %>%
  mutate(percent = n / sum(n) * 100)

employment_50_64_table <- eda_data %>%
  filter(age_group == "50-64") %>%
  count(employment_3cat) %>%
  mutate(percent = n / sum(n) * 100)

print(sample_summary)
print(age_group_table)
print(living_arrangement_table)
print(education_table)
print(employment_table)
print(employment_50_64_table)

write.csv(sample_summary, "outputs/tables/sample_summary.csv", row.names = FALSE)
write.csv(age_group_table, "outputs/tables/age_group_distribution.csv", row.names = FALSE)
write.csv(living_arrangement_table, "outputs/tables/living_arrangement_distribution.csv", row.names = FALSE)
write.csv(education_table, "outputs/tables/education_distribution.csv", row.names = FALSE)
write.csv(employment_table, "outputs/tables/employment_distribution.csv", row.names = FALSE)
write.csv(employment_50_64_table, "outputs/tables/employment_distribution_age_50_64.csv", row.names = FALSE)


# ============================================================
# Table 1: Baseline characteristics by sex
# ============================================================

# Helper functions --------------------------------------------------------

format_n <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}

format_n_pct <- function(n, denom) {
  if (is.na(n) || is.na(denom) || denom == 0) {
    return("-")
  }

  paste0(
    format_n(n),
    " (",
    sprintf("%.1f", 100 * n / denom),
    "%)"
  )
}

format_mean_sd <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return("-")
  }

  paste0(
    sprintf("%.1f", mean(x)),
    " (",
    sprintf("%.2f", sd(x)),
    ")"
  )
}

cat_cell <- function(data, var, level) {
  n <- sum(data[[var]] == level, na.rm = TRUE)
  denom <- nrow(data)
  format_n_pct(n, denom)
}

binary_cell <- function(data, condition) {
  n <- sum(condition, na.rm = TRUE)
  denom <- nrow(data)
  format_n_pct(n, denom)
}

mean_sd_cell <- function(data, var) {
  format_mean_sd(data[[var]])
}


# Split data by sex -------------------------------------------------------

eda_total <- eda_data
eda_men <- eda_data %>% filter(gender == "Male")
eda_women <- eda_data %>% filter(gender == "Female")

n_total <- nrow(eda_total)
n_men <- nrow(eda_men)
n_women <- nrow(eda_women)


# Create Table 1 ----------------------------------------------------------

table1_baseline <- tibble(
  Characteristic = c(
    "Sociodemographic",
    "    Age, mean (SD)",
    "    Age group, n (%)",
    "        50-64",
    "        65-74",
    "        75+",
    "    Female, n (%)",
    "    Living alone, n (%)",
    "    Employment status, n (%)",
    "        Employed",
    "        Retired",
    "        Other",
    "    Education, n (%)",
    "        Low",
    "        Medium",
    "        High",
    "Chronic condition count",
    "    Mean condition count (SD)",
    "    0 conditions, n (%)",
    "    1 condition, n (%)",
    "    2+ conditions, n (%)",
    "Depressive symptoms (Wave 6)",
    "    EURO-D >=4, n (%)",
    "    Mean EURO-D score (SD)"
  ),

  Total = c(
    "",
    mean_sd_cell(eda_total, "age2013"),
    "",
    cat_cell(eda_total, "age_group", "50-64"),
    cat_cell(eda_total, "age_group", "65-74"),
    cat_cell(eda_total, "age_group", "75+"),
    binary_cell(eda_total, eda_total$gender == "Female"),
    cat_cell(eda_total, "living_arrangement", "Living alone"),
    "",
    cat_cell(eda_total, "employment_3cat", "Employed"),
    cat_cell(eda_total, "employment_3cat", "Retired"),
    cat_cell(eda_total, "employment_3cat", "Other"),
    "",
    cat_cell(eda_total, "education_3cat", "Low"),
    cat_cell(eda_total, "education_3cat", "Medium"),
    cat_cell(eda_total, "education_3cat", "High"),
    "",
    mean_sd_cell(eda_total, "chronicw5"),
    binary_cell(eda_total, eda_total$chronicw5 == 0),
    binary_cell(eda_total, eda_total$chronicw5 == 1),
    binary_cell(eda_total, eda_total$chronicw5 >= 2),
    "",
    binary_cell(eda_total, eda_total$eurodcat_w6_num == 1),
    mean_sd_cell(eda_total, "eurod_w6_score")
  ),

  Men = c(
    "",
    mean_sd_cell(eda_men, "age2013"),
    "",
    cat_cell(eda_men, "age_group", "50-64"),
    cat_cell(eda_men, "age_group", "65-74"),
    cat_cell(eda_men, "age_group", "75+"),
    "-",
    cat_cell(eda_men, "living_arrangement", "Living alone"),
    "",
    cat_cell(eda_men, "employment_3cat", "Employed"),
    cat_cell(eda_men, "employment_3cat", "Retired"),
    cat_cell(eda_men, "employment_3cat", "Other"),
    "",
    cat_cell(eda_men, "education_3cat", "Low"),
    cat_cell(eda_men, "education_3cat", "Medium"),
    cat_cell(eda_men, "education_3cat", "High"),
    "",
    mean_sd_cell(eda_men, "chronicw5"),
    binary_cell(eda_men, eda_men$chronicw5 == 0),
    binary_cell(eda_men, eda_men$chronicw5 == 1),
    binary_cell(eda_men, eda_men$chronicw5 >= 2),
    "",
    binary_cell(eda_men, eda_men$eurodcat_w6_num == 1),
    mean_sd_cell(eda_men, "eurod_w6_score")
  ),

  Women = c(
    "",
    mean_sd_cell(eda_women, "age2013"),
    "",
    cat_cell(eda_women, "age_group", "50-64"),
    cat_cell(eda_women, "age_group", "65-74"),
    cat_cell(eda_women, "age_group", "75+"),
    "-",
    cat_cell(eda_women, "living_arrangement", "Living alone"),
    "",
    cat_cell(eda_women, "employment_3cat", "Employed"),
    cat_cell(eda_women, "employment_3cat", "Retired"),
    cat_cell(eda_women, "employment_3cat", "Other"),
    "",
    cat_cell(eda_women, "education_3cat", "Low"),
    cat_cell(eda_women, "education_3cat", "Medium"),
    cat_cell(eda_women, "education_3cat", "High"),
    "",
    mean_sd_cell(eda_women, "chronicw5"),
    binary_cell(eda_women, eda_women$chronicw5 == 0),
    binary_cell(eda_women, eda_women$chronicw5 == 1),
    binary_cell(eda_women, eda_women$chronicw5 >= 2),
    "",
    binary_cell(eda_women, eda_women$eurodcat_w6_num == 1),
    mean_sd_cell(eda_women, "eurod_w6_score")
  )
)


# Rename table columns with sample sizes ---------------------------------

names(table1_baseline) <- c(
  "Characteristic",
  paste0("Total (N=", format_n(n_total), ")"),
  paste0("Men (N=", format_n(n_men), ")"),
  paste0("Women (N=", format_n(n_women), ")")
)


# Print and save Table 1 --------------------------------------------------

print(table1_baseline)

write.csv(
  table1_baseline,
  "outputs/tables/table1_baseline_characteristics.csv",
  row.names = FALSE
)

saveRDS(
  table1_baseline,
  "outputs/tables/table1_baseline_characteristics.rds"
)


# Optional: save Table 1 as Word document ---------------------------------
# This only runs if the flextable package is installed.

if (requireNamespace("flextable", quietly = TRUE)) {

  table1_flextable <- flextable::flextable(table1_baseline) %>%
    flextable::theme_booktabs() %>%
    flextable::autofit()

  flextable::save_as_docx(
    "Table 1. Baseline characteristics of the analytical sample" =
      table1_flextable,
    path = "outputs/tables/table1_baseline_characteristics.docx"
  )
}




# Chronic disease burden --------------------------------------------------

chronic_count_table <- eda_data %>%
  count(chronic_cat) %>%
  mutate(percent = n / sum(n) * 100)

write.csv(
  chronic_count_table,
  "outputs/tables/chronic_condition_count_distribution.csv",
  row.names = FALSE
)

chronic_count_by_age_sex <- eda_data %>%
  count(age_group, gender, chronic_cat) %>%
  group_by(age_group, gender) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

chronic_count_age_sex_plot <- ggplot(
  chronic_count_by_age_sex,
  aes(x = age_group, y = n, fill = chronic_cat)
) +
  geom_col(position = "fill", width = 0.75) +
  facet_wrap(~ gender) +
  labs(
    title = "Distribution of chronic conditions (Wave 5)",
    x = "Age group",
    y = "Proportion",
    fill = "Number of chronic conditions"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

print(chronic_count_age_sex_plot)

ggsave(
  "outputs/figures/appendix_chronic_burden_by_age_sex.png",
  chronic_count_age_sex_plot,
  width = 8,
  height = 5,
  dpi = 300
)


# Individual chronic condition prevalence by sex --------------------------

condition_labels <- tibble(
  variable = chronic_disease_vars,
  condition = c(
    "Heart attack",
    "High blood pressure",
    "High blood cholesterol",
    "Stroke",
    "Diabetes",
    "Chronic lung disease",
    "Cancer",
    "Ulcer",
    "Parkinson's disease",
    "Cataracts",
    "Hip fracture",
    "Alzheimer's disease",
    "Other affective disorder",
    "Rheumatoid arthritis",
    "Osteoarthritis",
    "Other chronic condition"
  ),
  disease_group = c(
    "Cardiometabolic",
    "Cardiometabolic",
    "Cardiometabolic",
    "Cardiometabolic",
    "Cardiometabolic",
    "Respiratory",
    "Other",
    "Gastrointestinal",
    "Neurological",
    "Sensory",
    "Musculoskeletal",
    "Neurological",
    "Mental health",
    "Musculoskeletal",
    "Musculoskeletal",
    "Other"
  )
)

condition_long <- eda_data %>%
  select(mergeid, gender, all_of(chronic_disease_vars)) %>%
  pivot_longer(
    cols = all_of(chronic_disease_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  left_join(condition_labels, by = "variable")

condition_prevalence_by_sex <- condition_long %>%
  group_by(gender, condition) %>%
  summarise(
    n = sum(value == 1, na.rm = TRUE),
    total_n = n_distinct(mergeid),
    percent = n / total_n * 100,
    .groups = "drop"
  )

condition_prevalence_wide <- condition_prevalence_by_sex %>%
  select(condition, gender, percent) %>%
  pivot_wider(names_from = gender, values_from = percent) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

write.csv(
  condition_prevalence_wide,
  "outputs/tables/individual_chronic_condition_prevalence_by_sex.csv",
  row.names = FALSE
)



# Depressive symptoms by chronic disease burden and sex -------------------

depression_by_chronic_sex <- eda_data %>%
  group_by(chronic_cat, gender) %>%
  summarise(
    n = sum(!is.na(eurodcat_w6_num)),
    prevalence = mean(eurodcat_w6_num, na.rm = TRUE) * 100,
    .groups = "drop"
  )

write.csv(
  depression_by_chronic_sex,
  "outputs/tables/depressive_symptoms_by_chronic_burden_sex.csv",
  row.names = FALSE
)

depression_chronic_plot <- ggplot(
  depression_by_chronic_sex,
  aes(x = chronic_cat, y = prevalence, fill = gender)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Depressive symptoms by chronic disease burden and sex",
    x = "Number of chronic conditions",
    y = "Prevalence of depressive symptoms (%)",
    fill = "Sex"
  ) +
  theme_minimal()

print(depression_chronic_plot)

ggsave(
  "outputs/figures/figure_depressive_symptoms_by_chronic_burden_sex.png",
  depression_chronic_plot,
  width = 8,
  height = 5,
  dpi = 300
)


# Save key EDA objects ----------------------------------------------------

saveRDS(sample_summary, "outputs/tables/sample_summary.rds")
saveRDS(condition_prevalence_wide, "outputs/tables/condition_prevalence_by_sex.rds")
saveRDS(depression_by_chronic_sex, "outputs/tables/depression_by_chronic_burden_sex.rds")
