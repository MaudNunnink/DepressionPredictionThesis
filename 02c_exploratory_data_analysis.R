

# ============================================================
# 02c_exploratory_data_analysis.R
# Descriptive statistics and exploratory figures
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
      age2013 >= 50 & age2013 <= 64 ~ "50–64",
      age2013 >= 65 & age2013 <= 74 ~ "65–74",
      age2013 >= 75 ~ "75+",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("50–64", "65–74", "75+")),
    
    living_arrangement = case_when(
      hhsize == 1 ~ "Living alone",
      hhsize > 1 ~ "Living with others",
      TRUE ~ NA_character_
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
    
    educational_level = na_if(educational_level, 97),
    
    education_3cat = case_when(
      educational_level %in% c(0, 1, 2) ~ "low",
      educational_level %in% c(3, 4)    ~ "mid",
      educational_level %in% c(5, 6)    ~ "high",
      TRUE ~ NA_character_
    ),
    education_3cat = factor(education_3cat, levels = c("low", "mid", "high")),
    
    employment_status = as.numeric(as.character(employment_status)),
    employment_status = na_if(employment_status, 97),
    
    employment_3cat = case_when(
      employment_status == 2 ~ "employed",
      employment_status == 1 ~ "retired",
      employment_status %in% c(3, 4, 5) ~ "not_working",
      TRUE ~ NA_character_
    ),
    employment_3cat = factor(
      employment_3cat,
      levels = c("employed", "retired", "not_working")
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
  filter(age_group == "50–64") %>%
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


# Figure: age distribution ------------------------------------------------

mean_age <- mean(eda_data$age2013, na.rm = TRUE)
sd_age <- sd(eda_data$age2013, na.rm = TRUE)

line_data <- tibble(
  xintercept = c(
    mean_age,
    mean_age - sd_age,
    mean_age + sd_age
  ),
  line_type = c(
    "Mean",
    "±1 SD",
    "±1 SD"
  )
)

age_distribution_plot <- ggplot(eda_data, aes(x = age2013)) +
  
  geom_histogram(
    bins = 8,
    fill = "lightblue",
    color = "black",
    alpha = 0.7
  ) +
  
  geom_vline(
    data = line_data,
    aes(
      xintercept = xintercept,
      color = line_type,
      linetype = line_type
    ),
    linewidth = 1
  ) +
  
  scale_color_manual(
    values = c(
      "Mean" = "red",
      "±1 SD" = "darkgreen"
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      "Mean" = "dashed",
      "±1 SD" = "dotted"
    )
  ) +
  
  labs(
    title = "Age distribution of the study population",
    x = "Age",
    y = "Frequency",
    color = NULL,
    linetype = NULL
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = c(0.83, 0.82),
    legend.background = element_rect(
      fill = "white",
      color = "black"
    )
  )

print(age_distribution_plot)

ggsave(
  "outputs/figures/figure_age_distribution.png",
  age_distribution_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# Appendix figure: age distribution by sex --------------------------------

age_by_sex_plot <- ggplot(eda_data, aes(x = gender, y = age2013)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.shape = NA) +
  labs(
    title = "Age distribution by sex",
    x = "Sex",
    y = "Age"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/appendix_age_distribution_by_sex.png",
  age_by_sex_plot,
  width = 7,
  height = 5,
  dpi = 300
)

age_by_sex_table <- eda_data %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    mean_age = mean(age2013, na.rm = TRUE),
    sd_age = sd(age2013, na.rm = TRUE),
    median_age = median(age2013, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(age_by_sex_table, "outputs/tables/age_by_sex.csv", row.names = FALSE)



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
    "Heart attack", "High blood pressure", "High blood cholesterol",
    "Stroke", "Diabetes", "Chronic lung disease", "Cancer", "Ulcer",
    "Parkinson's disease", "Cataracts", "Hip fracture", "Alzheimer's disease",
    "Other affective disorder", "Rheumatoid arthritis", "Osteoarthritis",
    "Other chronic condition"
  ),
  disease_group = c(
    "Cardiometabolic", "Cardiometabolic", "Cardiometabolic",
    "Cardiometabolic", "Cardiometabolic", "Respiratory", "Other",
    "Gastrointestinal", "Neurological", "Sensory", "Musculoskeletal",
    "Neurological", "Mental health", "Musculoskeletal",
    "Musculoskeletal", "Other"
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

# Disease group prevalence ------------------------------------------------

disease_group_prevalence <- condition_long %>%
  mutate(value = ifelse(value == 1, 1, 0)) %>%
  group_by(mergeid, gender, disease_group) %>%
  summarise(has_group = max(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(gender, disease_group) %>%
  summarise(
    n = sum(has_group, na.rm = TRUE),
    total_n = n_distinct(mergeid),
    percent = n / total_n * 100,
    .groups = "drop"
  )

write.csv(
  disease_group_prevalence,
  "outputs/tables/disease_group_prevalence_by_sex.csv",
  row.names = FALSE
)

disease_group_plot <- ggplot(
  disease_group_prevalence,
  aes(x = disease_group, y = percent, fill = disease_group)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ gender) +
  labs(
    title = "Prevalence of disease groups by sex",
    x = NULL,
    y = "Prevalence (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "outputs/figures/appendix_disease_group_prevalence_by_sex.png",
  disease_group_plot,
  width = 9,
  height = 5,
  dpi = 300
)

# Depressive symptoms by wave, age group, and sex -------------------------

depression_long <- eda_data %>%
  select(mergeid, gender, age_group, eurodcat_w5_num, eurodcat_w6_num) %>%
  pivot_longer(
    cols = c(eurodcat_w5_num, eurodcat_w6_num),
    names_to = "wave",
    values_to = "depressive_symptoms"
  ) %>%
  mutate(
    wave = recode(
      wave,
      eurodcat_w5_num = "Wave 5",
      eurodcat_w6_num = "Wave 6"
    )
  )

depression_age_sex <- depression_long %>%
  group_by(wave, age_group, gender) %>%
  summarise(
    n = sum(!is.na(depressive_symptoms)),
    prevalence = mean(depressive_symptoms, na.rm = TRUE) * 100,
    .groups = "drop"
  )

write.csv(
  depression_age_sex,
  "outputs/tables/depressive_symptoms_by_wave_age_sex.csv",
  row.names = FALSE
)

depression_age_sex_plot <- ggplot(
  depression_age_sex,
  aes(x = age_group, y = prevalence, fill = gender)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  facet_wrap(~ wave) +
  labs(
    title = "Depressive symptom prevalence by age group, sex, and wave",
    x = "Age group",
    y = "Prevalence (%)",
    fill = "Sex"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/appendix_depressive_symptoms_by_wave_age_sex.png",
  depression_age_sex_plot,
  width = 9,
  height = 5,
  dpi = 300
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

ggsave(
  "outputs/figures/figure_depressive_symptoms_by_chronic_burden_sex.png",
  depression_chronic_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# print all plots 

print(age_distribution_plot)
print(age_by_sex_plot)
#print(chronic_count_age_sex_plot)
print(disease_group_plot)
print(depression_age_sex_plot)
print(depression_chronic_plot)

# Save key EDA objects ----------------------------------------------------

saveRDS(sample_summary, "outputs/tables/sample_summary.rds")
saveRDS(age_by_sex_table, "outputs/tables/age_by_sex.rds")
saveRDS(condition_prevalence_wide, "outputs/tables/condition_prevalence_by_sex.rds")
saveRDS(depression_by_chronic_sex, "outputs/tables/depression_by_chronic_burden_sex.rds")