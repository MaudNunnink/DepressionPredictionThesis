
library(haven)
library(dplyr)

###########################################
## load all datasets to create dataframe ##
###########################################

# Functie om wave-data te laden en te filteren op land
load_wave <- function(file_path, country_code = 12) {
  data <- read_sav(file_path)
  data <- data %>% filter(country == country_code)
  return(data)
}

# 1.Generated Variables (**EURO-D caseness**) (nodig van zowel Wave 5 als Wave 6)
w5_gv <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 5/sharew5_rel9-0-0_gv_health.sav")
w6_gv <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 6/sharew6_rel9-0-0_gv_health.sav")

# 2.Chronic diseases (alleen wave 5)
health_conditions_w5 <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 5/sharew5_rel9-0-0_ph.sav")

# 3.coverscreen (**Age**, **Gender**, **Household Size**) (alleen wave 5)
w5_coverscreen <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 5/sharew5_rel9-0-0_cv_r.sav")

# 4.Sociodemographics (alleen Wave 5)
demographics_w5 <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 5/sharew5_rel9-0-0_dn.sav")

# 5.Isced module (**Isced 97, Isced 2011** (Standardized version of Educational level)) (alleen wave 5)
w5_isced <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 5/sharew5_rel9-0-0_gv_isced.sav")

# 6.EP module (**Current job situation**) (alleen wave 5)
w5_employment <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 5/sharew5_rel9-0-0_ep.sav")

# 7.Income (**household income (in an average month)**) (alleen wave 5)
income_w5 <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 5/sharew5_rel9-0-0_hh.sav")

# 8.Lifestyle (**Smoking**, **Alcohol** **Physical activity**) (alleen wave 5)
lifestyle_w5 <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 5/sharew5_rel9-0-0_br.sav")

# 9. Healthcare (om nursing home stay uit te halen)
w5_healthcare <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 5/sharew5_rel9-0-0_hc.sav")
w6_healthcare <- load_wave("C:/Users/Administrador/Desktop/SHARE wave 6/sharew6_rel9-0-0_hc.sav")



###########################################
### Filter all datasets op common id's  ###
###########################################


# Bepaal welke id's in beide datasets voorkomen
common_ids <- intersect(w5_gv$mergeid, w6_gv$mergeid)

n_common <- length(common_ids)
n_common

# 4321

# Filter wave 5 en wave 6 data zodat alleen die deelnemers erin blijven
w5_gv_panel <- w5_gv %>% filter(mergeid %in% common_ids)
w6_gv_panel <- w6_gv %>% filter(mergeid %in% common_ids)


# 4321 

### combineer W5-variabelen via inner join ###
panel_w5 <- w5_gv_panel %>%
  inner_join(health_conditions_w5 %>% filter(mergeid %in% common_ids), by = "mergeid") %>%
  inner_join(demographics_w5 %>% filter(mergeid %in% common_ids), by = "mergeid") %>%
  inner_join(w5_employment %>% filter(mergeid %in% common_ids), by = "mergeid") %>%
  inner_join(lifestyle_w5 %>% filter(mergeid %in% common_ids), by = "mergeid") %>%
  inner_join(income_w5 %>% filter(mergeid %in% common_ids), by = "mergeid") %>%
  inner_join(w5_coverscreen %>% filter(mergeid %in% common_ids), by = "mergeid") %>%
  inner_join(w5_isced %>% filter(mergeid %in% common_ids), by = "mergeid") %>% 
  inner_join(w5_healthcare %>% filter (mergeid %in% common_ids), by = "mergeid") %>%
  inner_join(w6_healthcare %>% filter (mergeid %in% common_ids), by = "mergeid") %>%
  dplyr::select(mergeid, eurodcat_w5 = eurodcat, dplyr::everything())



# panel_w5 heeft 4321 deelnemers



### voeg W6 outcome toe ###

panel_full <- panel_w5 %>%
  inner_join(
    w6_gv_panel %>% 
      dplyr::select(mergeid, eurodcat_w6 = eurodcat),
    by = "mergeid"
  ) %>%
  filter(!is.na(eurodcat_w6))


sum(is.na(w6_gv_panel$eurodcat))
# 85 mensen verwijderd die missings in EURO-D hebben
# nog 4,236 deelnemers



# mensen verwijderen die niet de juiste leeftijd hebben (die jonger dan 50 zijn)

panel_full <- panel_full %>%
  filter(age2013 >= 50)

# 4,142 observaties

# mensen verwijderen die een nursing home stay (in the past year) hadden

panel_full <- panel_full %>%
  filter(
    hc029_.x %in% c(1, 5),
    hc029_.y %in% c(1, 5)
  )

nrow(panel_full)
# nog 4094 deelnemers


# nu opslaan als .rds
saveRDS(panel_full, "panel_full.rds")

panel_full <- readRDS("panel_full.rds")


#######################################################









