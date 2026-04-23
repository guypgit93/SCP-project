library(haven)
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)
library(ggplot2)

df_longdata <- readRDS(
  path.expand("~/python-venv-demo/SCP-project/Data/UKHLS_long_hijklmno.rds")
)

# Clean variables and construct main analysis variables
df <- df_longdata %>%
  mutate(
    # Outcomes
    ghq_likert   = ifelse(scghq1_dv < 0, NA, scghq1_dv),
    ghq_caseness = ifelse(scghq2_dv < 0, NA, scghq2_dv),
    
    # Geography and timing
    country    = ifelse(country < 0, NA, country),
    intdaty_dv = ifelse(intdaty_dv < 0, NA, intdaty_dv),
    intdatm_dv = ifelse(intdatm_dv < 0, NA, intdatm_dv),
    
    # Children
    nkids05   = ifelse(nkids05 < 0, NA, nkids05),
    nkids615  = ifelse(nkids615 < 0, NA, nkids615),
    nkids015  = ifelse(nkids015 < 0, NA, nkids015),
    nkids_dv  = ifelse(nkids_dv < 0, NA, nkids_dv),
    
    # Benefits
    benbase1 = ifelse(benbase1 < 0, NA, benbase1),
    benbase2 = ifelse(benbase2 < 0, NA, benbase2),
    benbase3 = ifelse(benbase3 < 0, NA, benbase3),
    benbase4 = ifelse(benbase4 < 0, NA, benbase4),
    benctc   = ifelse(benctc < 0, NA, benctc),
    
    # Demographics / controls
    age_dv      = ifelse(age_dv < 0, NA, age_dv),
    sex_dv      = ifelse(sex_dv < 0, NA, sex_dv),
    jbstat      = ifelse(jbstat < 0, NA, jbstat),
    hhtype_dv   = ifelse(hhtype_dv < 0, NA, hhtype_dv),
    marstat_dv  = ifelse(marstat_dv < 0, NA, marstat_dv),
    ethn_dv     = ifelse(ethn_dv < 0, NA, ethn_dv),
    health      = ifelse(health < 0, NA, health),
    hh_income   = ifelse(fihhmnnet1_dv < 0, NA, fihhmnnet1_dv),
    
    # Timing vars
    year  = intdaty_dv,
    month = intdatm_dv,
    
    # Labour market
    employed = case_when(
      is.na(jbstat) ~ NA_real_,
      jbstat %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    
    # Gender
    female = case_when(
      is.na(sex_dv) ~ NA_real_,
      sex_dv == 2 ~ 1,
      TRUE ~ 0
    ),
    
    # Ethnicity
    white_ethnicity = case_when(
      is.na(ethn_dv) ~ NA_real_,
      ethn_dv %in% c(1, 2, 4) ~ 1,
      TRUE ~ 0
    ),
    
    # Household structure
    partnered = case_when(
      is.na(marstat_dv) ~ NA_real_,
      marstat_dv %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    
    # Scotland dummy
    scotland = case_when(
      is.na(country) ~ NA_real_,
      country == 3 ~ 1,
      TRUE ~ 0
    ),
    
    # Strict benefit-based proxy (keep for robustness only)
    benefit_eligible = case_when(
      is.na(benbase1) & is.na(benbase2) & is.na(benbase4) ~ NA_real_,
      benbase4 == 1 | benbase1 == 1 | benbase2 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    # Children under 16
    child_eligible = case_when(
      is.na(nkids015) ~ NA_real_,
      nkids015 > 0 ~ 1,
      TRUE ~ 0
    ),
    
    # Strict eligibility (keep for robustness only)
    eligible = case_when(
      is.na(benefit_eligible) | is.na(child_eligible) ~ NA_real_,
      benefit_eligible == 1 & child_eligible == 1 ~ 1,
      TRUE ~ 0
    ),
    
    # Main broad eligibility: all parents with children under 16
    eligible_parent = case_when(
      is.na(nkids015) ~ NA_real_,
      nkids015 > 0 ~ 1,
      TRUE ~ 0
    ),
    
    # Policy timing: full rollout / expansion from Nov 2022
    post = case_when(
      is.na(year) | is.na(month) ~ NA_real_,
      year > 2022 | (year == 2022 & month >= 11) ~ 1,
      TRUE ~ 0
    ),
    
    # Child/family controls
    has_young_kids = case_when(
      is.na(nkids05) ~ NA_real_,
      nkids05 > 0 ~ 1,
      TRUE ~ 0
    ),
    
    larger_family = case_when(
      is.na(nkids015) ~ NA_real_,
      nkids015 >= 3 ~ 1,
      TRUE ~ 0
    ),
    
    # Broad age bands
    age_band = case_when(
      is.na(age_dv) ~ NA_character_,
      age_dv <= 25 ~ "18-25",
      age_dv <= 30 ~ "26-30",
      age_dv <= 35 ~ "31-35",
      age_dv <= 40 ~ "36-40",
      TRUE ~ "41-65"
    ),
    
    # Long-term illness / disability dummy
    disability = case_when(
      is.na(health) ~ NA_real_,
      health == 1 ~ 1,
      health == 2 ~ 0,
      TRUE ~ NA_real_
    ),
  )

# Pre-treatment income measure (keep for robustness only)
df <- df %>%
  group_by(pidp) %>%
  mutate(
    hh_income_pre = mean(hh_income[year < 2022 & !(year == 2020 & month >= 3)], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    hh_income_pre = ifelse(is.nan(hh_income_pre), NA, hh_income_pre)
  )

median_pre <- median(df$hh_income_pre, na.rm = TRUE)
poverty_line_pre <- 0.6 * median_pre

df <- df %>%
  mutate(
    low_income_pre = case_when(
      is.na(hh_income_pre) ~ NA_real_,
      hh_income_pre <= poverty_line_pre ~ 1,
      TRUE ~ 0
    ),
    eligible_income = case_when(
      is.na(nkids015) | is.na(low_income_pre) ~ NA_real_,
      nkids015 > 0 & low_income_pre == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Analysis sample: working-age parents in Scotland vs England only
df_analysis <- df %>%
  filter(
    age_dv >= 18,
    age_dv <= 65,
    
    # Main sample = all parents
    !is.na(nkids015),
    nkids015 > 0,
    
    # Geography: England and Scotland only
    country %in% c(1, 3),
    
    # Core variables
    !is.na(ghq_likert),
    !is.na(scotland),
    !is.na(post),
    !is.na(year),
    !is.na(month),
    
    # Controls
    !is.na(female),
    !is.na(employed),
    !is.na(partnered),
    !is.na(disability),
    !is.na(white_ethnicity),
    
    # Drop COVID disruption period
    !(year == 2020 & month >= 3),
    !(year == 2021 & month <= 6)
  ) %>%
  mutate(
    has_young_kids = case_when(
      is.na(nkids05) ~ NA_real_,
      nkids05 > 0 ~ 1,
      TRUE ~ 0
    ),
    year.f = factor(year),
    region.f = factor(gor_dv),
    age_band = factor(age_band)
  )