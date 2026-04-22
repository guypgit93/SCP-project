library(dplyr)
library(tidyr)
library(knitr)

summary_by_region <- df_analysis %>%
  group_by(scotland) %>%
  summarise(
    N = n(),
    
    ghq_mean = mean(ghq_likert, na.rm = TRUE),
    age_mean = mean(age_dv, na.rm = TRUE),
    
    female = mean(female, na.rm = TRUE),
    employed = mean(employed, na.rm = TRUE),
    partnered = mean(partnered, na.rm = TRUE),
    
    larger_family = mean(larger_family, na.rm = TRUE),
    young_kids = mean(has_young_kids, na.rm = TRUE),
    
    disability = mean(disability, na.rm = TRUE),
    white_ethnicity = mean(white_ethnicity, na.rm = TRUE),
    
    benefit = mean(benefit_eligible, na.rm = TRUE),
    low_income = mean(low_income_pre, na.rm = TRUE)
  )

summary_by_region

summary_pre <- df_analysis %>%
  filter(post == 0) %>%
  group_by(scotland) %>%
  summarise(
    N = n(),
    
    ghq_mean = mean(ghq_likert, na.rm = TRUE),
    age_mean = mean(age_dv, na.rm = TRUE),
    
    female = mean(female, na.rm = TRUE),
    employed = mean(employed, na.rm = TRUE),
    partnered = mean(partnered, na.rm = TRUE),
    
    larger_family = mean(larger_family, na.rm = TRUE),
    young_kids = mean(has_young_kids, na.rm = TRUE),
    
    disability = mean(disability, na.rm = TRUE),
    white_ethnicity = mean(white_ethnicity, na.rm = TRUE)
  )

summary_pre


summary_table <- df_analysis %>%
  mutate(scotland = ifelse(scotland == 1, "Scotland", "England")) %>%
  group_by(scotland) %>%
  summarise(
    N = n(),
    GHQ = mean(ghq_likert, na.rm = TRUE),
    Age = mean(age_dv, na.rm = TRUE),
    Female = mean(female, na.rm = TRUE),
    Employed = mean(employed, na.rm = TRUE),
    Partnered = mean(partnered, na.rm = TRUE),
    `Large family` = mean(larger_family, na.rm = TRUE),
    `Young kids` = mean(has_young_kids, na.rm = TRUE),
    Disability = mean(disability, na.rm = TRUE),
    `White ethnicity` = mean(white_ethnicity, na.rm = TRUE),
    `Low income` = mean(low_income_pre, na.rm = TRUE)
  ) %>%
  pivot_longer(-scotland, names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = scotland, values_from = Value)

kable(
  summary_table,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  caption = "Summary statistics: England and Scotland"
)

library(dplyr)

attrition <- tibble(
  Step = character(),
  N = integer()
)

add_row_count <- function(data, label) {
  tibble(Step = label, N = nrow(data))
}

# Build attrition table to see how the sample size shrinks

# Start from df, since df_analysis is built from df
attrition <- bind_rows(
  attrition,
  add_row_count(df, "All observations")
)

df_1 <- df %>%
  filter(age_dv >= 18, age_dv <= 65)
attrition <- bind_rows(attrition, add_row_count(df_1, "Age 18-65"))

df_2 <- df_1 %>%
  filter(country %in% c(1, 3))
attrition <- bind_rows(attrition, add_row_count(df_2, "England + Scotland"))

df_3 <- df_2 %>%
  filter(!is.na(nkids015), nkids015 > 0)
attrition <- bind_rows(attrition, add_row_count(df_3, "Parents (children under 16)"))

df_4 <- df_3 %>%
  filter(!is.na(ghq_likert))
attrition <- bind_rows(attrition, add_row_count(df_4, "Non-missing GHQ"))

df_5 <- df_4 %>%
  filter(
    !is.na(scotland),
    !is.na(post),
    !is.na(year),
    !is.na(month)
  )
attrition <- bind_rows(attrition, add_row_count(df_5, "Non-missing timing/geography vars"))

df_6 <- df_5 %>%
  filter(
    !is.na(female),
    !is.na(employed),
    !is.na(partnered),
    !is.na(disability),
    !is.na(white_ethnicity)
  )
attrition <- bind_rows(attrition, add_row_count(df_6, "Non-missing controls"))

df_7 <- df_6 %>%
  filter(
    !(year == 2020 & month >= 3),
    !(year == 2021 & month <= 6)
  )
attrition <- bind_rows(attrition, add_row_count(df_7, "Exclude COVID disruption period"))

# This should now match df_analysis
attrition <- bind_rows(attrition, add_row_count(df_analysis, "Final analysis sample"))

treated_sample <- df_analysis %>%
  filter(scotland == 1, post == 1)
attrition <- bind_rows(attrition, add_row_count(treated_sample, "Treated observations (Scotland × post)"))

attrition