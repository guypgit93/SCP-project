library(dplyr)
library(lmtest)
library(sandwich)
library(ggplot2)
library(stargazer)
library(lmtest)

## Robustness Checks

# 1. Drop 2022
df_robust <- df_analysis %>%
  filter(year != 2022) %>%
  mutate(
    post_clean = ifelse(year >= 2023, 1, 0)
  )

reg_robust1.all <- lm(
  ghq_likert ~ post_clean * scotland + year.f + region.f,
  data = df_robust
)

reg_robust1 <- coeftest(
  reg_robust1.all,
  vcovCL(reg_robust1.all, cluster = ~ gor_dv)
)

reg_robust2.all <- lm(
  ghq_likert ~ post_clean * scotland +
    female + employed + partnered +
    larger_family + has_young_kids +
    white_ethnicity + disability +
    age_band +
    year.f + region.f,
  data = df_robust
)

reg_robust2 <- coeftest(
  reg_robust2.all,
  vcovCL(reg_robust2.all, cluster = ~ gor_dv)
)

se1 <- sqrt(diag(vcovCL(reg_robust1.all, cluster = ~ gor_dv)))
se2 <- sqrt(diag(vcovCL(reg_robust2.all, cluster = ~ gor_dv)))

stargazer(reg_robust1.all, reg_robust2.all,
          se = list(se1, se2),
          keep = "post_clean:scotland",
          type = "text")

# Results do not change.

# 2. Use low income as eligibility

df_lowinc <- df_analysis %>%
  filter(eligible_income == 1)

reg_low1.all <- lm(
  ghq_likert ~ post * scotland + year.f + region.f,
  data = df_lowinc
)

se_low1 <- sqrt(diag(vcovCL(reg_low1.all, cluster = ~ gor_dv)))

reg_low2.all <- lm(
  ghq_likert ~ post * scotland +
    female + employed + partnered +
    larger_family + has_young_kids +
    white_ethnicity + disability +
    age_band +
    year.f + region.f,
  data = df_lowinc
)

se_low2 <- sqrt(diag(vcovCL(reg_low2.all, cluster = ~ gor_dv)))

stargazer(reg_low1.all, reg_low2.all,
          se = list(se_low1, se_low2),
          keep = "post:scotland",
          type = "text")

# Event Study

df_lowinc_es <- df_analysis %>%
  filter(
    eligible_income == 1,
    year != 2020
  ) %>%
  mutate(
    event_time = year - 2023   # 2023 = event year 0
  )

# Check support
table(df_lowinc_es$year, df_lowinc_es$scotland)

df_lowinc_es <- df_lowinc_es %>%
  mutate(
    evt_m5 = ifelse(event_time == -5 & scotland == 1, 1, 0),  # 2018
    evt_m4 = ifelse(event_time == -4 & scotland == 1, 1, 0),  # 2019
    evt_m1 = ifelse(event_time == -1 & scotland == 1, 1, 0),  # 2022 transition
    evt_0  = ifelse(event_time ==  0 & scotland == 1, 1, 0),  # 2023
    evt_1  = ifelse(event_time ==  1 & scotland == 1, 1, 0)   # 2024
  )

reg_low_es.all <- lm(
  ghq_likert ~
    evt_m5 + evt_m4 + evt_m1 + evt_0 + evt_1 +
    female + employed + partnered +
    larger_family + has_young_kids +
    white_ethnicity + disability +
    age_band + region.f + year.f,
  data = df_lowinc_es
)

V_low_es  <- vcovCL(reg_low_es.all, cluster = ~ gor_dv)
se_low_es <- sqrt(diag(V_low_es))

event_names <- grep("^evt_", names(coef(reg_low_es.all)), value = TRUE)

stargazer(reg_low_es.all,
          se = list(se_low_es),
          keep = event_names,
          type = "text")


coef_vals <- coef(reg_low_es.all)[event_names]
se_vals   <- se_low_es[event_names]

fig_dat_low <- data.frame(
  year = c(2018, 2019, 2021, 2022, 2023, 2024),
  coefs = c(
    coef_vals["evt_m5"],
    coef_vals["evt_m4"],
    0,
    coef_vals["evt_m1"],
    coef_vals["evt_0"],
    coef_vals["evt_1"]
  ),
  se = c(
    se_vals["evt_m5"],
    se_vals["evt_m4"],
    NA,
    se_vals["evt_m1"],
    se_vals["evt_0"],
    se_vals["evt_1"]
  )
)

fig_dat_low <- data.frame(
  year  = c(2018, 2019, 2021, 2023, 2024),
  coefs = c(coef_vals["evt_m5"],
            coef_vals["evt_m4"],
            0,
            coef_vals["evt_0"],
            coef_vals["evt_1"]),
  se    = c(se_vals["evt_m5"],
            se_vals["evt_m4"],
            NA,
            se_vals["evt_0"],
            se_vals["evt_1"])
)

ggplot(fig_dat_low, aes(x = year, y = coefs)) +
  geom_hline(yintercept = 0, alpha = 0.4, linewidth = 0.5) +
  geom_line(data = subset(fig_dat_low, year <= 2021),
            linewidth = 0.6, linetype = "longdash") +
  geom_line(data = subset(fig_dat_low, year >= 2021),
            linewidth = 0.6, linetype = "longdash") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", alpha = 0.6) +
  geom_point(size = 2) +
  geom_errorbar(
    data = subset(fig_dat_low, !is.na(se)),
    aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se),
    width = 0.15
  ) +
  scale_x_continuous(breaks = c(2018, 2019, 2021, 2023, 2024)) +
  theme_classic() +
  xlab("Year") +
  ylab("Effect on GHQ relative to 2021")


# 3. Use GHQ Caseness as outcome
reg_case1.all <- lm(
  ghq_caseness ~ post * scotland + year.f + region.f,
  data = df_analysis
)

se_case1 <- sqrt(diag(vcovCL(reg_case1.all, cluster = ~ gor_dv)))

reg_case2.all <- lm(
  ghq_caseness ~ post * scotland +
    female + employed + partnered +
    larger_family + has_young_kids +
    white_ethnicity + disability +
    age_band +
    year.f + region.f,
  data = df_analysis
)

se_case2 <- sqrt(diag(vcovCL(reg_case2.all, cluster = ~ gor_dv)))

stargazer(reg_case1.all, reg_case2.all,
          se = list(se_case1, se_case2),
          keep = "post:scotland",
          type = "text")

# 4. Restrict control sample to North of England

df_north <- df_analysis %>%
  filter(
    scotland == 1 | gor_dv %in% c(1, 2, 3)
  )

table(df_north$gor_dv)
table(df_north$scotland)
nrow(df_north)

reg_north1.all <- lm(
  ghq_likert ~ post * scotland + year.f + region.f,
  data = df_north
)

se_north1 <- sqrt(diag(vcovCL(reg_north1.all, cluster = ~ gor_dv)))

reg_north2.all <- lm(
  ghq_likert ~ post * scotland +
    female + employed + partnered +
    larger_family + has_young_kids +
    white_ethnicity + disability +
    age_band +
    year.f + region.f,
  data = df_north
)

se_north2 <- sqrt(diag(vcovCL(reg_north2.all, cluster = ~ gor_dv)))

stargazer(reg_north1.all, reg_north2.all,
          se = list(se_north1, se_north2),
          keep = "post:scotland",
          type = "text")