library(dplyr)
library(lmtest)
library(sandwich)
library(ggplot2)
library(stargazer)

#--------------------------------------------------#
# 1. Start from your parent-only analysis sample   #
#--------------------------------------------------#

df_es <- df_analysis %>%
  filter(year != 2020) %>%   # drop COVID year
  mutate(
    event_time = year - 2023   # 2023 = event year 0
  )

# Check support
table(df_es$year, df_es$scotland)

#--------------------------------------------------#
# 2. Create yearly lead/lag dummies                #
#    Omit event_time = -2  (i.e. 2021)             #
#--------------------------------------------------#

df_es <- df_es %>%
  mutate(
    evt_m5 = ifelse(event_time == -5 & scotland == 1, 1, 0),  # 2018
    evt_m4 = ifelse(event_time == -4 & scotland == 1, 1, 0),  # 2019
    # omit event_time == -2  (2021) as baseline
    evt_m1  = ifelse(event_time ==  -1 & scotland == 1, 1, 0),  # 2022
    evt_0  = ifelse(event_time ==  0 & scotland == 1, 1, 0),  # 2023
    evt_1  = ifelse(event_time ==  1 & scotland == 1, 1, 0),  # 2024
  )

df_es <- df_es %>% mutate(year.f = droplevels(factor(year))) #Drops 2020 FE

reg_es.all <- lm(
  ghq_likert ~
    evt_m5 + evt_m4 + evt_m1 + evt_0 + evt_1 +
    female + employed + partnered +
    larger_family + has_young_kids +
    white_ethnicity + disability +
    age_band + region.f + year.f,
  data = df_es
)

se_es <- sqrt(diag(vcovCL(reg_es.all, cluster = ~ gor_dv)))

event_names <- grep("^evt_", names(coef(reg_es.all)), value = TRUE)

stargazer(reg_es.all,
          se = list(se_es),
          keep = event_names,
          type = "text")

coef_vals <- coef(reg_es.all)[event_names]
se_vals   <- se_es[event_names]

fig_dat <- data.frame(
  year = c(2018, 2019, 2021, 2022, 2023, 2024),
  coefs = c(
    coef_vals["evt_m5"],
    coef_vals["evt_m4"],
    0,                      # omitted base year
    coef_vals["evt_m1"],
    coef_vals["evt_0"],
    coef_vals["evt_1"]
  ),
  se = c(
    se_vals["evt_m5"],
    se_vals["evt_m4"],
    NA,                     # no CI for omitted base year
    se_vals["evt_m1"],
    se_vals["evt_0"],
    se_vals["evt_1"]
  )
)

ggplot(fig_dat, aes(x = year, y = coefs)) +
  geom_hline(yintercept = 0, alpha = 0.4, linewidth = 0.5) +
  geom_vline(xintercept = 2022, linetype = "dashed", alpha = 0.6) +
  geom_errorbar(
    data = subset(fig_dat, !is.na(se)),
    aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se),
    width = 0.15
  ) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.5, linetype = "longdash") +
  scale_x_continuous(breaks = c(2018, 2019, 2021, 2022, 2023, 2024)) +
  theme_classic() +
  xlab("Year") +
  ylab("Effect on GHQ relative to 2021")