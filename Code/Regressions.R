library(sandwich)
library(lmtest)

# Create factors
df_analysis$year.f   <- factor(df_analysis$year)
df_analysis$region.f <- factor(df_analysis$gor_dv)
df_analysis$age_band <- factor(df_analysis$age_band)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#          Main DiD              #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# (1) Baseline: minimal FE model
reg1.all <- lm(
  ghq_likert ~ post*scotland + year.f + region.f,
  data = df_analysis
)

reg1 <- coeftest(reg1.all, vcovCL(reg1.all, cluster = ~ gor_dv))

se_cluster <- sqrt(diag(vcovCL(reg1.all, cluster = ~ gor_dv)))

stargazer(reg1.all,
          se = list(se_cluster),
          type = "text",
          keep = "post:scotland")

# Full controls
reg2.all <- lm(
  ghq_likert ~ post*scotland +
    female + employed + partnered +
    larger_family + has_young_kids +
    white_ethnicity + disability +
    age_band +
    year.f + region.f,
  data = df_analysis
)

se_cluster2 <- sqrt(diag(vcovCL(reg2.all, cluster = ~ gor_dv)))

stargazer(reg2.all,
          se = list(se_cluster2),
          type = "text",
          keep = "post:scotland")

se1 <- sqrt(diag(vcovCL(reg1.all, cluster = ~ gor_dv)))
se2 <- sqrt(diag(vcovCL(reg2.all, cluster = ~ gor_dv)))

stargazer(reg1.all, reg2.all,
          se = list(se1, se2),
          keep = "post:scotland",
          type = "text")


# Heterogeneity 

run_het <- function(var) {
  formula <- as.formula(
    paste0(
      "ghq_likert ~ post*scotland*", var, " + ",
      "female + employed + partnered + larger_family + ",
      "has_young_kids + white_ethnicity + disability + age_band + ",
      "year.f + region.f"
    )
  )
  
  lm(formula, data = df_analysis) 
}

# A) Partnered

reg_partner <- run_het("partnered")

# B) Young Children

reg_kids <- run_het("has_young_kids")

# C) Larger families
reg_family <- run_het("larger_family")


se_partner <- sqrt(diag(vcovCL(reg_partner, cluster = ~ gor_dv)))
se_kids    <- sqrt(diag(vcovCL(reg_kids,    cluster = ~ gor_dv)))
se_family  <- sqrt(diag(vcovCL(reg_family,  cluster = ~ gor_dv)))

library(stargazer)

stargazer(reg_partner, reg_kids, reg_family,
          se = list(se_partner, se_kids, se_family),
          keep = "post:scotland",
          type = "text")