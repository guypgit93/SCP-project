main_models <- list(
  reg2.all,
  reg_partner,
  reg_kids,
  reg_family
)

main_ses <- list(
  sqrt(diag(vcovCL(reg2.all, cluster = ~ gor_dv))),
  sqrt(diag(vcovCL(reg_partner, cluster = ~ gor_dv))),
  sqrt(diag(vcovCL(reg_kids, cluster = ~ gor_dv))),
  sqrt(diag(vcovCL(reg_family, cluster = ~ gor_dv)))
)

stargazer(main_models,
          se = main_ses,
          keep = c(
            "post:scotland",
            "post:scotland:partnered",
            "post:scotland:has_young_kids",
            "post:scotland:larger_family"
          ),
          column.labels = c(
            "Main",
            "Partnered",
            "Young kids",
            "Large family"
          ),
          dep.var.labels = "GHQ Likert",
          type = "latex",
          digits = 3,
          notes = "Standard errors clustered at the regional level.")