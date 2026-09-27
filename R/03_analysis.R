main <- rd_est(eh, "emp")
hon_emp <- honest_est(eh, "emp")
hon_pov <- honest_est(eh, "pext0")
M_emp <- hon_emp$M
M_pov <- hon_pov$M
h0 <- main$h
no_pens <- filter(eh, pension == 0)

first_stage <- bind_rows(
  "Non-labor income (Bs/month)" = rd_est(eh, "ynolab"),
  "Renta Dignidad income (Bs/month)" = rd_est(eh, "rd_inc"),
  "Contributory pension income (Bs/month)" = rd_est(eh, "pens_inc"),
  "Any non-labor income" = rd_est(eh, "anynl"),
  .id = "outcome"
)

takeup <- eh |>
  filter(age >= 60) |>
  summarise(
    first_year = weighted.mean(rd[age == 60], w[age == 60]),
    age61 = weighted.mean(rd[age == 61], w[age == 61]),
    age63plus = weighted.mean(rd[age >= 63], w[age >= 63]),
    urban = weighted.mean(rd[rural == 0], w[rural == 0]),
    rural = weighted.mean(rd[rural == 1], w[rural == 1]),
    poor = weighted.mean(rd[p0 %in% 1], w[p0 %in% 1]),
    nonpoor = weighted.mean(rd[p0 %in% 0], w[p0 %in% 0])
  )

employment <- bind_rows(
  "Main: local linear, MSE-optimal bandwidth" = main,
  "Year fixed effects" = rd_est(eh, "emp", covs = "yr"),
  "Covariates: year, sex, schooling, area, indigenous" = rd_est(eh, "emp", covs = c("yr", "female", "educ", "rural", "indig")),
  "Honest CI (Armstrong-Koles\u00e1r bound)" = hon_emp,
  "Local randomization, age 59 vs 60" = lr_est(eh, "emp"),
  "Donut: excluding age 60" = rd_est(filter(eh, age != 60), "emp"),
  "Threshold at age 61" = rd_est(eh, "emp", c = 1),
  "Fuzzy: receipt as treatment" = rd_est(eh, "emp", fuzzy = "rd"),
  "Excluding contributory pensioners" = rd_est(no_pens, "emp"),
  .id = "spec"
)

groups <- list(
  "Education, 7+ years" = quote(educ7 == 1), "Education, 0-6 years" = quote(educ7 == 0),
  "Urban" = quote(rural == 0), "Rural" = quote(rural == 1),
  "Male" = quote(female == 0), "Female" = quote(female == 1)
)
rd_benefit <- 4550 / 12

subgroups <- imap_dfr(groups, \(g, nm) {
  d <- filter(eh, !!g)
  earn <- with(filter(d, between(age, 55, 64), emp == 1, ylab > 0), median(ylab))
  bind_cols(group = nm, earnings = earn, replacement = rd_benefit / earn, rd_est(d, "emp"))
})
pair_tests <- map_dfr(seq(1, 5, 2), \(i) bind_cols(pair = sub(",.*| .*", "", subgroups$group[i]), diff_test(subgroups[i, ], subgroups[i + 1, ])))

pension_age <- eh |>
  filter(between(age, 56, 63)) |>
  group_by(educ7, age) |>
  summarise(share = weighted.mean(pension, w), .groups = "drop")

pension_rd_educ7 <- rd_est(filter(eh, educ7 == 1), "pension")

excl <- imap_dfr(list("All respondents" = quote(TRUE), "Education, 7+ years" = quote(educ7 == 1), "Education, 0-6 years" = quote(educ7 == 0)), \(g, nm) {
  a <- rd_est(filter(eh, !!g), "emp")
  b <- rd_est(filter(no_pens, !!g), "emp")
  tibble(sample = nm, full = a$est, p_full = a$p, restricted = b$est, p_restricted = b$p, lo_r = b$lo, hi_r = b$hi, change = b$est / a$est - 1)
})
did_educ <- diff_test(subgroups[1, ], subgroups[2, ])

work_type <- bind_rows(
  "Employed (any)" = main,
  "Salaried employment" = rd_est(eh, "salaried"),
  "Self-employment and other" = rd_est(eh, "selfemp"),
  "Public-sector employment" = rd_est(eh, "public"),
  "Salaried, excluding pensioners" = rd_est(no_pens, "salaried"),
  .id = "outcome"
)

placebo <- bind_rows(
  map_dfr(c(54, 56, 58), \(a) bind_cols(test = paste("Placebo, employment at", a), honest_est(filter(eh, age < 60), "emp", c = a - 60, h = hon_emp$h, M = M_emp))),
  map_dfr(c(62, 64, 66), \(a) bind_cols(test = paste("Placebo, employment at", a), honest_est(filter(eh, age >= 60), "emp", c = a - 60, h = hon_emp$h, M = M_emp))),
  map_dfr(c(54, 56, 58), \(a) bind_cols(test = paste("Placebo, extreme poverty at", a), honest_est(filter(eh, age < 60), "pext0", c = a - 60, h = hon_pov$h, M = M_pov))),
  map_dfr(c(62, 64, 66), \(a) bind_cols(test = paste("Placebo, extreme poverty at", a), honest_est(filter(eh, age >= 60), "pext0", c = a - 60, h = hon_pov$h, M = M_pov)))
)

balance <- bind_rows(
  "Balance: female" = rd_est(eh, "female"),
  "Balance: years of schooling" = rd_est(eh, "educ"),
  "Balance: rural residence" = rd_est(eh, "rural"),
  "Balance: indigenous identification" = rd_est(eh, "indig"),
  .id = "test"
)

density_p <- rddensity(eh$x, c = 0)$test$p_jk
bino_p <- with(count(filter(eh, age %in% 59:60), age), binom.test(n[2], sum(n))$p.value)
heap <- count(eh, age) |> mutate(ratio = n / ((lag(n) + lead(n)) / 2))

sensitivity <- bind_rows(
  map_dfr(c(3.5, 4.5, 5.5, 6.5, 7.5, 10.5), \(h) bind_cols(spec = sprintf("Local linear, h = %.1f (honest CI)", h), honest_est(eh, "emp", h = h, M = M_emp))),
  bind_cols(spec = "Local quadratic, MSE-optimal bandwidth", rd_est(eh, "emp", p = 2))
)

poverty <- bind_rows(
  "Extreme poverty" = rd_est(eh, "pext0"),
  "Income poverty" = rd_est(eh, "p0"),
  "Poverty gap" = rd_est(eh, "p1"),
  "Income relative to poverty line" = rd_est(eh, "ratio"),
  "Log per capita household income" = rd_est(eh, "lny"),
  "Extreme poverty, simulated without Renta Dignidad" = rd_est(eh, "pext0_sim"),
  "Income poverty, simulated without Renta Dignidad" = rd_est(eh, "p0_sim"),
  .id = "outcome"
)

poverty_area <- bind_rows(
  "Extreme poverty, urban" = rd_est(filter(eh, rural == 0), "pext0"),
  "Extreme poverty, rural" = rd_est(filter(eh, rural == 1), "pext0"),
  "Income poverty, urban" = rd_est(filter(eh, rural == 0), "p0"),
  "Income poverty, rural" = rd_est(filter(eh, rural == 1), "p0"),
  .id = "outcome"
)
area_diff <- diff_test(poverty_area[2, ], poverty_area[1, ])

pov_levels <- eh |>
  filter(between(age, 56, 64)) |>
  summarise(
    pext_before = weighted.mean(pext0[age < 60], w[age < 60], na.rm = TRUE),
    pext_after = weighted.mean(pext0[age >= 60], w[age >= 60], na.rm = TRUE),
    recip_poor = weighted.mean(p0[rd == 1], w[rd == 1], na.rm = TRUE),
    recip_ext = weighted.mean(pext0[rd == 1], w[rd == 1], na.rm = TRUE),
    zext = weighted.mean(zext, w, na.rm = TRUE), z = weighted.mean(z, w, na.rm = TRUE),
    hhsize = weighted.mean(hhsize[age >= 60], w[age >= 60], na.rm = TRUE)
  )

household <- bind_rows(
  "Household size" = rd_est(eh, "hhsize"),
  "Co-resident children under 15" = rd_est(eh, "kids"),
  "Any prime-age member present" = rd_est(eh, "prime"),
  "Number of employed prime-age members" = rd_est(eh, "prime_emp"),
  "Number of members aged 50-70" = rd_est(eh, "older"),
  .id = "outcome"
)

wage_context <- eh |>
  filter(between(age, 55, 64), emp == 1, ylab > 0) |>
  summarise(median = median(ylab), below = weighted.mean(ylab < rd_benefit, w))

results <- mget(c(
  "main", "h0", "first_stage", "takeup", "employment", "subgroups", "pair_tests", "pension_age",
  "pension_rd_educ7", "excl", "did_educ", "work_type", "placebo", "balance", "density_p", "bino_p", "heap",
  "sensitivity", "poverty", "poverty_area", "area_diff", "pov_levels", "household", "wage_context"
))
saveRDS(results, "output/results.rds")
iwalk(keep(results, is.data.frame), \(t, nm) write_csv(t, file.path("output/csv", paste0(nm, ".csv"))))
