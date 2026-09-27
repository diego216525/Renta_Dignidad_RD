num <- \(x, d = 3) if_else(is.na(x), "\u2014", gsub("-", "\u2212", map2_chr(x, rep_len(d, length(x)), \(v, k) formatC(v, format = "f", digits = k, big.mark = ","))))
ci <- \(lo, hi, d = 3) sprintf("[%s, %s]", num(lo, d), num(hi, d))
pv <- \(p) case_when(is.na(p) ~ "\u2014", p < 0.001 ~ "<0.001", TRUE ~ sprintf("%.3f", p))
p_in <- \(p) if_else(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))
n_fmt <- \(n) if_else(is.na(n), "\u2014", formatC(n, format = "d", big.mark = ","))
std <- \(t, lab, d = 3) transmute(t, !!lab := .data[[names(t)[1]]], Estimate = num(est, d), `95% CI` = ci(lo, hi, d), `p-value` = pv(p), N = n_fmt(n))

save_table <- function(df, title, name, note = NULL) {
  m <- rbind(names(df), as.matrix(df))
  gp <- gpar(fontfamily = font, fontsize = 9)
  agg_png(tempfile(), width = 10, height = 5, units = "in", res = 72)
  pushViewport(viewport(gp = gp))
  cw <- apply(m, 2, \(col) max(convertWidth(stringWidth(col), "in", valueOnly = TRUE))) + 0.35
  tw <- convertWidth(grobWidth(textGrob(title, gp = gpar(fontfamily = font, fontface = "bold", fontsize = 9.5))), "in", valueOnly = TRUE) + 0.1
  dev.off()
  cw[1] <- cw[1] + max(0, tw - sum(cw))
  W <- sum(cw) + 0.3
  note_lines <- if (is.null(note)) character() else strwrap(note, width = floor(W * 17))
  rh <- 0.27
  H <- 0.5 + rh * nrow(m) + 0.2 + length(note_lines) * 0.16 + 0.15
  agg_png(file.path("output/tables", paste0(name, ".png")), width = W, height = H, units = "in", res = 300)
  grid.newpage()
  pushViewport(viewport(gp = gp))
  y <- H - 0.28
  grid.text(title, x = unit(0.15, "in"), y = unit(y, "in"), just = "left", gp = gpar(fontfamily = font, fontface = "bold", fontsize = 9.5))
  rule <- \(y, lwd) grid.lines(unit(c(0.15, W - 0.15), "in"), unit(c(y, y), "in"), gp = gpar(lwd = lwd))
  y <- y - 0.25
  rule(y, 1.2)
  xs <- 0.15 + cumsum(c(0, cw[-length(cw)]))
  for (i in seq_len(nrow(m))) {
    yy <- y - rh * (i - 0.5) - if (i > 1) 0.06 else 0
    for (j in seq_len(ncol(m))) {
      if (j == 1) grid.text(m[i, j], unit(xs[j] + 0.02, "in"), unit(yy, "in"), just = "left", gp = gp)
      else grid.text(m[i, j], unit(xs[j] + cw[j] - 0.05, "in"), unit(yy, "in"), just = "right", gp = gp)
    }
    if (i == 1) rule(y - rh - 0.03, 0.8)
  }
  y <- y - rh * nrow(m) - 0.1
  rule(y, 1.2)
  walk(seq_along(note_lines), \(k) grid.text(note_lines[k], unit(0.15, "in"), unit(y - 0.14 - (k - 1) * 0.16, "in"), just = "left", gp = gpar(fontfamily = font, fontsize = 7.5)))
  dev.off()
}

inf_note <- "Local linear regression with triangular kernel and survey weights at the age-60 threshold. Point estimates are conventional; 95% confidence intervals and p-values are robust bias-corrected (Calonico, Cattaneo and Titiunik 2014) with standard errors clustered by primary sampling unit."

save_table(std(first_stage, "Outcome", 3) |> mutate(Estimate = num(first_stage$est, if_else(first_stage$est > 5, 1, 3)), `95% CI` = ci(first_stage$lo, first_stage$hi, if_else(first_stage$est > 5, 1, 3)), Bandwidth = num(first_stage$h, 2), .before = N),
  "Table 1. First-Stage Effects of Renta Dignidad Eligibility", "table1_first_stage",
  paste(inf_note, "Each outcome uses its own MSE-optimal bandwidth."))

save_table(std(employment, "Specification"), "Table 2. Regression Discontinuity Estimates of the Effect of Eligibility on Employment", "table2_employment",
  paste(inf_note, "The honest interval (Armstrong and Koles\u00e1r 2020) bounds the second derivative of the regression function and is valid with a discrete running variable. Local randomization compares weighted means at ages 59 and 60. The fuzzy estimate uses reported receipt as the treatment. N counts observations within the bandwidth."))

save_table(transmute(subgroups, Subgroup = group, `Median earnings` = formatC(earnings, format = "f", digits = 0, big.mark = ","), `Replacement rate` = num(replacement), `RD estimate` = num(est), `95% CI` = ci(lo, hi), `p-value` = pv(p)),
  "Table 3. Employment Estimates and Replacement Rates by Subgroup", "table3_subgroups",
  paste0("Median monthly labor earnings among employed respondents aged 55 to 64. Replacement rate is the annualized benefit of Bs 379 divided by median earnings. p-values for the difference within each pair: education ", pv(pair_tests$p_diff[1]), "; area ", pv(pair_tests$p_diff[2]), "; sex ", pv(pair_tests$p_diff[3]), ". ", inf_note))

save_table(pension_age |> mutate(`Years of schooling` = if_else(educ7 == 1, "7 or more", "0 to 6"), share = num(share)) |> select(-educ7) |>
  pivot_wider(names_from = age, values_from = share) |> arrange(desc(`Years of schooling`)),
  "Table 4. Share Receiving a Contributory Pension by Age and Education", "table4_pension_by_age", "Weighted shares reporting a contributory old-age pension (jubilaci\u00f3n), excluding Renta Dignidad.")

save_table(transmute(excl, Sample = sample, Unrestricted = sprintf("%s (%s)", num(full), p_in(p_full)), `Excluding pension recipients` = sprintf("%s (%s)", num(restricted), p_in(p_restricted)), Change = paste0(if_else(change > 0, "+", ""), num(100 * change, 0), "%")),
  "Table 5. Employment Estimates Excluding Contributory Pension Recipients", "table5_excluding_pensioners",
  paste0("Because pension receipt is itself related to labor-market exit, restricted estimates condition on an outcome and should be read as bounds. Difference between education groups in the unrestricted estimate: ", num(did_educ$diff), " (p = ", pv(did_educ$p_diff), "). ", inf_note))

save_table(std(work_type, "Outcome"), "Table 6. Discontinuities in Employment by Type of Work", "table6_work_type",
  paste("Outcomes are shares of the full population aged 45 to 75. Salaried employment includes wage employees, salaried employers and domestic workers; public-sector employment includes public administration and public enterprises.", inf_note))

validity <- bind_rows(
  transmute(placebo, Test = test, Estimate = num(est), `95% CI` = ci(lo, hi), `p-value` = pv(p)),
  transmute(balance, Test = test, Estimate = num(est), `95% CI` = ci(lo, hi), `p-value` = pv(p)),
  tibble(Test = c("Density test at 60 (rddensity)", "Binomial test, counts at ages 59 and 60"), Estimate = "\u2014", `95% CI` = "\u2014", `p-value` = pv(c(density_p, bino_p)))
)
save_table(validity, "Table 7. Validity Checks", "table7_validity",
  "Placebo estimates use only observations on the same side of the true cutoff (ages below 60 for placebos at 54, 56 and 58; ages 60 and above for 62, 64 and 66) with honest confidence intervals, using the smoothness bound and bandwidth from the main honest estimate for each outcome. Balance tests use the main specification.")

save_table(transmute(sensitivity, Specification = spec, Estimate = num(est), `95% CI` = ci(lo, hi), N = n_fmt(n)),
  "Table 8. Sensitivity of the Employment Estimate to Bandwidth and Polynomial Order", "table8_sensitivity",
  "Local linear rows report honest confidence intervals with a common smoothness bound. The local quadratic row reports a robust bias-corrected interval; with four or five distinct ages on each side, it is shown to illustrate fragility rather than as an alternative estimate.")

save_table(std(poverty, "Outcome"), "Table 9. Regression Discontinuity Estimates of the Effect of Eligibility on Poverty and Income", "table9_poverty",
  paste("Poverty indicators are INE's constructed measures based on household per capita income, which includes Renta Dignidad. Simulated outcomes subtract all Renta Dignidad income in the household before comparing per capita income with the poverty lines.", inf_note))

save_table(std(poverty_area, "Outcome"), "Table 10. Poverty Estimates by Area of Residence", "table10_poverty_area",
  paste0("Difference between rural and urban extreme-poverty estimates: ", num(area_diff$diff), " (p = ", pv(area_diff$p_diff), "). ", inf_note))

save_table(std(household, "Outcome"), "Table 11. Effect of Eligibility on Household Composition", "table11_household",
  paste("Estimated on all respondents aged 45 to 75, with the respondent excluded from counts of prime-age members.", inf_note))
