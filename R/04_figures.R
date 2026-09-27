theme_rd <- theme_classic(base_family = font, base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 10.5), plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, size = 7.5, lineheight = 1.1), plot.caption.position = "plot",
    legend.position = "bottom", legend.title = element_blank(), axis.text = element_text(color = "black")
  )
cutoff <- geom_vline(xintercept = 59.5, linetype = "dashed", linewidth = 0.4)
wrap <- \(x) paste(strwrap(x, 115), collapse = "\n")
pts <- \(...) list(geom_line(color = "grey45", linewidth = 0.4, ...), geom_point(shape = 21, fill = "white", size = 2, stroke = 0.6, ...))
save_fig <- \(p, name, h = 4.2) ggsave(file.path("output/figures", paste0(name, ".png")), p, width = 7, height = h, dpi = 300, bg = "white", device = agg_png)
ages <- \(lo = 52, hi = 70) filter(eh, between(age, lo, hi))

local_fit <- function(d, y, h) {
  d <- filter(d, abs(x) < h, !is.na(.data[[y]])) |> mutate(side = D, k = w * (1 - abs(x) / h))
  d |>
    group_by(side) |>
    group_modify(\(s, g) {
      m <- lm(as.formula(paste(y, "~ x")), data = s, weights = k)
      tibble(age = range(s$age), fit = predict(m, tibble(x = range(s$x))))
    })
}

nl <- bind_rows(
  "Total non-labor income" = wmean_by_age(ages(), "ynolab"),
  "Renta Dignidad" = wmean_by_age(ages(), "rd_inc"),
  "Contributory pension" = wmean_by_age(ages(), "pens_inc"),
  .id = "series"
) |> mutate(series = factor(series, unique(series)))

f1 <- ggplot(nl, aes(age, value, shape = series, group = series)) +
  geom_line(color = "grey45", linewidth = 0.4) + geom_point(fill = "white", size = 2, stroke = 0.6) + cutoff +
  scale_shape_manual(values = c(21, 22, 24)) + scale_x_continuous(breaks = seq(52, 70, 2)) +
  labs(title = "Figure 1. Mean Non-Labor Income by Age and Source, Bolivia, 2021\u20132024", x = "Age", y = "Mean income (Bs/month)",
       caption = wrap("Note: Points are weighted means by single year of age. Renta Dignidad income is the reported monthly payment scaled by 13/12 to include the annual aguinaldo. The dashed line marks the age-60 eligibility threshold.")) +
  theme_rd
save_fig(f1, "figure1_nonlabor_income", 4.5)

f2 <- ggplot(wmean_by_age(ages(60, 70), "rd"), aes(age, value)) + pts() +
  scale_x_continuous(breaks = 60:70) + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Figure 2. Reported Receipt of Renta Dignidad by Age, Bolivia, 2021\u20132024", x = "Age", y = "Share reporting receipt",
       caption = wrap("Note: Weighted share of respondents reporting receipt. The survey asks about receipt only for respondents aged 60 and older.")) +
  theme_rd
save_fig(f2, "figure2_takeup")

fit3 <- local_fit(eh, "emp", h0)
f3 <- ggplot(wmean_by_age(ages(), "emp"), aes(age, value)) +
  annotate("rect", xmin = 60 - h0, xmax = 60 + h0, ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_point(shape = 21, fill = "white", size = 2, stroke = 0.6) +
  geom_line(data = fit3, aes(age, fit, group = side), linewidth = 0.8) + cutoff +
  scale_x_continuous(breaks = seq(52, 70, 2)) +
  labs(title = "Figure 3. Share Employed by Age, Bolivia, 2021\u20132024", x = "Age", y = "Share employed in reference week",
       caption = wrap(sprintf("Note: Points are weighted means by single year of age. Solid lines are local linear fits with triangular kernel and survey weights, estimated separately on each side of the threshold within the MSE-optimal bandwidth of %.1f years (shaded).", h0))) +
  theme_rd
save_fig(f3, "figure3_employment", 4.5)

f4 <- ggplot(subgroups, aes(replacement, est)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.006, color = "grey40", linewidth = 0.4) +
  geom_point(shape = 21, fill = "white", size = 2.2, stroke = 0.7) +
  geom_text(aes(label = sub("Education, ", "Educ. ", group), hjust = if_else(group %in% c("Urban", "Female"), 1.15, -0.15)), family = font, size = 2.7, vjust = -0.5) +
  scale_x_continuous(limits = c(0.1, 0.45)) +
  labs(title = "Figure 4. Employment Estimates by Subgroup and Replacement Rate, 2021\u20132024",
       x = "Replacement rate (annualized benefit \u00f7 median monthly earnings, ages 55\u201364)", y = "RD estimate on employment",
       caption = wrap("Note: Points are local linear RD estimates with 95% robust bias-corrected confidence intervals clustered by primary sampling unit. The benefit is Bs 4,550 per year (Bs 379 per month).")) +
  theme_rd
save_fig(f4, "figure4_subgroups", 4.5)

pens <- ages() |>
  mutate(group = factor(if_else(educ7 == 1, "7 or more years of schooling", "0 to 6 years of schooling"), c("7 or more years of schooling", "0 to 6 years of schooling"))) |>
  wmean_by_age("pension", group)
f5 <- ggplot(pens, aes(age, value, shape = group, linetype = group)) +
  geom_line(color = "grey20", linewidth = 0.4) + geom_point(fill = "white", size = 2, stroke = 0.6) +
  geom_vline(xintercept = 57.5, linetype = "dotted", color = "grey40") + cutoff +
  scale_shape_manual(values = c(21, 22)) + scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(52, 70, 2)) +
  labs(title = "Figure 5. Contributory Pension Receipt by Age and Education, 2021\u20132024", x = "Age", y = "Share receiving a contributory pension",
       caption = wrap("Note: Weighted shares. The dotted line marks the Prestaci\u00f3n Solidaria de Vejez threshold at 58; the dashed line marks Renta Dignidad eligibility at 60.")) +
  theme_rd
save_fig(f5, "figure5_contributory_pension", 4.5)

work <- bind_rows(
  "Salaried employment" = wmean_by_age(ages(), "salaried"),
  "Self-employment and other" = wmean_by_age(ages(), "selfemp"),
  .id = "series"
)
f6 <- ggplot(work, aes(age, value, shape = series, group = series)) +
  geom_line(color = "grey45", linewidth = 0.4) + geom_point(fill = "white", size = 2, stroke = 0.6) + cutoff +
  scale_shape_manual(values = c(21, 22)) + scale_x_continuous(breaks = seq(52, 70, 2)) +
  labs(title = "Figure 6. Employment by Type of Work and Age, Bolivia, 2021\u20132024", x = "Age", y = "Share of population",
       caption = wrap("Note: Salaried employment includes wage employees, salaried employers and domestic workers; self-employment and other includes own-account workers, non-salaried employers, cooperative members and unpaid family workers.")) +
  theme_rd
save_fig(f6, "figure6_work_type", 4.5)

f7 <- ggplot(count(eh, age), aes(age, n)) + pts() + cutoff +
  scale_x_continuous(breaks = seq(45, 75, 5)) +
  labs(title = "Figure 7. Number of Respondents by Single Year of Age, Bolivia, 2021\u20132024", x = "Age", y = "Respondents",
       caption = wrap(sprintf("Note: Unweighted counts in the pooled sample. Density test robust p-value = %.3f; binomial test of ages 59 and 60, p = %.3f.", density_p, bino_p))) +
  theme_rd
save_fig(f7, "figure7_age_distribution")

pov <- bind_rows(
  "Observed" = wmean_by_age(ages(), "pext0"),
  "Simulated without Renta Dignidad income" = wmean_by_age(ages(), "pext0_sim"),
  .id = "series"
)
f8 <- ggplot(pov, aes(age, value, shape = series, group = series)) +
  geom_line(color = "grey45", linewidth = 0.4) + geom_point(fill = "white", size = 2, stroke = 0.6) + cutoff +
  scale_shape_manual(values = c(21, 22)) + scale_x_continuous(breaks = seq(52, 70, 2)) +
  labs(title = "Figure 8. Share Below the Extreme Poverty Line by Age, Bolivia, 2021\u20132024", x = "Age", y = "Share below the extreme poverty line",
       caption = wrap("Note: The simulated series subtracts all Renta Dignidad income received in the household from household income before comparing per capita income with the extreme poverty line.")) +
  theme_rd
save_fig(f8, "figure8_extreme_poverty", 4.5)
