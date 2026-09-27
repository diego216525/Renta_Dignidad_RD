rd_est <- function(d, y, c = 0, h = NULL, p = 1, fuzzy = NULL, covs = NULL) {
  d <- d[is.finite(d[[y]]) & complete.cases(d[covs]), ]
  r <- suppressWarnings(rdrobust(
    d[[y]], d$x, c = c, h = h, p = p, weights = d$w, cluster = d$psu, masspoints = "adjust",
    fuzzy = if (!is.null(fuzzy)) d[[fuzzy]],
    covs = if (!is.null(covs)) model.matrix(reformulate(covs), d)[, -1]
  ))
  tibble(est = r$coef[1], se = r$se[3], lo = r$ci[3, 1], hi = r$ci[3, 2], p = r$pv[3], h = r$bws[1, 1], n = sum(r$N_h))
}

honest_est <- function(d, y, c = 0, h = NULL, M = NULL) {
  d <- d[is.finite(d[[y]]), ]
  args <- list(as.formula(paste(y, "~ x")), data = d, weights = d$w, cutoff = c, clusterid = d$psu, se.method = "EHW")
  r <- suppressMessages(do.call(RDHonest, c(args, if (!is.null(h)) list(h = h), if (!is.null(M)) list(M = M))))$coefficients
  tibble(est = r$estimate, se = r$std.error, lo = r$conf.low, hi = r$conf.high, p = r$p.value, h = r$bandwidth, n = sum(abs(d$x - c) < r$bandwidth), M = r$M)
}

lr_est <- function(d, y, left = 59, right = 60) {
  d <- filter(d, between(age, left, right), !is.na(.data[[y]]))
  m <- lm(as.formula(paste(y, "~ D")), data = d, weights = w)
  ct <- coeftest(m, vcovCL(m, cluster = ~psu))["D", ]
  tibble(est = ct[1], se = ct[2], lo = ct[1] - 1.96 * ct[2], hi = ct[1] + 1.96 * ct[2], p = ct[4], h = NA_real_, n = nrow(d))
}

wmean_by_age <- function(d, y, ...) {
  d |>
    filter(!is.na(.data[[y]])) |>
    group_by(age, ...) |>
    summarise(value = weighted.mean(.data[[y]], w), n = n(), .groups = "drop")
}

diff_test <- function(a, b) {
  dz <- (a$est - b$est) / sqrt(a$se^2 + b$se^2)
  tibble(diff = a$est - b$est, p_diff = 2 * pnorm(-abs(dz)))
}
