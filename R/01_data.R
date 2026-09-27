read_year <- function(y) {
  d <- read_sav(file.path("data", sprintf("EH%d_Personas.sav", y)))
  if (y == 2022) d <- rename(d, s05a_01e = s05a_01e_1, s05a_01e0 = s05a_01e_2)
  d |>
    zap_labels() |>
    transmute(
      year = y, hh = paste(y, folio), psu = paste(y, upm), w = Factor_Rev2025,
      age = s01a_03, female = as.integer(s01a_02 == 2), rural = as.integer(area == 2),
      indig = as.integer(s01a_09 == 1), educ = aestudio, hhsize = totper,
      emp = as.integer(condact %in% 1), catocup = s04b_12, sector = s04b_13,
      ylab, ynolab, yhogpc, z, zext, p0, p1, pext0,
      rd = if_else(age >= 60, as.integer(s05a_01e == 1), 0L),
      rd_inc = if_else(age >= 60 & s05a_01e == 1, s05a_01e0 * 13 / 12, 0),
      pens_inc = coalesce(s05a_01a, 0), pension = as.integer(pens_inc > 0)
    )
}

persons <- map_dfr(2021:2024, read_year)

hh_vars <- persons |>
  group_by(hh) |>
  summarise(
    kids = sum(age < 15), prime_n = sum(between(age, 18, 59)),
    prime_emp = sum(between(age, 18, 59) & emp == 1), older = sum(between(age, 50, 70)),
    rd_hh = sum(rd_inc)
  )

eh <- persons |>
  filter(between(age, 45, 75)) |>
  left_join(hh_vars, by = "hh") |>
  mutate(
    x = age - 60, D = as.integer(age >= 60), yr = factor(year), educ7 = as.integer(educ >= 7),
    salaried = as.integer(emp == 1 & catocup %in% c(1, 2, 8)),
    selfemp = as.integer(emp == 1 & catocup %in% 3:7),
    public = as.integer(emp == 1 & sector %in% 1:2),
    anynl = as.integer(ynolab > 0), self_prime = between(age, 18, 59),
    prime = as.integer(prime_n - self_prime > 0), prime_emp = prime_emp - self_prime * emp, lny = if_else(yhogpc > 0, log(yhogpc), NA_real_), ratio = yhogpc / z,
    yhogpc_sim = yhogpc - rd_hh / hhsize,
    pext0_sim = as.integer(yhogpc_sim < zext), p0_sim = as.integer(yhogpc_sim < z)
  )
