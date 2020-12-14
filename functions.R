#### Functions used in bc-covid-epi

smooth_new_cases <- function(cases){
  cases %>%
    arrange(Reported_Date) %>%
    mutate(n_smooth = round(
      smoother::smth(n, window = 7, tails = TRUE)
    )) %>%
    select(Reported_Date, n, n_smooth)
}


compute_likelihood <- function(cases){
  likelihood <- cases %>%
    filter(n_smooth > 0) %>%
    mutate(
      r_t = list(r_t_range),
      lambda = map(lag(n_smooth, 1), ~ .x * exp(GAMMA * (r_t_range - 1))),
      likelihood_r_t = map2(n_smooth, lambda, dpois, log = TRUE)
    ) %>%
    slice(-1) %>%
    select(-lambda) %>%
    unnest(c(likelihood_r_t, r_t))
}



compute_posterior <- function(likelihood){
  likelihood %>%
    arrange(Reported_Date) %>%
    group_by(r_t) %>%
    mutate(posterior = exp(
      zoo::rollapplyr(likelihood_r_t, 7, sum, partial = TRUE)
    )) %>%
    group_by(Reported_Date) %>%
    mutate(posterior = posterior / sum(posterior, na.rm = TRUE),
           posterior = ifelse(is.nan(posterior), 0, posterior)) %>%
    # HACK: NaNs in the posterior create issues later on. So we remove them.
    #LEGACY remove once tested: mutate(posterior = ifelse(is.nan(posterior), 0, posterior)) %>%
    ungroup() %>%
    select(-likelihood_r_t)
}


estimate_rt <- function(posteriors){
  posteriors %>%
    group_by(Reported_Date) %>%
    summarize(
      r_t_simulated = list(sample(r_t_range, 10000, replace = TRUE, prob = posterior)),
      r_t_most_likely = r_t_range[which.max(posterior)]
    ) %>%
    mutate(
      r_t_lo = map_dbl(r_t_simulated, ~ hdi(.x)[1]),
      r_t_hi = map_dbl(r_t_simulated, ~ hdi(.x)[2])
    ) %>%
    select(-r_t_simulated)
}
