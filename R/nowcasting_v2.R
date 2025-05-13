#' Run Nowcasting v2
#'
#' Run nowcasting function for notification system based data with informed parameters.
#'
#' @param data The notification system registers data frame. Should be composed in a way that each line represents a notification, with onset and report dates.
#' @param onset_var The variable name in `data` that corresponds to symptom onset/occurrence date.
#' @param report_var The variable name in `data` that corresponds to the date when information is registered in the system.
#' @param time_struc The temporal random effect structure for `inla()`. Should be 'rw1', 'rw2', 'ar1', 'ar2'.
#' @param family The model likelihood passed to `inla()`
#' @param allow_time_varying boolean. Whether to use time-varying delay effects (varying by epidemiological week).
#' @param delay_struc Model structure for the delay effect, traditionally "ar".
#' @param delay_ar_order In case `delay_struc = "ar"`, which order?
#' @param days_forecast Number of days in the future to forecast. 0 for only nowcasting.
#' @param max_delay Maximum days of delay to account to. Should be based on the distribution of the delays. Raising this parameter increases expressively the computational time and memory used by the function.
#'
#' @return A list of tibbles containing:
#' \describe{
#'  \item{daily_estimates}{A tibble containing the number of notifications observed and estimated (a.k.a., yet to enter) for each onset date.}
#'  \item{delay_matrix_df}{A tibble specifying the values of the possibly time-varying delay effects.}
#'  \item{week_day_df}{A tibble with the day-of-week effects estimated.}
#'  \item{delay_df}{More detailed tibble of estimated and observed number of notifications by day, with credible 95% intervals.}
#' }
#'
#' @examples
#' # Using built-in data from Dengue notifications in Rio
#' ncast_results <- dengue_rio |> nowcasting_v2(onset_var = "DT_SIN_PRI", report_var = "DT_DIGITA")
#'
#'if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("magrittr", quietly = TRUE)) {
#' ncast_results$daily_estimates |>
#'  ggplot2::ggplot(ggplot2::aes(x=onset, y=n)) +
#'  ggplot2::geom_bar(ggplot2::aes(fill = observed), stat = "identity", position = "stack")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom INLA f
#' @importFrom INLA inla
#' @export
nowcasting_v2 <- function(
    data,
    onset_var,
    report_var,
    time_struc = "rw1",
    family = "zeroinflatednbinomial0",
    allow_time_varying = T,
    delay_struc = "ar",
    delay_ar_order = 2,
    days_forecast = 0,
    max_delay = 15) {


  max_dt_obs <- max(data[[report_var]], na.rm=T)
  onset_sym <- dplyr::sym(onset_var)
  report_sym <- dplyr::sym(report_var)

  delay_df <- data %>%
    dplyr::filter(!is.na(!!report_sym)) %>%
    dplyr::mutate(delay = difftime(!!report_sym, !!onset_sym, units = c("days")) %>%
             as.numeric()) %>%
    dplyr::count(!!onset_sym, delay)

  delay_df_over <- delay_df %>%
    dplyr::filter(delay > max_delay | is.na(delay))

  delay_df <- delay_df %>%
    dplyr::rename(onset = dplyr::all_of(onset_var)) %>%
    dplyr::right_join(
      tidyr::expand_grid(
        onset = seq(
          min(data[[onset_var]], na.rm=T),
          max(data[[onset_var]], na.rm=T) + days_forecast,
          by=1
        ),
        delay = 0:max_delay
      ),
      by = c("onset", "delay")
    ) %>%
    dplyr::mutate(n = dplyr::case_when(
      is.na(n) & onset + delay <= max_dt_obs ~ 0,
      TRUE ~ n
    ),
    onset_num = as.numeric(onset),
    week_day = factor(lubridate::wday(onset, label=T), ordered=F),
    week_day_report = factor(lubridate::wday(onset + delay, label=T), ordered=F),
    epi_week = lubridate::epiweek(onset),
    epi_year = lubridate::epiyear(onset)
    ) %>%
    dplyr::arrange(onset, delay)

  # Adding week and delay id columns (for interaction)

  delay_df <- delay_df %>%
    dplyr::left_join(
      delay_df %>%
        dplyr::select(epi_week, epi_year) %>%
        dplyr::distinct() %>%
        dplyr::arrange(epi_year, epi_week) %>%
        dplyr::mutate(week_id = seq_len(nrow(.))),
      by = c("epi_week", "epi_year")
    ) %>%
    dplyr::mutate(delay_id = delay + 1)

  # Model formula

  if (allow_time_varying) {
    if (grepl("ar", time_struc)) {

      ar_order <- gsub("[^0-9]", "", time_struc) %>% as.integer()

      model_formula <- n ~ 1 +
        f(onset_num, model = "ar", order = ar_order) +
        f(week_day_report, model = "iid") +
        f(delay_id, model = "rw1", group = week_id,
          control.group = list(model = delay_struc, order = delay_ar_order))
    } else {
      model_formula <- n ~ 1 +
        f(onset_num, model = time_struc) +
        f(week_day_report, model = "iid") +
        f(delay_id, model = "rw1", group = week_id,
          control.group = list(model = delay_struc, order = delay_ar_order))
    }
  } else {
    if (grepl("ar", time_struc)) {

      ar_order <- gsub("[^0-9]", "", time_struc) %>% as.integer()

      model_formula <- n ~ 1 +
        f(onset_num, model = "ar", order = ar_order) +
        f(week_day_report, model = "iid") +
        f(delay_id, model = "rw1")
    } else {
      model_formula <- n ~ 1 +
        f(onset_num, model = time_struc) +
        f(week_day_report, model = "iid") +
        f(delay_id, model = "rw1")
    }
  }

  # Model fitting

  ncast_fit <-
    INLA::inla(
    model_formula,
    family = family,
    data = delay_df,
    control.predictor = list(link = 1, compute = T),
    control.compute = list(config =T, waic=F, dic=F)
  )

  # Results

  delay_matrix_df <- ncast_fit$summary.random$delay_id %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      week_id = rep(seq_len(length(unique(delay_df$week_id))), each = max_delay + 1)
    ) %>%
    dplyr::left_join(
      delay_df %>%
        dplyr::select(week_id, epi_week, epi_year) %>%
        dplyr::distinct(),
      by="week_id"
    ) %>%
    dplyr::mutate(week_date = aweek::get_date(epi_week, epi_year, start = "Sunday"), delay=ID-1) %>%
    dplyr::rename(q_025 = `0.025quant`,
           q_50 = `0.5quant`,
           q_975 = `0.975quant`) %>%
    dplyr::select(week_id, epi_week, epi_year, delay, mean:kld)


  daily_df <- ncast_fit$summary.random$onset_num %>%
    tibble::as_tibble() %>%
    dplyr::rename(q_025 = `0.025quant`,
           q_50 = `0.5quant`,
           q_975 = `0.975quant`) %>%
    dplyr::mutate(onset_date = as.Date(ID, origin="1970-01-01")) %>%
    dplyr::relocate(onset_date, .after = ID)

  week_day_df <- ncast_fit$summary.random$week_day_report %>%
    tibble::as_tibble() %>%
    dplyr::rename(q_025 = `0.025quant`,
           q_50 = `0.5quant`,
           q_975 = `0.975quant`) %>%
    dplyr::mutate(ID = factor(ID, levels = .$ID)) %>%
    dplyr::rename(week_day = ID)

  delay_df <- delay_df %>%
    dplyr::mutate(pred = ncast_fit$summary.fitted.values$`0.5quant`,
           low = ncast_fit$summary.fitted.values$`0.025quant`,
           upp = ncast_fit$summary.fitted.values$`0.975quant`) %>%
    dplyr::rename(n_obs = n) %>%
    dplyr::mutate(n = dplyr::case_when(
      is.na(n_obs) ~ pred,
      TRUE ~ n_obs
    )) %>%
    dplyr::add_row(
      delay_df_over %>%
        dplyr::rename(onset = dplyr::all_of(onset_var)) %>%
        dplyr::mutate(
          week_day = factor(lubridate::wday(onset, label=T), ordered=F),
          week_day_report = factor(lubridate::wday(onset + delay, label=T), ordered=F),
          epi_week = lubridate::epiweek(onset),
          epi_year = lubridate::epiyear(onset)
        ) %>%
        dplyr::left_join(
          delay_df %>%
            dplyr::select(epi_week, epi_year) %>%
            dplyr::distinct() %>%
            dplyr::arrange(epi_year, epi_week) %>%
            dplyr::mutate(week_id = seq_len(nrow(.))),
          by = c("epi_week", "epi_year")
        ) %>%
        dplyr::mutate(
          delay_id = delay + 1,
          n_obs = n,
          pred = NA_real_,
          low = NA_real_,
          upp = NA_real_
        )
    ) %>%
    dplyr::select(-onset_num) %>%
    dplyr::arrange(onset, delay)

  daily_estimates <- delay_df %>%
    dplyr::mutate(observed = dplyr::case_when(
      is.na(n_obs) ~ "estimate",
      !is.na(n_obs) ~ "observed"
    )) %>%
    dplyr::select(onset, observed, n) %>%
    dplyr::summarise(n=sum(n), .by=c(onset, observed)) %>%
    dplyr::mutate(
      epi_week = lubridate::epiweek(onset),
      epi_year = lubridate::epiyear(onset)
    ) %>%
    dplyr::arrange(onset)

  results <- list(
    daily_estimates = daily_estimates,
    delay_matrix_df = delay_matrix_df,
    week_day_df = week_day_df,
    delay_df = delay_df
  )

  results
}
