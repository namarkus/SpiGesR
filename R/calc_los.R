# R/calc_los.R
#' Berechne Varianten der Aufenthaltsdauer aus spiges_data
#'
#' Erwartet eine Liste `spiges_data` mit mindestens `admin` und einer
#' Patientenbewegungs-Tabelle. Liefert ein tibble mit Varianten.
#'
#' @param spiges_data Liste mit data.frames (z.B. spiges_data$admin, spiges_data$patientenbewegung)
#' @param admin_name Name des Admin-Data.frames (default: "admin")
#' @param movement_name Name der Bewegungs-Tabelle (default: "patientenbewegung")
#' @param id_col optionaler Patienten-ID-Name (wenn NULL wird versucht zu erkennen)
#' @param max_gap_hours max. Stunden-Gap um Bewegungen derselben Episode zuzuordnen (default: 24)
#' @return tibble mit Spalten: patient_id, variant, start, end, length_days, source
calc_los <- function(spiges_data) {
  check_spiges_tables(spiges_data, tablenames = c("admin", "patientenbewegung"))

  admin_varnames <- c(
    'fall_id',
    'jahr',
    'eintrittsdatum',
    'austrittsdatum',
    'admin_urlaub',
    'austrittsentscheid',
    'austritt_aufenthalt'
  )
  check_spiges_var(spiges_data$admin, var_names = admin_varnames)

  patbew_varnames <- c(
    'fall_id',
    'episode_id',
    'episode_beginn',
    'episode_ende',
    'episode_art',
    'wiedereintritt_aufenthalt'
  )
  check_spiges_var(spiges_data$patientenbewegung, var_names = patbew_varnames)

  spiges_admin <- spiges_data$admin |> dplyr::select(all_of(admin_varnames))
  spiges_patbew <- spiges_data$patientenbewegung |>
    dplyr::select(all_of(patbew_varnames))

  # prepare cases
  cases <-
    spiges_admin |>
    dplyr::mutate(
      ADD = spiges2date(eintrittsdatum),
      SED = spiges2date(austrittsdatum),
      EOY = spiges2date(paste0(as.character(jahr), '1231')),
      ADL = as.integer(floor(admin_urlaub / 24)),
      TRNSF_A = austrittsentscheid != 5L & austritt_aufenthalt == 6L,
      TRNSF_R = austrittsentscheid != 5L &
        austritt_aufenthalt %in% c(4, 44, 5, 6, 66),
      TRNSF_P = austrittsentscheid != 5L &
        austritt_aufenthalt %in% c(4, 5, 55, 6, 66)
    ) |>
    dplyr::select(fall_id, ADD, SED, EOY, ADL, TRNSF_A, TRNSF_R, TRNSF_P)

  # Episodes represent the time between two sub-cases. From these, periods are created that represent the time between discharge and readmission.  periods <-
  first_periods <-
    spiges_patbew |>
    dplyr::filter(episode_art == 2) |>
    dplyr::group_by(fall_id) |>
    dplyr::mutate(period_id = dplyr::row_number(episode_id) - 1L) |> # make sure, first episode starts at 0
    dplyr::mutate(
      ep_start = spiges2date(episode_beginn),
      ep_end = spiges2date(episode_ende),
      period_start = dplyr::lag(ep_end, default = NULL, order_by = period_id),
      period_end = ep_start,
      period_transf_R = wiedereintritt_aufenthalt %in% c(4, 44, 5, 6, 66),
      period_transf_P = wiedereintritt_aufenthalt %in% c(4, 5, 55, 6, 66),
      last_period = FALSE
    ) |>
    dplyr::select(
      fall_id,
      period_id,
      period_start,
      period_end,
      last_period,
      period_transf_R,
      period_transf_P,
      ep_start,
      ep_end
    )

  last_period <-
    first_periods |>
    dplyr::filter(period_id == max(period_id)) |>
    dplyr::mutate(
      period_id = period_id + 1L,
      period_start = ep_end,
      period_end = as.Date(NA_character_),
      last_period = TRUE
    ) |>
    dplyr::select(fall_id, period_id, period_start, period_end, last_period)

  periods <-
    dplyr::bind_rows(first_periods, last_period) |>
    dplyr::mutate(
      prev_period_end = dplyr::lag(period_end, order_by = period_id),
      sameday = dplyr::if_else(
        period_start == prev_period_end,
        TRUE,
        FALSE,
        FALSE
      )
    ) |>
    dplyr::arrange(period_id, .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select(
      fall_id,
      period_id,
      period_start,
      period_end,
      last_period,
      sameday,
      period_transf_R,
      period_transf_P
    )

  # # special cases
  # na_cases <-
  #   cases |>
  #   dplyr::filter(is.na(ADD)) |>
  #   dplyr::mutate(episodes = 0L, loc = NA_integer_, los_drg = NA_integer_, los_stre = NA_integer_, los_tpsy = NA_integer_) |>
  #   dplyr::select(fall_id, episodes, loc, los_drg, los_stre, los_tpsy)

  # c_cases <-
  #   cases |>
  #   dplyr::filter(is.na()) |>
  #   dplyr::mutate(episodes = 0L, loc = NA_integer_, los_drg = NA_integer_, los_stre = NA_integer_, los_tpsy = NA_integer_) |>
  #   dplyr::select(fall_id, episodes, loc, los_drg, los_stre, los_tpsy)

  single_cases <-
    cases |>
    dplyr::anti_join(periods, by = 'fall_id') |>
    dplyr::mutate(
      episodes = 1L,
      loc = as.integer(dplyr::coalesce(SED, EOY) - ADD),
      los_drg = dplyr::case_when(
        ADD != SED ~ loc - ADL,
        ADD == SED & ADL == 0 ~ 1L,
        ADD == SED & ADL != 0 ~ -1L
      ),
      los_stre = dplyr::case_when(
        ADD != SED & !TRNSF_R ~ loc - ADL + 1L,
        ADD != SED & TRNSF_R ~ loc - ADL,
        ADD == SED & ADL == 0 ~ 1L,
        ADD == SED & ADL != 0 ~ -1L
      ),
      los_tpsy = dplyr::case_when(
        ADD != SED & !TRNSF_P ~ loc - ADL + 1L,
        ADD != SED & TRNSF_P ~ loc - ADL,
        ADD == SED & ADL == 0 ~ 1L,
        ADD == SED & ADL != 0 ~ -1L
      )
    ) |>
    dplyr::select(fall_id, episodes, loc, los_drg, los_stre, los_tpsy)

  multi_cases <-
    cases |>
    dplyr::inner_join(periods, by = 'fall_id') |>
    dplyr::mutate(
      loc = as.integer(dplyr::coalesce(SED, EOY) - ADD),
      # Period-Start: case-start at period , period_start else
      period_start = dplyr::if_else(period_id == 0, ADD, period_start),
      # Period-End: case-end at last period, period_end else
      period_end = dplyr::if_else(last_period, SED, period_end),
      period_transf_R = dplyr::if_else(last_period, TRNSF_R, period_transf_R),
      period_transf_P = dplyr::if_else(last_period, TRNSF_P, period_transf_P)
    ) |>
    dplyr::mutate(
      lop = as.integer(period_end - period_start),
      lop_drg = dplyr::case_when(
        period_start != period_end ~ lop,
        period_start == period_end ~ 1L
      ),
      lop_stre = dplyr::case_when(
        period_start != period_end & !sameday & !period_transf_R ~ lop + 1L,
        period_start != period_end & (sameday | period_transf_R) ~ lop,
        period_start == period_end ~ 1L
      ),
      lop_tpsy = dplyr::case_when(
        period_start != period_end & !sameday & !period_transf_P ~ lop + 1L,
        period_start != period_end & (sameday | period_transf_P) ~ lop,
        period_start == period_end ~ 1L
      )
    ) |>
    dplyr::summarise(
      dplyr::across(c(lop, lop_drg, lop_stre, lop_tpsy), sum),
      episodes = dplyr::n(),
      .by = c(fall_id, loc, ADL)
    ) |>
    dplyr::mutate(
      los_drg = lop_drg - ADL,
      los_stre = lop_stre - ADL,
      los_tpsy = lop_tpsy - ADL
    ) |>
    dplyr::select(fall_id, episodes, loc, los_drg, los_stre, los_tpsy)

  return(dplyr::bind_rows(single_cases, multi_cases))
}
