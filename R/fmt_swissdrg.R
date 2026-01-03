#' Export SpiGes data to SwissDRG BATCH_2017 format
#'
#' This function exports SpiGes data to the SwissDRG BATCH_2017 format.
#'
#' @param spiges_data A list of data frames representing the SpiGes data structure
#' @param version SpiGes version (default: "1.4")
#'
#' @return character vector, prepared for writing to directory of SwissDRG-Grouper
#' @export
#'
#' @examples
#' \dontrun{
#' fmt_swissdrg(spiges_data)
#' }
fmt_swissdrg <- function(
  spiges_data,
  format = 'BATCH_2017',
  tariff = c('SwissDRG', 'ST Reha', 'TARPSY')
) {
  # Check input
  check_spiges_tables(
    spiges_data,
    c('admin', 'neugeborene', 'diag', 'proc', 'medi')
  )

  format <- match.arg(format)
  tariff <- match.arg(tariff)

  spiges_admin <- spiges_data$admin
  spiges_neugeb <- spiges_data$neugeborene |>
    dplyr::select(fall_id, gestationsalter2, geburtsgewicht)
  spiges_diag <- spiges_data$diag
  spiges_proc <- spiges_data$proc
  spiges_medi <- spiges_data$medi

  if ('los' %in% names(spiges_data)) {
    spiges_los <- spiges_data$los
  } else {
    spiges_los <- calc_los(spiges_data)
  }

  # format admin
  swissdrg_admin <- fmt_swissdrg_admin(spiges_admin)

  # format los
  swissdrg_los <- fmt_swissdrg_los(spiges_los, tariff)

  # format babydata
  swissdrg_babydata <- fmt_swissdrg_babydata(spiges_admin, spiges_neugeb)

  # format diags
  swissdrg_diags <- fmt_swissdrg_diag(spiges_diag)

  # format procs
  swissdrg_procs <- fmt_swissdrg_proc(spiges_proc)

  # format medi
  swissdrg_medi <- fmt_swissdrg_medi(spiges_medi)

  swissdrg_combined <-
    swissdrg_admin |>
    dplyr::left_join(swissdrg_babydata, by = "ID") |>
    dplyr::mutate(baby_data = dplyr::coalesce(baby_data, '')) |>
    dplyr::left_join(swissdrg_los, by = "ID") |>
    dplyr::mutate(los = dplyr::coalesce(los, '')) |>
    dplyr::left_join(swissdrg_diags, by = "ID") |>
    dplyr::mutate(diagnoses = dplyr::coalesce(diagnoses, '')) |>
    dplyr::left_join(swissdrg_procs, by = "ID") |>
    dplyr::mutate(procedures = dplyr::coalesce(procedures, '')) |>
    dplyr::left_join(swissdrg_medi, by = "ID") |>
    dplyr::mutate(medications = dplyr::coalesce(medications, ''))

  splg_in <-
    swissdrg_combined |>
    dplyr::select(
      patient_id,
      age,
      age_days,
      baby_data,
      sex,
      adm_date,
      adm_mode,
      exit_date,
      exit_mode,
      los,
      resp_hours,
      diagnoses,
      procedures,
      medications
    ) |>
    tidyr::unite('swissdrg_in', everything(), sep = ';') |>
    dplyr::pull(swissdrg_in)

  return(splg_in)
}

fmt_swissdrg_admin <- function(admin) {
  # check varibles
  check_spiges_var(
    admin,
    c(
      'fall_id',
      'alter',
      'alter_U1',
      'geschlecht',
      'eintrittsdatum',
      'eintritt_aufenthalt',
      'eintrittsart',
      'austrittsdatum',
      'austrittsentscheid',
      'austritt_aufenthalt',
      'beatmung'
    )
  )

  swissdrg_admin <-
    admin |>
    dplyr::mutate(
      ID = fall_id,
      patient_id = as.character(fall_id),
      age = dplyr::case_when(
        !is.na(alter) & alter > 0L ~ as.character(alter),
        !is.na(alter_U1) & alter_U1 > 0 ~ '',
        .default = as.character(alter)
      ),
      age_days = dplyr::if_else(
        !is.na(alter) & alter > 0,
        '',
        as.character(alter_U1)
      ),
      sex = dplyr::case_match(geschlecht, 1 ~ 'M', 2 ~ 'W', .default = ''),
      adm_date = dplyr::case_when(
        inherits(eintrittsdatum, c("Date", "POSIXt")) ~ format(
          eintrittsdatum,
          format = "%Y%m%d"
        ),
        !is.na(eintrittsdatum) ~ substr(as.character(eintrittsdatum), 1, 8),
        .default = ''
      ),
      adm_mode = dplyr::case_when(
        eintrittsart == 3L ~ '01',
        eintritt_aufenthalt == 6L & eintrittsart != 5L ~ '11',
        eintritt_aufenthalt == 6L & eintrittsart == 5L ~ '06',
        eintritt_aufenthalt != 6L ~ '01'
      ),
      exit_date = dplyr::case_when(
        inherits(austrittsdatum, c("Date", "POSIXt")) ~ format(
          austrittsdatum,
          format = "%Y%m%d"
        ),
        !is.na(austrittsdatum) ~ substr(as.character(austrittsdatum), 1, 8),
        .default = ''
      ),
      exit_mode = dplyr::case_when(
        austrittsentscheid == 5L ~ '07',
        austrittsentscheid != 5L & austritt_aufenthalt == 6L ~ '06',
        austrittsentscheid %in% c(2:3) & austritt_aufenthalt != 6L ~ '04',
        !(austrittsentscheid %in% c(2L, 3L, 5L)) &
          austritt_aufenthalt != 6L ~ '00'
      ),
      resp_hours = as.character(beatmung)
    ) |>
    dplyr::select(
      ID,
      patient_id,
      age,
      age_days,
      sex,
      adm_date,
      adm_mode,
      exit_date,
      exit_mode,
      resp_hours
    )

  return(swissdrg_admin)
}


#' Internal: format length-of-stay records for SwissDRG-Grouper input
fmt_swissdrg_los <- function(los, tariff) {
  if (tariff == 'SwissDRG') {
    tariff_los <-
      los |>
      dplyr::mutate(ID = fall_id, los = as.character(los_drg), .keep = 'none')
  } else if (tariff == 'ST Reha') {
    tariff_los <-
      los |>
      dplyr::mutate(ID = fall_id, los = as.character(los_stre), .keep = 'none')
  } else if (tariff == 'TARPSY') {
    tariff_los <-
      los |>
      dplyr::mutate(ID = fall_id, los = as.character(los_tpsy), .keep = 'none')
  } else {
    tariff_los <-
      los |>
      dplyr::mutate(ID = fall_id, los = '')
  }

  return(tariff_los)
}

#' Internal: format babydata records for SwissDRG-Grouper input
fmt_swissdrg_babydata <- function(admin, neugeb) {
  # check variables
  has_aufnahme <- 'aufnahmegewicht' %in% names(admin)
  has_geburts <- 'geburtsgewicht' %in% names(neugeb)
  has_gest1 <- 'gestationsalter1' %in% names(neugeb)
  has_gest2 <- 'gestationsalter2' %in% names(neugeb)

  if (has_aufnahme & has_geburts) {
    ggw_data <-
      dplyr::inner_join(admin, neugeb, by = 'fall_id') |>
      dplyr::mutate(ggw = dplyr::coalesce(aufnahmegewicht, geburtsgewicht)) |>
      dplyr::select(fall_id, ggw)
  } else if (has_geburts) {
    ggw_data <-
      dplyr::inner_join(admin, neugeb, by = 'fall_id') |>
      dplyr::select(fall_id, ggw = geburtsgewicht)
  } else if (has_aufnahme) {
    ggw_data <-
      dplyr::inner_join(admin, neugeb, by = 'fall_id') |>
      dplyr::select(fall_id, ggw = aufnahmegewicht)
  } else {
    stop(
      "At least one of 'aufnahmegewicht' or 'geburtsgewicht' must be present."
    )
  }

  if (has_gest1 & has_gest2) {
    ssw_data <- neugeb |>
      dplyr::mutate(
        ssw = dplyr::coalesce(gestationsalter2, gestationsalter1)
      ) |>
      dplyr::select(fall_id, ssw)
  } else if (has_gest2) {
    ssw_data <- neugeb |> dplyr::select(fall_id, ssw = gestationsalter2)
  } else if (has_gest1) {
    ssw_data <- neugeb |> dplyr::select(fall_id, ssw = gestationsalter1)
  } else {
    stop(
      "At least one of 'gestationsalter1' or 'gestationsalter2' must be present."
    )
  }

  babydata <-
    dplyr::inner_join(ggw_data, ssw_data, by = 'fall_id') |>
    dplyr::mutate(
      ggw = dplyr::coalesce(as.character(ggw), ''),
      ssw = dplyr::coalesce(as.character(ssw), '')
    ) |>
    tidyr::unite(baby_data, ggw, ssw, sep = '|') |>
    dplyr::mutate(
      baby_data = dplyr::if_else(baby_data == '|', '', baby_data)
    ) |>
    dplyr::select(ID = fall_id, baby_data)

  return(babydata)
}


#' Internal: format diagnoses (ICD) records for SwissDRG-Grouper input
fmt_swissdrg_diag <- function(diag) {
  # check varibles
  check_spiges_var(
    diag,
    c('fall_id', 'diagnose_id', 'diagnose_kode', 'diagnose_zusatz')
  )

  swissdrg_diag <- diag |>
    dplyr::filter(
      !is.na(diagnose_id) & !is.na(diagnose_kode) & diagnose_kode != ''
    ) |>
    dplyr::select(
      ID = fall_id,
      rang = diagnose_id,
      code = diagnose_kode,
      zusatz = diagnose_zusatz
    ) |>
    # addidtional diagnosis of main diagnosis is encoded as normal diagnosis
    dplyr::mutate(
      code = dplyr::if_else(
        rang == 1 & zusatz != '' & !is.na(zusatz),
        paste0(code, '|', zusatz),
        code
      )
    ) |>
    dplyr::group_by(ID) |>
    dplyr::arrange(rang, .by_group = TRUE) |>
    dplyr::summarise(diags = paste0(code, collapse = '|'), .groups = 'drop') |>
    dplyr::mutate(diags = dplyr::if_else(is.na(diags), '', diags)) |>
    dplyr::select(ID, diagnoses = diags)

  return(swissdrg_diag)
}


#' Internal: format procedure (CHOP) records for SwissDRG-Grouper input
fmt_swissdrg_proc <- function(proc) {
  # check varibles
  check_spiges_var(
    proc,
    c(
      'fall_id',
      'behandlung_id',
      'behandlung_chop',
      'behandlung_seitigkeit',
      'behandlung_beginn'
    )
  )

  swissdrg_proc <- proc |>
    dplyr::filter(
      !is.na(behandlung_id) & !is.na(behandlung_chop) & behandlung_chop != ''
    ) |>
    dplyr::mutate(
      ID = fall_id,
      rang = behandlung_id,
      code = behandlung_chop,
      seitigkeit = dplyr::case_match(
        behandlung_seitigkeit,
        0 ~ 'B',
        1 ~ 'R',
        2 ~ 'L',
        .default = ''
      ),
      beginn = dplyr::if_else(
        is.na(behandlung_beginn),
        '',
        as.character(behandlung_beginn)
      ),
      .keep = 'none'
    ) |>
    dplyr::mutate(procs = paste0(code, ':', seitigkeit, ':', beginn)) |>
    dplyr::group_by(ID) |>
    dplyr::arrange(rang, .by_group = TRUE) |>
    dplyr::summarise(procs = paste0(procs, collapse = '|'), .groups = 'drop') |>
    dplyr::mutate(procs = dplyr::if_else(is.na(procs), '', procs)) |>
    dplyr::select(ID, procedures = procs)

  return(swissdrg_proc)
}

#' Internal: format medication records for SwissDRG-Grouper input
fmt_swissdrg_medi <- function(medi) {
  # check varibles
  check_spiges_var(
    medi,
    c(
      'fall_id',
      'medi_id',
      'medi_atc',
      'medi_zusatz',
      'medi_verabreichungsart',
      'medi_dosis',
      'medi_einheit'
    )
  )

  swissdrg_medi <- medi |>
    dplyr::mutate(
      ID = fall_id,
      medi_id,
      atc_code = tidyr::replace_na(medi_atc, ''),
      annex = tidyr::replace_na(medi_zusatz, ''),
      application = tidyr::replace_na(medi_verabreichungsart, ''),
      dose = dplyr::if_else(is.na(medi_dosis), '', as.character(medi_dosis)),
      unit = tidyr::replace_na(medi_einheit, ''),
      .keep = 'none'
    ) |>
    dplyr::mutate(
      medis = paste0(
        atc_code,
        ':',
        annex,
        ':',
        application,
        ':',
        dose,
        ':',
        unit
      )
    ) %>%
    dplyr::group_by(ID) |>
    dplyr::arrange(medi_id, .by_group = TRUE) |>
    dplyr::summarise(medis = paste0(medis, collapse = '|'), .groups = 'drop') |>
    dplyr::mutate(medis = dplyr::if_else(is.na(medis), '', medis)) %>%
    dplyr::select(ID, medications = medis)

  return(swissdrg_medi)
}
