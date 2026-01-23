#' Format SpiGes data to a format for SPLG-Grouper Input
#'
#' This function prepares SpiGes data so they can be used as input for the SPLG-Grouper.
#' The data are transformed according to the SPLG-Grouper specification.
#'
#' @param spiges_data A list of data frames representing the SpiGes data structure
#' @param type One of 'group', 'control', 'output', see SPLG-Grouper documentation for more information.
#' @param type One of 'TEXT', 'XML', 'JSON', for defining a input format for the SPLG-Grouper
#' @param version SpiGes version (default: "1.4")
#'
#' @return character vector, prepared for writing to directory `1-input` of SPLG-Grouper
#' @export
#'
#' @examples
#' \dontrun{
#' fmt_splg(spiges_data)
#' }
fmt_splg <- function(
  spiges_data,
  type = c('group', 'control', 'output'),
  format = c('TEXT', 'XML', 'JSON'),
  version = "1.4"
) {
  type = match.arg(type)
  format = match.arg(format)

  # Check input
  check_spiges_tables(spiges_data, c('admin', 'newborn', 'diag', 'proc'))

  spiges_admin <- spiges_data$admin
  spiges_newborn <- spiges_data$newborn |>
    dplyr::select(fall_id, gestationsalter2, geburtsgewicht)
  spiges_diag <- spiges_data$diag
  spiges_proc <- spiges_data$proc

  # format admin
  splg_admin <- fmt_splg_admin_newborn(
    spiges_admin,
    spiges_newborn,
    format = format,
    type = type,
    version = version
  )

  # format diags
  splg_icd <- fmt_splg_diag(spiges_diag, format = format, version = version)

  # format procs
  splg_chop <- fmt_splg_proc(spiges_proc, format = format, version = version)

  splg_combined <-
    splg_admin |>
    dplyr::left_join(splg_icd, by = "ID") |>
    dplyr::left_join(splg_chop, by = "ID")

  if (version == "1.4") {
    if (format == 'TEXT') {
      splg_in <-
        splg_combined |>
        dplyr::mutate(
          diagnosen = paste('ICD', dplyr::coalesce(diagnosen, ''))
        ) |>
        dplyr::mutate(
          behandlungen = paste('CHOP', dplyr::coalesce(behandlungen, ''))
        ) |>
        tidyr::pivot_longer(-ID, names_to = 'teil', values_to = 'splg_text') |>
        dplyr::pull(splg_text)

      splg_in <- c('SPLG-INPUT', splg_in)
    } else if (format == 'XML') {
      stop('Format XML for SPLG-Grouper is not implemented yet.')
    } else if (format == 'JSON') {
      splg_in <-
        splg_combined |>
        dplyr::mutate(admin = sub('\\[', '', admin)) |>
        dplyr::mutate(admin = sub('\\}\\]', '', admin)) |>
        tidyr::unite(
          json_fall,
          admin,
          diagnosen,
          behandlungen,
          sep = ',',
          na.rm = T
        ) |>
        dplyr::mutate(json_fall = paste0(json_fall, '}')) |>
        dplyr::summarise(json_faelle = paste(json_fall, collapse = ',')) |>
        dplyr::mutate(
          splg_json = paste0('{"splg-json":[', json_faelle, ']}')
        ) |>
        dplyr::pull(splg_json)
    }
  }
  return(splg_in)
}


fmt_splg_admin_newborn <- function(
  admin,
  newborn,
  format,
  type,
  version = '1.4'
) {
  group_cols = c(
    'alter',
    'alter_U1',
    'gestationsalter2',
    'geburtsgewicht',
    'beatmung',
    'austrittsdatum'
  )
  ctrl_cols = c(
    'spital_id',
    'plz',
    'standort',
    'wohnkanton',
    'abc_fall',
    'tarif'
  )
  out_cols = c(
    'eintrittsdatum',
    'eintritt_aufenthalt',
    'eintrittsart',
    'einw_instanz',
    'grund_wiedereintritt',
    'austritt_aufenthalt',
    'aufnahmegewicht',
    'grundversicherung'
  )

  if (type == 'group') {
    missing_cols <- setdiff(
      c('fall_id', group_cols),
      c(names(admin), names(newborn))
    )
  } else if (type == 'control') {
    missing_cols <- setdiff(
      c('fall_id', group_cols, ctrl_cols),
      c(names(admin), names(newborn))
    )
  } else if (type == 'output') {
    missing_cols <- setdiff(
      c('fall_id', group_cols, ctrl_cols, out_cols),
      c(names(admin), names(newborn))
    )
  }

  if (length(missing_cols) > 0) {
    stop(
      "Missing column(s): ",
      paste(missing_cols, collapse = ', '),
      ' is/are needed for type = ',
      type,
      ' but are not present.'
    )
  }

  splg_admin <-
    admin %>%
    dplyr::mutate(
      austritt = spiges2datestr(austrittsdatum),
      austritt = dplyr::if_else(austritt == '', NA_character_, austritt)
    ) |>
    dplyr::left_join(newborn, by = 'fall_id') %>%
    dplyr::select(
      ID = fall_id,
      fallid = fall_id,
      burnr = spital_id,
      agey = alter,
      aged = alter_U1,
      ssw = gestationsalter2,
      ggw = geburtsgewicht,
      dmb = beatmung,
      austritt
    )

  if (type %in% c('control', 'output')) {
    splg_admin_ctrl <-
      admin |>
      dplyr::mutate(falltyp = paste0(abc_fall, ':3:', tarif)) |>
      dplyr::select(
        ID = fall_id,
        burnr = spital_id,
        plz,
        standort,
        wohnkanton,
        falltyp
      )

    splg_admin <- dplyr::left_join(splg_admin, splg_admin_ctrl, by = 'ID')
  }

  if (type == 'output') {
    splg_admin_out <-
      admin |>
      dplyr::select(
        ID = fall_id,
        ed = eintrittsdatum,
        ave = eintritt_aufenthalt,
        ea = eintrittsart,
        ei = einw_instanz,
        ana = austritt_aufenthalt,
        gew = aufnahmegewicht,
        hktr = grundversicherung
      )

    splg_admin <- dplyr::left_join(splg_admin, splg_admin_out, by = 'ID')
  }

  if (version == "1.4") {
    if (format == 'TEXT') {
      splg_admin_in <-
        splg_admin |>
        dplyr::mutate(dplyr::across(
          -ID,
          ~ dplyr::if_else(
            is.na(.x),
            NA_character_,
            paste0(dplyr::cur_column(), "=", as.character(.x))
          )
        )) |>
        tidyr::unite('admin', -ID, sep = ';', na.rm = T)
    } else if (format == 'XML') {
      stop('Format XML for SPLG-Grouper (admin) is not implemented yet.')
    } else if (format == 'JSON') {
      splg_admin_in <-
        splg_admin |>
        dplyr::mutate(dplyr::across(-ID, as.character)) |>
        tidyr::nest(json_data = -ID) |>
        dplyr::mutate(
          admin = purrr::map_chr(json_data, jsonlite::toJSON)
        ) |>
        dplyr::select(ID, admin)
    }
  }
  return(splg_admin_in)
}


fmt_splg_diag <- function(
  diag,
  format = c('TEXT', 'XML', 'JSON'),
  version = '1.4'
) {
  missing_cols <- setdiff(
    c(
      'fall_id',
      'diagnose_id',
      'diagnose_kode',
      'diagnose_seitigkeit',
      'diagnose_zusatz'
    ),
    names(diag)
  )
  if (length(missing_cols) > 0) {
    stop(
      "Missing column(s): ",
      paste(missing_cols, collapse = ', '),
      " is/are needed and not present."
    )
  }

  splg_diag <- diag |>
    dplyr::select(
      ID = fall_id,
      rang = diagnose_id,
      code = diagnose_kode,
      seitigkeit = diagnose_seitigkeit,
      zusatz = diagnose_zusatz
    ) |>
    # not documented, but probably right
    dplyr::mutate(zusatz = dplyr::if_else(rang == 1, zusatz, NA_character_)) |>
    dplyr::mutate(across(
      where(is.character),
      ~ dplyr::if_else(. == ' ' | . == '', NA_character_, .)
    ))

  if (version == "1.4") {
    if (format == 'TEXT') {
      splg_diag_in <-
        splg_diag |>
        dplyr::mutate(
          diags = dplyr::if_else(
            is.na(seitigkeit) | seitigkeit == '',
            code,
            paste0(code, ':', seitigkeit)
          ),
          diags = dplyr::if_else(
            rang == 1 & !is.na(zusatz),
            paste0(diags, ';', zusatz),
            diags
          )
        ) |>
        dplyr::group_by(ID) |>
        dplyr::arrange(rang, .by_group = T) |>
        dplyr::summarise(
          diags = paste0(diags, collapse = ';'),
          .groups = 'drop'
        ) |>
        dplyr::select(ID, diagnosen = diags)
    } else if (format == 'XML') {
      stop('Format XML for SPLG-Grouper (diag) is not implemented yet.')
    } else if (format == 'JSON') {
      #   splg_diag_in <-
      #     splg_diag %>%
      #     group_by(ID, rang) |>
      #     summarise(
      #       diagnose = list({
      #         vals <- c(
      #           code = as.character(code),
      #           rang = as.character(rang),
      #           seitigkeit = as.character(seitigkeit),
      #           zusatz = as.character(zusatz)
      #         )
      #         # NA und reine leere/whitespace-Strings entfernen
      #         vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
      #         vals
      #       })
      #     ) |>
      #     summarise(diagnosen = list(diagnose)) |>
      #     dplyr::select(ID, diagnosen)
      # }
      splg_diag_in <- splg_diag |>
        dplyr::mutate(dplyr::across(-ID, as.character)) |>
        tidyr::nest(json_data = c(rang, code, seitigkeit, zusatz)) |>
        dplyr::mutate(
          diagnosen = purrr::map_chr(json_data, jsonlite::toJSON)
        ) |>
        dplyr::select(ID, diagnosen)
    }
  }
  return(splg_diag_in)
}


#' Internal: format procedure (CHOP) records for SPLG-Grouper input (supports TEXT and JSON outputs)
fmt_splg_proc <- function(
  proc,
  format = c('TEXT', 'XML', 'JSON'),
  version = '1.4'
) {
  missing_cols <- setdiff(
    c(
      'behandlung_id',
      'behandlung_chop',
      'behandlung_seitigkeit',
      'behandlung_beginn',
      'behandlung_auswaerts'
    ),
    names(proc)
  )
  if (length(missing_cols) > 0) {
    stop(
      "Missing column(s): ",
      paste(missing_cols, collapse = ', '),
      " is/are needed and not present."
    )
  }

  splg_proc <- proc |>
    dplyr::mutate(
      beginn = spiges2datestr(behandlung_beginn),
      beginn = dplyr::if_else(beginn == '', NA_character_, beginn)
    ) |>
    dplyr::select(
      ID = fall_id,
      rang = behandlung_id,
      code = behandlung_chop,
      seitigkeit = behandlung_seitigkeit,
      beginn,
      ambext = behandlung_auswaerts
    ) |>
    dplyr::mutate(across(
      where(is.character),
      ~ dplyr::if_else(. == ' ' | is.na(.) == '', '', .)
    )) |>
    dplyr::mutate(across(
      c(where(is.integer), -ID),
      ~ dplyr::if_else(is.na(.), '', as.character(.))
    ))

  if (version == "1.4") {
    if (format == 'TEXT') {
      splg_proc_in <- splg_proc |>
        dplyr::mutate(
          procs = dplyr::case_when(
            seitigkeit == '' & ambext == '' & beginn == '' ~ code,
            beginn != '' ~ paste0(
              code,
              ':',
              seitigkeit,
              ':',
              ambext,
              ':',
              beginn
            ),
            ambext != '' ~ paste0(code, ':', seitigkeit, ':', ambext),
            seitigkeit != '' ~ paste0(code, ':', seitigkeit),
            .default = 'ERR'
          )
        ) %>%
        dplyr::group_by(ID) |>
        dplyr::arrange(rang, .by_group = T) |>
        dplyr::summarise(
          procs = paste0(procs, collapse = ';'),
          .groups = 'drop'
        ) %>%
        dplyr::select(ID, behandlungen = procs)
    } else if (format == 'XML') {
      stop('Format XML for SPLG-Grouper (proc) is not implemented yet.')
    } else if (format == 'JSON') {
      splg_proc_in <- splg_proc |>
        dplyr::mutate(dplyr::across(-ID, as.character)) |>
        tidyr::nest(json_data = c(rang, code, seitigkeit, beginn, ambext)) |>
        dplyr::mutate(
          behandlungen = purrr::map_chr(json_data, jsonlite::toJSON)
        ) |>
        dplyr::select(ID, behandlungen)
    }
  }
}
