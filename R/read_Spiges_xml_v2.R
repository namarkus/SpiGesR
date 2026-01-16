#' Read SPiGes XML and return a spiges_data object (list of tibbles)
#'
#' Parses a SPiGes XML file into a fixed set of tables. Attributes are read as
#' character first, then optionally mapped to canonical names and casted to the
#' target types. Casting issues are collected and available via problems().
#'
#' @param xml_path Path to the SPiGes XML file.
#' @param xsd_path Optional path to the XSD. Used only when validate is TRUE.
#' @param validate Logical. If TRUE, validate XML against XSD (strict).
#' @param type_spec A data.frame with columns: canonical, type, clear_name, anon_name.
#'   Usually an internal package dataset (e.g. spiges_xml_types). If NULL, no casting is applied.
#' @param Datenjahr Optional. If you store Datenjahr in meta for CSV, provide it here as well.
#' @param Datenversion Optional. If you store Datenversion in meta for CSV, provide it here as well.
#'
#' @return An object of class `spiges_data`: a named list of tibbles.
#' @examples
#' \dontrun{
#' xml_path <- system.file("extdata", "do-d-14.04-SPIGES-2024-01.xml", package = "SpiGesR")
#' xsd_path <- system.file("extdata", "do-d-14.04-SPIGES-2024-02.xsd", package = "SpiGesR")
#' data <- import_spiges_xml(data_file, id_file)
#' }
#' @export
read_spiges_xml <- function(
  xml_path,
  xsd_path = NULL,
  validate = FALSE,
  type_spec = NULL,
  Datenjahr = NULL,
  Datenversion = NULL
) {
  stopifnot(file.exists(xml_path))

  suppressPackageStartupMessages({
    library(xml2)
    library(dplyr)
    library(purrr)
    library(tibble)
    library(tidyr)
    library(readr)
  })

  # --- optional: strict XSD validation ---
  if (isTRUE(validate)) {
    if (is.null(xsd_path)) {
      stop("validate = TRUE requires xsd_path.")
    }
    stopifnot(file.exists(xsd_path))
    schema <- xml2::read_xml(xsd_path)
    ok <- xml2::xml_schema_validate(xml2::read_xml(xml_path), schema)
    if (!isTRUE(ok)) stop("XML does not validate against the provided XSD.")
  }

  doc <- xml2::read_xml(xml_path)

  # Root attributes (keep as character)
  ent_id <- xml2::xml_attr(doc, "ent_id")
  version <- xml2::xml_attr(doc, "version")

  standorte <- xml2::xml_find_all(
    doc,
    "./*[local-name()='Unternehmen']/*[local-name()='Standort']"
  )

  collected <- collect_falls(standorte, ent_id)

  out <- list(
    Administratives = purrr::map_dfr(collected, "Administratives") |>
      distinct(),
    Neugeborene = purrr::map_dfr(collected, "Neugeborene") |> distinct(),
    KostentraegerFall = purrr::map_dfr(collected, "KostentraegerFall") |>
      distinct(),
    Diagnose = purrr::map_dfr(collected, "Diagnose") |> distinct(),
    Behandlung = purrr::map_dfr(collected, "Behandlung") |> distinct(),
    Rechnung = purrr::map_dfr(collected, "Rechnung") |> distinct(),
    Medikamente = purrr::map_dfr(collected, "Medikamente") |> distinct(),
    Psychiatrie = purrr::map_dfr(collected, "Psychiatrie") |> distinct(),
    Patientenbewegungen = purrr::map_dfr(collected, "Patientenbewegungen") |>
      distinct()
  )

  # --- optional: rename-to-canonical + cast + collect problems ---
  problems_tbl <- tibble(
    table = character(),
    column = character(),
    row = integer(),
    expected = character(),
    actual = character()
  )

  add_readr_problems <- function(tbl_name, col_name, parsed_vec) {
    p <- readr::problems(parsed_vec)
    if (nrow(p) == 0) {
      return(invisible(NULL))
    }
    problems_tbl <<- bind_rows(
      problems_tbl,
      p %>%
        transmute(
          table = tbl_name,
          column = col_name,
          row = .data$row,
          expected = .data$expected,
          actual = .data$actual
        )
    )
    invisible(NULL)
  }

  parse_spiges_logical <- function(x, tbl_name, col_name) {
    x0 <- x
    x1 <- dplyr::case_when(
      is.na(x0) | x0 == "" ~ NA_character_,
      x0 %in% c("1", "TRUE", "true", "T", "J", "JA", "ja") ~ "TRUE",
      x0 %in% c("0", "FALSE", "false", "F", "N", "NEIN", "nein") ~ "FALSE",
      TRUE ~ NA_character_
    )
    bad <- which(!(is.na(x0) | x0 == "") & is.na(x1))
    if (length(bad) > 0) {
      problems_tbl <<- bind_rows(
        problems_tbl,
        tibble(
          table = tbl_name,
          column = col_name,
          row = bad,
          expected = "logical",
          actual = x0[bad]
        )
      )
    }
    as.logical(x1)
  }

  if (!is.null(type_spec)) {
    stopifnot(all(
      c("canonical", "type", "clear_name", "anon_name") %in% names(type_spec)
    ))

    name_map <- type_spec %>%
      select(canonical, clear_name, anon_name) %>%
      pivot_longer(
        c(clear_name, anon_name),
        names_to = "which",
        values_to = "src"
      ) %>%
      filter(!is.na(src), src != "") %>%
      distinct(src, canonical)

    types <- type_spec %>%
      select(canonical, type) %>%
      distinct()

    out <- purrr::imap(out, function(tbl, tbl_name) {
      if (nrow(tbl) == 0) {
        return(tbl)
      }

      # Rename to canonical names when possible
      nm <- names(tbl)
      hit <- match(nm, name_map$src)
      nm2 <- nm
      nm2[!is.na(hit)] <- name_map$canonical[hit[!is.na(hit)]]
      names(tbl) <- make.unique(nm2)

      # Cast only columns that exist in this table
      cols_to_cast <- intersect(types$canonical, names(tbl))

      for (cc in cols_to_cast) {
        target <- types$type[match(cc, types$canonical)]

        if (identical(target, "character")) {
          tbl[[cc]] <- as.character(tbl[[cc]])
        } else if (identical(target, "integer")) {
          parsed <- readr::parse_vector(tbl[[cc]], readr::col_integer())
          add_readr_problems(tbl_name, cc, parsed)
          tbl[[cc]] <- parsed
        } else if (identical(target, "double")) {
          parsed <- readr::parse_vector(tbl[[cc]], readr::col_double())
          add_readr_problems(tbl_name, cc, parsed)
          tbl[[cc]] <- parsed
        } else if (identical(target, "date")) {
          parsed <- readr::parse_vector(
            tbl[[cc]],
            readr::col_date(format = "%Y-%m-%d")
          )
          add_readr_problems(tbl_name, cc, parsed)
          tbl[[cc]] <- parsed
        } else if (identical(target, "datetime")) {
          parsed <- readr::parse_vector(tbl[[cc]], readr::col_datetime())
          add_readr_problems(tbl_name, cc, parsed)
          tbl[[cc]] <- parsed
        } else if (identical(target, "logical")) {
          tbl[[cc]] <- parse_spiges_logical(tbl[[cc]], tbl_name, cc)
        } else {
          # Unknown type, keep as character and record a note
          problems_tbl <<- bind_rows(
            problems_tbl,
            tibble(
              table = tbl_name,
              column = cc,
              row = NA_integer_,
              expected = paste0("known_type (got: ", target, ")"),
              actual = NA_character_
            )
          )
          tbl[[cc]] <- as.character(tbl[[cc]])
        }
      }

      tbl
    })
  }

  # --- build spiges_data ---
  spiges_data <- structure(out, class = "spiges_data")
  attr(spiges_data, "problems") <- problems_tbl

  # --- apply the same meta attributes as read_spiges_csv() ---
  #
  # Prefer your new helper if available.
  # This keeps XML and CSV perfectly aligned regarding meta structure.
  if (exists("spiges_set_meta", mode = "function", inherits = TRUE)) {
    spiges_data <- spiges_set_meta(
      spiges_data,
      Datenjahr = Datenjahr,
      Datenversion = Datenversion,
      Source = "SpiGes",
      Sourceformat = "XML"
    )
  } else {
    # Fallback: only if helper isn't available in the current environment
    attr(spiges_data, "Datenjahr") <- Datenjahr
    attr(spiges_data, "Datenversion") <- Datenversion
    attr(spiges_data, "Source") <- "SpiGes"
    attr(spiges_data, "Sourceformat") <- "XML"
  }

  # Optional: include XML-level info in meta if you do so for CSV as well
  # (If you don't want extra meta keys, remove these.)
  attr(spiges_data, "ent_id") <- ent_id
  attr(spiges_data, "xml_version") <- version

  spiges_data
}


# Helper: collect all falls from standorte using nested for-loops + pre-allocation
collect_falls <- function(standorte, ent_id) {
  # compute total number of 'Fall' nodes across all standorte
  total_falls <- sum(
    vapply(
      standorte,
      function(st) length(xml2::xml_find_all(st, "./*[local-name()='Fall']")),
      integer(1)
    )
  )

  if (total_falls == 0L) {
    return(list())
  }

  out_list <- vector("list", total_falls)
  idx <- 1L

  for (si in seq_along(standorte)) {
    st <- standorte[[si]]
    standort_burnr <- xml2::xml_attr(st, "burnr")
    faelle <- xml2::xml_find_all(st, "./*[local-name()='Fall']")

    if (length(faelle) == 0L) {
      next
    }

    for (fi in seq_along(faelle)) {
      fall <- faelle[[fi]]
      fall_index <- fi

      key <- tibble::tibble(
        ent_id = ent_id,
        standort_burnr = standort_burnr,
        fall_index = fall_index
      )

      # 0/1 nodes
      admin_node <- xml2::xml_find_first(
        fall,
        "./*[local-name()='Administratives']"
      )
      neo_node <- xml2::xml_find_first(fall, "./*[local-name()='Neugeborene']")
      ktf_node <- xml2::xml_find_first(
        fall,
        "./*[local-name()='KostentraegerFall']"
      )
      psy_node <- xml2::xml_find_first(fall, "./*[local-name()='Psychiatrie']")

      # 0..n nodes
      diag_nodes <- xml2::xml_find_all(fall, "./*[local-name()='Diagnose']")
      beh_nodes <- xml2::xml_find_all(fall, "./*[local-name()='Behandlung']")
      rec_nodes <- xml2::xml_find_all(fall, "./*[local-name()='Rechnung']")
      med_nodes <- xml2::xml_find_all(fall, "./*[local-name()='Medikament']")
      pb_nodes <- xml2::xml_find_all(
        fall,
        "./*[local-name()='Patientenbewegung']"
      )

      # local helpers for attributes -> tibble
      node_attrs_tbl_local <- function(node) {
        if (length(node) == 0L) {
          return(tibble::tibble())
        }
        tibble::as_tibble(as.list(xml2::xml_attrs(node)))
      }

      nodes_attrs_tbl_local <- function(nodes) {
        if (length(nodes) == 0L) {
          return(tibble::tibble())
        }
        purrr::map_dfr(nodes, node_attrs_tbl_local)
      }

      # Behandlung may contain nested Operierende nodes. Expand rows if present.
      beh_tbl <- purrr::map_dfr(beh_nodes, function(b) {
        b_attrs <- node_attrs_tbl_local(b)
        ops <- xml2::xml_find_all(b, "./*[local-name()='Operierende']")
        if (length(ops) == 0L) {
          b_attrs
        } else {
          purrr::map_dfr(ops, function(op) {
            op_attrs <- node_attrs_tbl_local(op) |>
              dplyr::rename_with(~ paste0("op_", .x))
            dplyr::bind_cols(b_attrs, op_attrs)
          })
        }
      })

      entry <- list(
        Administratives = dplyr::bind_cols(
          node_attrs_tbl_local(admin_node),
          key
        ),
        Neugeborene = dplyr::bind_cols(node_attrs_tbl_local(neo_node), key),
        KostentraegerFall = dplyr::bind_cols(
          node_attrs_tbl_local(ktf_node),
          key
        ),
        Diagnose = dplyr::bind_cols(nodes_attrs_tbl_local(diag_nodes), key),
        Behandlung = dplyr::bind_cols(beh_tbl, key),
        Rechnung = dplyr::bind_cols(nodes_attrs_tbl_local(rec_nodes), key),
        Medikamente = dplyr::bind_cols(nodes_attrs_tbl_local(med_nodes), key),
        Psychiatrie = dplyr::bind_cols(node_attrs_tbl_local(psy_node), key),
        Patientenbewegungen = dplyr::bind_cols(
          nodes_attrs_tbl_local(pb_nodes),
          key
        )
      )

      out_list[[idx]] <- entry
      idx <- idx + 1L
    }
  }

  out_list
}
