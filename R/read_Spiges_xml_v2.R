# Unten ist eine tidyverse-orientierte R-Funktion read_spiges_xml(), die eine SPiGes-XML (v1.4) einliest und als Liste von Dataframes/Tibbles zurückgibt:

# Administratives

# Neugeborene

# KostentraegerFall

# Diagnose

# Behandlung

# Rechnung

# Medikamente

# Psychiatrie

# Patientenbewegungen

# Wichtig: In deinem Beispiel liegen die Werte als Attribute der jeweiligen XML-Nodes (z.B. <Administratives .../>). Die Funktion liest diese Attribute als Spalten ein. Zusätzlich fügt sie Schlüsselspalten hinzu (ent_id, standort_burnr, fall_index), damit man Tabellen wieder verbinden kann.

# read_spiges_xml <- function(xml_path, xsd_path = NULL, validate = FALSE) {
#   stopifnot(file.exists(xml_path))

#   suppressPackageStartupMessages({
#     library(xml2)
#     library(dplyr)
#     library(purrr)
#     library(tidyr)
#     library(tibble)
#     library(stringr)
#   })

#   # --- optional: XSD-Validierung (falls gewünscht) ---
#   if (isTRUE(validate)) {
#     if (is.null(xsd_path)) {
#       stop("validate=TRUE, aber xsd_path ist NULL.")
#     }
#     stopifnot(file.exists(xsd_path))
#     schema <- xml2::read_xml(xsd_path)
#     ok <- xml2::xml_schema_validate(xml2::read_xml(xml_path), schema)
#     if (!isTRUE(ok)) {
#       stop("XML ist gemäss XSD nicht valide (xml_schema_validate == FALSE).")
#     }
#   }

#   doc <- xml2::read_xml(xml_path)

#   # Namespace-unabhängige XPath-Helfer (arbeitet mit local-name())
#   xp <- function(name) paste0(".//*[local-name()='", name, "']")
#   xp_child <- function(parent, name) paste0("./*[local-name()='", name, "']")

#   # Root-Infos
#   ent_id <- xml2::xml_attr(doc, "ent_id")

#   # Standorte (direkte Kinder des Root)
#   standorte <- xml2::xml_find_all(doc, "./*[local-name()='Standort']")

#   # Hilfsfunktion: Attribute eines Nodes -> tibble (1 Zeile)
#   attrs_tbl <- function(node) {
#     if (length(node) == 0) {
#       return(tibble())
#     }
#     as.list(xml2::xml_attrs(node)) |>
#       tibble::as_tibble()
#   }

#   # Hilfsfunktion: Nodes (mehrere) -> tibble (mehrere Zeilen) + key cols
#   nodes_to_tbl <- function(nodes, key) {
#     if (length(nodes) == 0) {
#       return(tibble())
#     }
#     purrr::map_dfr(nodes, \(nd) {
#       attrs_tbl(nd) |>
#         bind_cols(key)
#     })
#   }

#   # Wir laufen Standort -> Fall und sammeln pro Fall Keys + Nodes
#   collected <- purrr::map2(standorte, seq_along(standorte), \(st, st_i) {
#     standort_burnr <- xml2::xml_attr(st, "burnr")
#     faelle <- xml2::xml_find_all(st, xp_child("Standort", "Fall"))

#     purrr::map2(faelle, seq_along(faelle), \(fall, fi) {
#       key <- tibble(
#         ent_id = ent_id,
#         standort_burnr = standort_burnr,
#         fall_index = fi
#       )

#       # Einzelnodes (0/1 pro Fall)
#       admin_node <- xml2::xml_find_first(
#         fall,
#         xp_child("Fall", "Administratives")
#       )
#       neo_node <- xml2::xml_find_first(fall, xp_child("Fall", "Neugeborene"))
#       ktf_node <- xml2::xml_find_first(
#         fall,
#         xp_child("Fall", "KostentraegerFall")
#       )
#       psy_node <- xml2::xml_find_first(fall, xp_child("Fall", "Psychiatrie"))

#       # Multinodes (0..n pro Fall)
#       diag_nodes <- xml2::xml_find_all(fall, xp_child("Fall", "Diagnose"))
#       beh_nodes <- xml2::xml_find_all(fall, xp_child("Fall", "Behandlung"))
#       rec_nodes <- xml2::xml_find_all(fall, xp_child("Fall", "Rechnung"))
#       med_nodes <- xml2::xml_find_all(fall, xp_child("Fall", "Medikament"))
#       pb_nodes <- xml2::xml_find_all(
#         fall,
#         xp_child("Fall", "Patientenbewegung")
#       )

#       # Behandlung: optional Operierende (0..n). Wenn vorhanden, pro Operierende eine Zeile.
#       behandlung_tbl <- purrr::map_dfr(beh_nodes, \(b) {
#         b_attrs <- attrs_tbl(b)

#         ops <- xml2::xml_find_all(b, xp_child("Behandlung", "Operierende"))
#         if (length(ops) == 0) {
#           b_attrs |>
#             bind_cols(key)
#         } else {
#           purrr::map_dfr(ops, \(op) {
#             op_attrs <- attrs_tbl(op) |>
#               rename_with(~ paste0("op_", .x))
#             b_attrs |>
#               bind_cols(op_attrs) |>
#               bind_cols(key)
#           })
#         }
#       })

#       list(
#         Administratives = attrs_tbl(admin_node) |> bind_cols(key),
#         Neugeborene = attrs_tbl(neo_node) |> bind_cols(key),
#         KostentraegerFall = attrs_tbl(ktf_node) |> bind_cols(key),
#         Psychiatrie = attrs_tbl(psy_node) |> bind_cols(key),

#         Diagnose = nodes_to_tbl(diag_nodes, key),
#         Behandlung = behandlung_tbl,
#         Rechnung = nodes_to_tbl(rec_nodes, key),
#         Medikamente = nodes_to_tbl(med_nodes, key),
#         Patientenbewegungen = nodes_to_tbl(pb_nodes, key)
#       )
#     })
#   }) |>
#     purrr::list_flatten()

#   # Jetzt alles über alle Fälle zusammenführen
#   out <- list(
#     Administratives = purrr::map_dfr(collected, "Administratives") |>
#       distinct(),
#     Neugeborene = purrr::map_dfr(collected, "Neugeborene") |> distinct(),
#     KostentraegerFall = purrr::map_dfr(collected, "KostentraegerFall") |>
#       distinct(),
#     Diagnose = purrr::map_dfr(collected, "Diagnose") |> distinct(),
#     Behandlung = purrr::map_dfr(collected, "Behandlung") |> distinct(),
#     Rechnung = purrr::map_dfr(collected, "Rechnung") |> distinct(),
#     Medikamente = purrr::map_dfr(collected, "Medikamente") |> distinct(),
#     Psychiatrie = purrr::map_dfr(collected, "Psychiatrie") |> distinct(),
#     Patientenbewegungen = purrr::map_dfr(collected, "Patientenbewegungen") |>
#       distinct()
#   )

#   # Optional: leere Tibbles wirklich als 0x0 statt nur key-spalten?
#   # (hier lassen wir sie mit key-spalten, weil es oft praktisch ist)

#   out
# }

# res <- read_spiges_xml("do-d-14.04-SPIGES-2024-01.xml")
# str(res$Administratives)
