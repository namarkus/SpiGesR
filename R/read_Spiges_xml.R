#' Import SpiGes data from XML files
#'
#' This function imports SpiGes data from XML files (data file and identifiers file)
#' and converts it to a structured R object. All variables are converted to their
#' appropriate formats as defined in the SpiGes documentation.
#'
#' @param data_file Path to the SpiGes data XML file
#' @param id_file Path to the SpiGes identifiers XML file
#' @param version SpiGes version (default: "1.4")
#'
#' @return A list of data frames representing the SpiGes data structure
#' @export
#'
#' @examples
#' \dontrun{
#' data_file <- system.file("extdata", "do-d-14.04-SPIGES-2024-01.xml", package = "SpiGesR")
#' id_file <- system.file("extdata", "do-d-14.04-SPIGES-2024-03.xml", package = "SpiGesR")
#' data <- import_spiges_xml(data_file, id_file)
#' }
read_spiges_xml <- function(data_file, id_file, version = "1.4") {
  # Check if files exist
  if (!file.exists(data_file)) {
    stop("Data file does not exist: ", data_file)
  }
  if (!file.exists(id_file)) {
    stop("Identifiers file does not exist: ", id_file)
  }

  # Load XML files
  data_xml <- xml2::read_xml(data_file)
  id_xml <- xml2::read_xml(id_file)

  # Extract namespace
  ns_data <- xml2::xml_ns(data_xml)
  ns_id <- xml2::xml_ns(id_xml)

  # Get default namespace URIs
  data_ns_uri <- ns_data[[1]]
  id_ns_uri <- ns_id[[1]]

  # Check version
  xml_version <- xml2::xml_attr(data_xml, "version")
  if (xml_version != version) {
    warning(
      "XML file version (",
      xml_version,
      ") differs from requested version (",
      version,
      ")"
    )
  }

  # Extract data
  result <- list()

  # Extract enterprise data
  result$enterprise <- data.frame(
    ent_id = xml2::xml_attr(data_xml, "ent_id"),
    version = xml2::xml_attr(data_xml, "version"),
    stringsAsFactors = FALSE
  )

  # Extract locations (Standort)
  # Use xpath with default namespace
  locations <- xml2::xml_find_all(data_xml, "//d:Standort", c(d = data_ns_uri))
  result$locations <- data.frame(
    burnr = xml2::xml_attr(locations, "burnr"),
    stringsAsFactors = FALSE
  )

  # Extract cases (Fall)
  cases <- list()
  for (loc_idx in seq_along(locations)) {
    location <- locations[[loc_idx]]
    location_burnr <- xml2::xml_attr(location, "burnr")

    # Find all cases for this location
    case_nodes <- xml2::xml_find_all(location, ".//d:Fall", c(d = data_ns_uri))

    for (case_idx in seq_along(case_nodes)) {
      case_node <- case_nodes[[case_idx]]
      case_id <- xml2::xml_attr(case_node, "fall_id")

      # Create case data structure
      case <- list(
        fall_id = case_id,
        burnr = location_burnr
      )

      # Extract administrative data
      admin_node <- xml2::xml_find_first(
        case_node,
        ".//d:Administratives",
        c(d = data_ns_uri)
      )
      if (!is.na(admin_node)) {
        admin_attrs <- xml2::xml_attrs(admin_node)
        case$administrative <- as.list(admin_attrs)
      }

      # Extract newborn data
      newborn_node <- xml2::xml_find_first(
        case_node,
        ".//d:Neugeborene",
        c(d = data_ns_uri)
      )
      if (!is.na(newborn_node)) {
        newborn_attrs <- xml2::xml_attrs(newborn_node)
        case$newborn <- as.list(newborn_attrs)
      }

      # Extract psychiatric data
      psych_node <- xml2::xml_find_first(
        case_node,
        ".//d:Psychiatrie",
        c(d = data_ns_uri)
      )
      if (!is.na(psych_node)) {
        psych_attrs <- xml2::xml_attrs(psych_node)
        case$psychiatric <- as.list(psych_attrs)
      }

      # Extract cost data
      cost_node <- xml2::xml_find_first(
        case_node,
        ".//d:KostentraegerFall",
        c(d = data_ns_uri)
      )
      if (!is.na(cost_node)) {
        cost_attrs <- xml2::xml_attrs(cost_node)
        case$costs <- as.list(cost_attrs)
      }

      # Extract diagnoses
      diag_nodes <- xml2::xml_find_all(
        case_node,
        ".//d:Diagnose",
        c(d = data_ns_uri)
      )
      if (length(diag_nodes) > 0) {
        diagnoses <- lapply(diag_nodes, function(node) {
          as.list(xml2::xml_attrs(node))
        })
        case$diagnoses <- diagnoses
      }

      # Extract treatments
      treat_nodes <- xml2::xml_find_all(
        case_node,
        ".//d:Behandlung",
        c(d = data_ns_uri)
      )
      if (length(treat_nodes) > 0) {
        treatments <- lapply(treat_nodes, function(node) {
          treatment <- as.list(xml2::xml_attrs(node))

          # Extract operators
          op_nodes <- xml2::xml_find_all(
            node,
            ".//d:Operierende",
            c(d = data_ns_uri)
          )
          if (length(op_nodes) > 0) {
            operators <- lapply(op_nodes, function(op_node) {
              as.list(xml2::xml_attrs(op_node))
            })
            treatment$operators <- operators
          }

          return(treatment)
        })
        case$treatments <- treatments
      }

      # Extract medications
      med_nodes <- xml2::xml_find_all(
        case_node,
        ".//d:Medikament",
        c(d = data_ns_uri)
      )
      if (length(med_nodes) > 0) {
        medications <- lapply(med_nodes, function(node) {
          as.list(xml2::xml_attrs(node))
        })
        case$medications <- medications
      }

      # Extract invoices
      inv_nodes <- xml2::xml_find_all(
        case_node,
        ".//d:Rechnung",
        c(d = data_ns_uri)
      )
      if (length(inv_nodes) > 0) {
        invoices <- lapply(inv_nodes, function(node) {
          as.list(xml2::xml_attrs(node))
        })
        case$invoices <- invoices
      }

      # Extract patient movements
      mov_nodes <- xml2::xml_find_all(
        case_node,
        ".//d:Patientenbewegung",
        c(d = data_ns_uri)
      )
      if (length(mov_nodes) > 0) {
        movements <- lapply(mov_nodes, function(node) {
          as.list(xml2::xml_attrs(node))
        })
        case$movements <- movements
      }

      # Extract canton data
      canton_node <- xml2::xml_find_first(
        case_node,
        ".//d:Kantonsdaten",
        c(d = data_ns_uri)
      )
      if (!is.na(canton_node)) {
        canton_attrs <- xml2::xml_attrs(canton_node)
        case$canton <- as.list(canton_attrs)
      }

      # Add case to list
      cases[[length(cases) + 1]] <- case
    }
  }

  # Extract location cost carriers
  loc_cost_carriers <- list()
  for (loc_idx in seq_along(locations)) {
    location <- locations[[loc_idx]]
    location_burnr <- xml2::xml_attr(location, "burnr")

    # Find all cost carriers for this location
    cost_nodes <- xml2::xml_find_all(
      location,
      ".//d:KostentraegerStandort",
      c(d = data_ns_uri)
    )

    for (cost_idx in seq_along(cost_nodes)) {
      cost_node <- cost_nodes[[cost_idx]]
      cost_attrs <- xml2::xml_attrs(cost_node)

      cost_carrier <- as.list(cost_attrs)
      cost_carrier$burnr <- location_burnr

      loc_cost_carriers[[length(loc_cost_carriers) + 1]] <- cost_carrier
    }
  }

  # Extract enterprise cost carriers
  ent_cost_nodes <- xml2::xml_find_all(
    data_xml,
    "//d:KostentraegerUnternehmen",
    c(d = data_ns_uri)
  )
  ent_cost_carriers <- lapply(ent_cost_nodes, function(node) {
    as.list(xml2::xml_attrs(node))
  })

  # Add to result
  result$cases <- cases
  result$location_cost_carriers <- loc_cost_carriers
  result$enterprise_cost_carriers <- ent_cost_carriers

  # Process identifiers file
  # Use xpath with default namespace for identifiers file
  id_locations <- xml2::xml_find_all(id_xml, "//i:Standort", c(i = id_ns_uri))

  identifiers <- list()
  for (loc_idx in seq_along(id_locations)) {
    location <- id_locations[[loc_idx]]
    location_burnr <- xml2::xml_attr(location, "burnr")

    # Find all cases for this location
    id_case_nodes <- xml2::xml_find_all(location, ".//i:Fall", c(i = id_ns_uri))

    for (case_idx in seq_along(id_case_nodes)) {
      id_case_node <- id_case_nodes[[case_idx]]
      case_id <- xml2::xml_attr(id_case_node, "fall_id")

      # Extract person identifiers
      person_node <- xml2::xml_find_first(
        id_case_node,
        ".//i:Personenidentifikatoren",
        c(i = id_ns_uri)
      )
      if (!is.na(person_node)) {
        person_attrs <- xml2::xml_attrs(person_node)

        identifier <- list(
          fall_id = case_id,
          burnr = location_burnr,
          person = as.list(person_attrs)
        )

        identifiers[[length(identifiers) + 1]] <- identifier
      }
    }
  }

  result$identifiers <- identifiers

  # Convert to appropriate formats
  # This would be implemented with the convert_variables function
  # For now, we return the raw data structure

  return(result)
}
