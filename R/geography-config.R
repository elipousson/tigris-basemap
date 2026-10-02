#' Read and validate geography specifications from a YAML file
#'
#' Each geography must include `name`, `tigris_year`, `state`, `county`, and
#' `msa`. The states in the MSA are parsed from the ACS name or listed with the
#' optional `msa_states` field. `state` must be one of the MSA states.
#'
#' @param path Path to a YAML file with a top-level `geographies` key.
#' @returns A named list of geography specifications with `state` and
#'   `msa_states` normalized to USPS abbreviations.
read_geographies <- function(path = "geographies.yml") {
  config <- yaml12::read_yaml(path)
  geographies <- config[["geographies"]]

  if (!is.list(geographies) || length(geographies) == 0) {
    cli::cli_abort(
      "{.file {path}} must include a non-empty {.field geographies} key."
    )
  }

  ids <- names(geographies)

  if (is.null(ids) || any(ids == "") || anyDuplicated(ids)) {
    cli::cli_abort(
      "Each entry in {.field geographies} must have a unique name."
    )
  }

  invalid_ids <- ids[!grepl("^[a-z][a-z0-9_]*$", ids)]

  if (length(invalid_ids) > 0) {
    cli::cli_abort(
      "Geography identifiers must use only lowercase letters, numbers, and
      underscores: {.val {invalid_ids}}"
    )
  }

  geographies <- lapply(
    ids,
    \(id) validate_geography(geographies[[id]], id = id)
  )

  rlang::set_names(geographies, ids)
}

#' Validate a single geography specification
#'
#' @param geography A list with a geography specification.
#' @param id Identifier for the geography used in error messages.
#' @noRd
validate_geography <- function(geography, id) {
  required <- c("name", "tigris_year", "state", "county", "msa")
  missing <- setdiff(required, names(geography))

  if (length(missing) > 0) {
    cli::cli_abort(
      "Geography {.val {id}} is missing required field{?s} {.field {missing}}."
    )
  }

  geography$state <- state_to_usps(geography$state, id = id)
  geography$msa_states <- validate_msa_states(geography, id = id)

  if (!geography$state %in% geography$msa_states) {
    cli::cli_abort(
      "Geography {.val {id}} has a {.field state} ({.val {geography$state}})
      that is not one of the MSA states ({.val {geography$msa_states}})."
    )
  }

  geography$division <- validate_division(geography$division, id = id)

  geography
}

#' Get and validate the states included in a metropolitan statistical area
#'
#' States are parsed from the ACS name of the MSA (e.g. "DC-VA-MD-WV" in
#' "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area"). If
#' `msa_states` is provided, it must match the parsed states. If the name can't
#' be parsed (e.g. an older ACS vintage with a different format), `msa_states`
#' is required and used on its own.
#'
#' @returns A character vector of USPS abbreviations.
#' @noRd
validate_msa_states <- function(geography, id) {
  parsed <- tryCatch(msa_name_states(geography$msa), error = \(cnd) NULL)
  listed <- geography$msa_states

  if (is.null(listed)) {
    if (is.null(parsed)) {
      cli::cli_abort(
        c(
          "Geography {.val {id}} must provide {.field msa_states} because the
          states can't be parsed from {.field msa}: {.val {geography$msa}}",
          "i" = "ACS names usually look like
          {.val Baltimore-Columbia-Towson, MD Metro Area}."
        )
      )
    }

    return(parsed)
  }

  listed <- unname(vapply(listed, \(x) state_to_usps(x, id = id), ""))

  if (anyDuplicated(listed)) {
    cli::cli_abort(
      "Geography {.val {id}} {.field msa_states} has duplicate states:
      {.val {listed[duplicated(listed)]}}"
    )
  }

  if (is.null(parsed)) {
    return(listed)
  }

  missing <- setdiff(parsed, listed)
  extra <- setdiff(listed, parsed)

  if (length(missing) > 0 || length(extra) > 0) {
    cli::cli_abort(
      c(
        "Geography {.val {id}} {.field msa_states} doesn't match the states in
        {.field msa} ({.val {parsed}}).",
        "x" = if (length(missing) > 0) "Missing: {.val {missing}}",
        "x" = if (length(extra) > 0) "Not in the MSA: {.val {extra}}"
      )
    )
  }

  # Use the order from the name so MSA keys are consistent
  parsed
}

#' Convert a state USPS abbreviation, name, or FIPS code to a USPS abbreviation
#' @noRd
state_to_usps <- function(state, id) {
  states <- dplyr::distinct(
    tigris::fips_codes,
    state,
    state_code,
    state_name
  )

  state <- trimws(as.character(state))
  match_idx <- c(
    match(toupper(state), states$state),
    match(tolower(state), tolower(states$state_name)),
    match(sprintf("%02d", suppressWarnings(as.integer(state))), states$state_code)
  )
  match_idx <- match_idx[!is.na(match_idx)]

  if (length(match_idx) == 0) {
    cli::cli_abort(
      "Geography {.val {id}} has an invalid {.field state}: {.val {state}}"
    )
  }

  states$state[[match_idx[[1]]]]
}

#' Get state USPS abbreviations from an ACS metropolitan statistical area name
#'
#' @param msa_name An ACS name such as "Baltimore-Columbia-Towson, MD Metro
#'   Area".
#' @noRd
msa_name_states <- function(msa_name) {
  pattern <- "^.+, ([A-Z]{2}(-[A-Z]{2})*) (Metro|Micro) Area$"

  if (!rlang::is_string(msa_name) || !grepl(pattern, msa_name)) {
    cli::cli_abort(
      "{.field msa} must be an ACS name such as
      {.val Baltimore-Columbia-Towson, MD Metro Area}, not {.val {msa_name}}."
    )
  }

  strsplit(sub(pattern, "\\1", msa_name), "-")[[1]]
}

#' Validate an optional division specification
#'
#' @param division `NULL` or a list with a `type` of "tract", "zcta", or
#'   "custom". Custom divisions require exactly one of `path` or `url`.
#' @noRd
validate_division <- function(division, id) {
  if (is.null(division)) {
    return(NULL)
  }

  division_types <- c("tract", "zcta", "custom")

  if (!rlang::is_string(division$type) || !division$type %in% division_types) {
    cli::cli_abort(
      "Geography {.val {id}} {.field division.type} must be one of
      {.or {.val {division_types}}}."
    )
  }

  has_path <- !is.null(division$path)
  has_url <- !is.null(division$url)

  if (division$type == "custom" && has_path == has_url) {
    cli::cli_abort(
      "Geography {.val {id}} must provide exactly one of
      {.field division.path} or {.field division.url} for a custom division."
    )
  }

  if (division$type != "custom" && (has_path || has_url)) {
    cli::cli_abort(
      "Geography {.val {id}} {.field division.path} and {.field division.url}
      are only used when {.field division.type} is {.val custom}."
    )
  }

  if (has_path && !file.exists(division$path)) {
    cli::cli_abort(
      "Geography {.val {id}} {.field division.path} does not exist:
      {.file {division$path}}"
    )
  }

  if (isTRUE(division$snap_to_tracts) && is.null(division$name_col)) {
    cli::cli_abort(
      "Geography {.val {id}} must provide {.field division.name_col} when
      {.field division.snap_to_tracts} is {.val true}."
    )
  }

  check_processing_steps(
    division$processing_steps,
    id = paste0(id, ".division"),
    available = "county"
  )

  division
}

#' Create a short key for a metropolitan statistical area and year
#'
#' Keys use the first principal city, state abbreviations, and year, e.g.
#' "baltimore_md_2023" or "washington_dc_va_md_wv_2023".
#' @noRd
make_msa_key <- function(msa_name, msa_states, tigris_year) {
  city <- sub("[-,].*$", "", msa_name)
  key <- tolower(paste(c(city, msa_states, tigris_year), collapse = "_"))
  gsub("[^a-z0-9]+", "_", key)
}

#' Create a call to `list()` with symbols for targets with a prefix and keys,
#' e.g. `list(counties_dc_2023, counties_md_2023)`
#'
#' The call is created in a function (rather than in a `dplyr::mutate()` call)
#' so `!!!` isn't evaluated by dplyr.
#' @noRd
make_ref_list <- function(prefix, keys) {
  rlang::call2("list", !!!rlang::syms(paste0(prefix, keys)))
}

#' Convert USPS abbreviations to state FIPS codes
#' @noRd
usps_to_fips <- function(usps) {
  states <- dplyr::distinct(tigris::fips_codes, state, state_code)
  states$state_code[match(usps, states$state)]
}

#' Create a data frame of values for `tarchetypes::tar_map()`
#'
#' Returns one row per geography. Columns ending in `_ref` are symbols for
#' targets shared across geographies (by year, by state and year, or by MSA and
#' year) so shared data is only loaded once.
#'
#' @param geographies A named list from [read_geographies()].
make_geography_values <- function(geographies) {
  values <- dplyr::tibble(
    id = names(geographies),
    tigris_year = vapply(geographies, \(x) as.integer(x$tigris_year), 1L),
    state_usps = vapply(geographies, \(x) x$state, ""),
    county_name = vapply(geographies, \(x) x$county, ""),
    msa_name = vapply(geographies, \(x) x$msa, ""),
    msa_states = unname(lapply(geographies, \(x) x$msa_states)),
    division_type = vapply(
      geographies,
      \(x) x$division$type %||% NA_character_,
      ""
    ),
    division_path = vapply(
      geographies,
      \(x) x$division$path %||% NA_character_,
      ""
    ),
    division_url = vapply(
      geographies,
      \(x) x$division$url %||% NA_character_,
      ""
    ),
    division_name_col = vapply(
      geographies,
      \(x) x$division$name_col %||% NA_character_,
      ""
    ),
    division_snap_to_tracts = vapply(
      geographies,
      \(x) isTRUE(x$division$snap_to_tracts),
      TRUE
    ),
    division_steps = unname(lapply(
      geographies,
      \(x) x$division$processing_steps
    ))
  )

  values <- dplyr::mutate(
    values,
    state_key = paste(tolower(state_usps), tigris_year, sep = "_"),
    msa_key = unlist(Map(make_msa_key, msa_name, msa_states, tigris_year))
  )

  msa_keys <- dplyr::distinct(values, msa_key, msa_name)

  if (anyDuplicated(msa_keys$msa_key)) {
    cli::cli_abort(
      "Multiple MSAs share the key{?s}
      {.val {msa_keys$msa_key[duplicated(msa_keys$msa_key)]}}."
    )
  }

  dplyr::mutate(
    values,
    # Custom divisions from a file use a tracked file target as the source
    division_src_ref = unname(Map(
      \(id, path, url) {
        if (is.na(path)) {
          return(url)
        }
        rlang::sym(paste0("division_file_", id))
      },
      id,
      division_path,
      division_url
    )),
    us_msa_ref = rlang::syms(paste0("us_msa_", tigris_year)),
    state_fips_ref = rlang::syms(paste0("state_fips_", state_key)),
    counties_ref = rlang::syms(paste0("counties_", state_key)),
    msa_ref = rlang::syms(paste0("msa_", msa_key)),
    msa_counties_ref = rlang::syms(paste0("msa_counties_", msa_key)),
    msa_water_ref = rlang::syms(paste0("msa_water_", msa_key)),
    msa_roads_ref = rlang::syms(paste0("msa_roads_", msa_key)),
    msa_parks_ref = rlang::syms(paste0("msa_parks_", msa_key)),
    msa_rail_lines_ref = rlang::syms(paste0("msa_rail_lines_", msa_key))
  )
}

#' Create values for national targets (one row per year)
#' @noRd
make_year_values <- function(geography_values) {
  dplyr::distinct(geography_values, tigris_year)
}

#' Create values for state-level targets (one row per state and year)
#'
#' Includes the state of each focal county and every state in each MSA,
#' de-duplicated so data for a state (e.g. Maryland for both the Baltimore and
#' Washington, DC MSAs) is only loaded once for each year.
#' @noRd
make_state_values <- function(geography_values) {
  msa_states <- dplyr::tibble(
    state_usps = unlist(geography_values$msa_states),
    tigris_year = rep(
      geography_values$tigris_year,
      lengths(geography_values$msa_states)
    )
  )

  geography_values |>
    dplyr::select(state_usps, tigris_year) |>
    dplyr::bind_rows(msa_states) |>
    dplyr::distinct() |>
    dplyr::mutate(
      state_key = paste(tolower(state_usps), tigris_year, sep = "_"),
      us_states_ref = rlang::syms(paste0("us_states_", tigris_year))
    )
}

#' Create values for MSA-level targets (one row per MSA and year)
#'
#' Columns ending in `_refs` are calls to `list()` with the state-level targets
#' for every state in the MSA.
#' @noRd
make_msa_values <- function(geography_values) {
  msa_values <- dplyr::distinct(
    geography_values,
    msa_key,
    msa_name,
    tigris_year,
    .keep_all = TRUE
  )

  state_keys <- Map(
    \(states, year) paste(tolower(states), year, sep = "_"),
    msa_values$msa_states,
    msa_values$tigris_year
  )

  dplyr::tibble(
    msa_key = msa_values$msa_key,
    msa_name = msa_values$msa_name,
    tigris_year = msa_values$tigris_year,
    msa_state_fips = unname(lapply(msa_values$msa_states, usps_to_fips)),
    us_msa_ref = msa_values$us_msa_ref,
    counties_refs = unname(lapply(state_keys, \(x) make_ref_list("counties_", x))),
    state_roads_refs = unname(lapply(
      state_keys,
      \(x) make_ref_list("state_roads_", x)
    )),
    rail_lines_refs = unname(lapply(
      state_keys,
      \(x) make_ref_list("rail_lines_", x)
    ))
  )
}

#' Filter metropolitan statistical areas by name
#'
#' @param msa_data ACS data for metropolitan statistical areas with a `NAME`
#'   column.
#' @param msa_name Name of a metropolitan statistical area matching the `NAME`
#'   column of `msa_data`.
filter_msa <- function(msa_data, msa_name) {
  msa <- dplyr::filter(msa_data, .data[["NAME"]] == msa_name)

  if (nrow(msa) != 1) {
    cli::cli_abort(
      "{.arg msa_name} must match exactly one metropolitan statistical area,
      not {nrow(msa)}: {.val {msa_name}}"
    )
  }

  msa
}

#' Select the counties in a metropolitan statistical area
#'
#' Counties are matched on the CBSA code in the `CBSAFP` column of TIGER/Line
#' counties, which is the same as the `GEOID` of the ACS metropolitan
#' statistical area.
#'
#' @param counties Counties from [load_county()] with a `CBSAFP` column for
#'   every state in the MSA, e.g. from [bind_sf()].
#' @param msa A single metropolitan statistical area from [filter_msa()].
#' @param state_fips FIPS codes for the states listed for the MSA. Used to check
#'   that the selected counties come from all listed states and cover the MSA
#'   (which fails if a state is missing from the list).
#' @param min_coverage Minimum share of the MSA area covered by the selected
#'   counties.
#' @returns The (unclipped) counties in the MSA.
select_msa_counties <- function(
  counties,
  msa,
  state_fips = NULL,
  min_coverage = 0.99
) {
  msa_counties <- dplyr::filter(counties, .data[["CBSAFP"]] %in% msa$GEOID)

  if (nrow(msa_counties) == 0) {
    cli::cli_abort(
      c(
        "No counties have a {.field CBSAFP} matching {.val {msa$NAME}}
        ({.val {msa$GEOID}}).",
        "i" = "The county and ACS data may use different metropolitan area
        delineations."
      )
    )
  }

  if (is.null(state_fips)) {
    return(msa_counties)
  }

  unmatched <- setdiff(state_fips, msa_counties$STATEFP)

  if (length(unmatched) > 0) {
    cli::cli_abort(
      "No counties in {.val {msa$NAME}} are in state{?s} with FIPS code{?s}
      {.val {unmatched}}. Check {.field msa_states}."
    )
  }

  msa_area <- sum(as.numeric(sf::st_area(msa)))
  covered_area <- sum(as.numeric(sf::st_area(
    sf::st_intersection(sf::st_union(msa_counties), sf::st_geometry(msa))
  )))

  if (covered_area / msa_area < min_coverage) {
    cli::cli_abort(
      c(
        "The selected counties cover only
        {round(100 * covered_area / msa_area, 1)}% of {.val {msa$NAME}}.",
        "i" = "A state in the MSA may be missing from {.field msa_states}."
      )
    )
  }

  msa_counties
}

#' Combine a list of `sf` objects
#'
#' @param x A list of `sf` objects with the same columns.
#' @param distinct_by Optional column used to drop duplicate features, e.g.
#'   features in national files loaded with a filter for each state.
#' @param filter_by Optional `sf` object. Only features intersecting its
#'   bounding box are kept (geometry is not changed).
bind_sf <- function(x, distinct_by = NULL, filter_by = NULL) {
  x <- do.call(rbind, unname(x))

  if (!is.null(distinct_by)) {
    x <- x[!duplicated(x[[distinct_by]]), ]
  }

  if (!is.null(filter_by)) {
    bbox <- sf::st_as_sfc(sf::st_bbox(filter_by))
    x <- x[lengths(sf::st_intersects(x, bbox)) > 0, ]
  }

  x
}

#' Check that a county is located in a metropolitan statistical area
#'
#' Compares the CBSA code in the `CBSAFP` column of the county to the `GEOID`
#' of the metropolitan statistical area.
#'
#' @returns The `msa` input if the check passes.
check_county_in_msa <- function(county, msa) {
  if (!all(county$CBSAFP %in% msa$GEOID)) {
    cli::cli_abort(
      "County {.val {county$NAMELSAD}} is not located in
      {.val {msa$NAME}}."
    )
  }

  msa
}
