#' Read and validate geography specifications from a YAML file
#'
#' Each geography must include `name`, `tigris_year`, `state`, `county`, and
#' `msa`. The states included in `msa` (based on the ACS name) must all be
#' listed in `state`. Multi-state MSAs are not yet supported.
#'
#' @param path Path to a YAML file with a top-level `geographies` key.
#' @returns A named list of geography specifications with `state` normalized to
#'   a USPS abbreviation.
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

  msa_states <- msa_name_states(geography$msa)
  unlisted_states <- setdiff(msa_states, geography$state)

  if (length(unlisted_states) > 0) {
    # TODO: Add support for multi-state MSAs
    cli::cli_abort(
      c(
        "Geography {.val {id}} has an MSA that includes state{?s}
        {.val {unlisted_states}} not listed in {.field state}.",
        "i" = "Multi-state MSAs are not yet supported."
      )
    )
  }

  if (!geography$state %in% msa_states) {
    cli::cli_abort(
      "Geography {.val {id}} has a {.field state} ({.val {geography$state}})
      that is not included in {.field msa} ({.val {geography$msa}})."
    )
  }

  geography$division <- validate_division(geography$division, id = id)

  geography
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

  division
}

#' Create a short key for a metropolitan statistical area and year
#'
#' Keys use the first principal city, state abbreviations, and year, e.g.
#' "baltimore_md_2023".
#' @noRd
make_msa_key <- function(msa_name, tigris_year) {
  city <- sub("[-,].*$", "", msa_name)
  states <- sub("^.+, ([A-Z-]+) (Metro|Micro) Area$", "\\1", msa_name)
  key <- tolower(paste(city, states, tigris_year, sep = "_"))
  gsub("[^a-z0-9]+", "_", key)
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
    )
  )

  values <- dplyr::mutate(
    values,
    state_key = paste(tolower(state_usps), tigris_year, sep = "_"),
    msa_key = make_msa_key(msa_name, tigris_year)
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
    msa_parks_ref = rlang::syms(paste0("msa_parks_", msa_key))
  )
}

#' Create values for national targets (one row per year)
#' @noRd
make_year_values <- function(geography_values) {
  dplyr::distinct(geography_values, tigris_year)
}

#' Create values for state-level targets (one row per state and year)
#' @noRd
make_state_values <- function(geography_values) {
  geography_values |>
    dplyr::distinct(state_key, state_usps, tigris_year) |>
    dplyr::mutate(
      us_states_ref = rlang::syms(paste0("us_states_", tigris_year))
    )
}

#' Create values for MSA-level targets (one row per MSA and year)
#' @noRd
make_msa_values <- function(geography_values) {
  geography_values |>
    dplyr::distinct(
      msa_key,
      msa_name,
      tigris_year,
      state_key,
      .keep_all = TRUE
    ) |>
    dplyr::select(
      msa_key,
      msa_name,
      tigris_year,
      us_msa_ref,
      state_fips_ref,
      counties_ref
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

#' Check that a county is located in a metropolitan statistical area
#'
#' Uses a point on the surface of the county to avoid errors from differences
#' in boundary generalization.
#'
#' @returns The `msa` input if the check passes.
check_county_in_msa <- function(county, msa) {
  county_pt <- county |>
    sf::st_geometry() |>
    sf::st_union() |>
    sf::st_point_on_surface() |>
    sf::st_transform(crs = sf::st_crs(msa))

  in_msa <- lengths(sf::st_intersects(county_pt, msa)) > 0

  if (!all(in_msa)) {
    cli::cli_abort(
      "County {.val {county$NAMELSAD}} is not located in
      {.val {msa$NAME}}."
    )
  }

  msa
}
