#' Read and validate data source specifications from a YAML file
#'
#' @param path Path to a YAML file with a top-level `sources` key.
#' @returns A named list of source specifications.
read_sources <- function(path = "sources.yml") {
  sources <- read_config_section(path, "sources")

  source_types <- c("tigris", "arcgis", "acs")

  for (id in names(sources)) {
    src <- sources[[id]]

    check_required_fields(src, c("name", "description", "type"), id = id)

    if (!rlang::is_string(src$type) || !src$type %in% source_types) {
      cli::cli_abort(
        "Source {.val {id}} {.field type} must be one of
        {.or {.val {source_types}}}."
      )
    }

    if (src$type == "arcgis" && !rlang::is_string(src$url)) {
      cli::cli_abort("Source {.val {id}} must provide a {.field url}.")
    }

    if (src$type != "arcgis" && !rlang::is_string(src[["function"]])) {
      cli::cli_abort("Source {.val {id}} must provide a {.field function}.")
    }

    check_process_args(src$defaults, id = id, field = "defaults")
  }

  sources
}

#' Read and validate basemap specifications from a YAML file
#'
#' Each layer is resolved to a list of loader arguments (`args`) by combining
#' the source `defaults` with the layer `process` values and, for ArcGIS
#' sources, the source `url`.
#'
#' @param path Path to a YAML file with a top-level `basemaps` key.
#' @param sources A named list from [read_sources()].
#' @returns A named list of basemap specifications.
read_basemaps <- function(path = "basemaps.yml", sources = read_sources()) {
  basemaps <- read_config_section(path, "basemaps")

  for (basemap in names(basemaps)) {
    spec <- basemaps[[basemap]]

    check_required_fields(spec, "layers", id = basemap)

    layer_names <- c(names(spec$layers), names(spec$staging))

    if (anyDuplicated(layer_names)) {
      cli::cli_abort(
        "Basemap {.val {basemap}} layer names must be unique across
        {.field layers} and {.field staging}:
        {.val {layer_names[duplicated(layer_names)]}}"
      )
    }

    for (section in c("layers", "staging")) {
      for (layer in names(spec[[section]])) {
        spec[[section]][[layer]] <- resolve_layer(
          spec[[section]][[layer]],
          sources = sources,
          id = paste0(basemap, ".", section, ".", layer)
        )
      }
    }

    basemaps[[basemap]] <- spec
  }

  basemaps
}

#' Resolve a basemap layer specification to loader arguments
#' @noRd
resolve_layer <- function(layer, sources, id) {
  check_required_fields(layer, "source", id = id)

  src <- sources[[layer$source]]

  if (is.null(src)) {
    cli::cli_abort(
      "Layer {.val {id}} has a {.field source} not found in sources:
      {.val {layer$source}}"
    )
  }

  check_process_args(layer$process, id = id, field = "process")

  layer$args <- utils::modifyList(
    source_args(sources, layer$source),
    layer$process %||% list()
  )

  layer
}

#' Get loader arguments for a source
#'
#' @param sources A named list from [read_sources()].
#' @param source Source identifier.
#' @returns A named list of the source defaults and, for ArcGIS sources, the
#'   source `url`.
source_args <- function(sources, source) {
  src <- sources[[source]]

  if (is.null(src)) {
    cli::cli_abort("{.arg source} not found in sources: {.val {source}}")
  }

  args <- src$defaults %||% list()

  if (src$type == "arcgis") {
    args <- c(list(url = src$url), args)
  }

  args
}

#' Get loader arguments for a basemap layer
#'
#' Use with `!!!` in a target command so the arguments are inserted into the
#' command and changes invalidate only the affected targets.
#'
#' @param basemaps A named list from [read_basemaps()].
#' @param basemap Basemap identifier, e.g. "county" or "msa".
#' @param layer Layer name from the basemap `layers` or `staging`.
layer_args <- function(basemaps, basemap, layer) {
  spec <- basemaps[[basemap]]

  if (is.null(spec)) {
    cli::cli_abort("{.arg basemap} not found in basemaps: {.val {basemap}}")
  }

  layer_spec <- spec$layers[[layer]] %||% spec$staging[[layer]]

  if (is.null(layer_spec)) {
    cli::cli_abort(
      "Layer {.val {layer}} not found in basemap {.val {basemap}}."
    )
  }

  layer_spec$args
}

#' Read a named, non-empty top-level section of a YAML file
#' @noRd
read_config_section <- function(path, section) {
  config <- yaml12::read_yaml(path)
  entries <- config[[section]]

  if (!is.list(entries) || length(entries) == 0) {
    cli::cli_abort(
      "{.file {path}} must include a non-empty {.field {section}} key."
    )
  }

  ids <- names(entries)

  if (is.null(ids) || any(ids == "") || anyDuplicated(ids)) {
    cli::cli_abort(
      "Each entry in {.field {section}} must have a unique name."
    )
  }

  entries
}

#' @noRd
check_required_fields <- function(x, required, id) {
  missing <- setdiff(required, names(x))

  if (length(missing) > 0) {
    cli::cli_abort(
      "{.val {id}} is missing required field{?s} {.field {missing}}."
    )
  }

  invisible(x)
}

#' Check that processing arguments are named and exclude reserved arguments set
#' by the pipeline
#' @noRd
check_process_args <- function(args, id, field) {
  if (is.null(args)) {
    return(invisible(args))
  }

  if (!is.list(args) || is.null(names(args)) || any(names(args) == "")) {
    cli::cli_abort(
      "{.val {id}} {.field {field}} must be a set of named values."
    )
  }

  reserved <- c("clip", "filter_geom", "filter_by", "crs", "url")
  reserved_args <- intersect(names(args), reserved)

  if (length(reserved_args) > 0) {
    cli::cli_abort(
      "{.val {id}} {.field {field}} can't include {.field {reserved_args}}
      as these are set by the pipeline or source."
    )
  }

  invisible(args)
}
