#' Names of data provided by the pipeline (not loaded from a source) that can
#' be used as the `source` of a basemap layer or reference
#' @noRd
pipeline_data_names <- function() {
  c("county", "counties", "msa", "buffer", "divisions")
}

#' Names of pipeline data that can be referenced in data processing steps
#'
#' Which names are available depends on the target loading the data, e.g. MSA
#' data targets are shared across geographies so can't reference `county`.
#' See [data_processing_steps()].
#' @noRd
data_ref_names <- function() {
  c("county", "msa", "msa_counties", "msa_hull")
}

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

    check_named_list(src$args, id = id, field = "args")
    check_reserved_args(src$args, id = id)
    check_processing_steps(src$processing_steps, id = id)
  }

  sources
}

#' Read and validate basemap specifications from a YAML file
#'
#' @param path Path to a YAML file with a top-level `basemaps` key.
#' @param sources A named list from [read_sources()].
#' @returns A named list of basemap specifications.
read_basemaps <- function(path = "basemaps.yml", sources = read_sources()) {
  basemaps <- read_config_section(path, "basemaps")

  for (basemap in names(basemaps)) {
    spec <- basemaps[[basemap]]

    check_required_fields(spec, c("data", "layers"), id = basemap)

    data_names <- c(names(spec$data), names(spec$staging))

    if (anyDuplicated(data_names)) {
      cli::cli_abort(
        "Basemap {.val {basemap}} data names must be unique across
        {.field data} and {.field staging}:
        {.val {data_names[duplicated(data_names)]}}"
      )
    }

    reserved_names <- intersect(data_names, pipeline_data_names())

    if (length(reserved_names) > 0) {
      cli::cli_abort(
        "Basemap {.val {basemap}} data names can't use names provided by the
        pipeline: {.val {reserved_names}}"
      )
    }

    for (section in c("data", "staging")) {
      for (name in names(spec[[section]])) {
        check_data_entry(
          spec[[section]][[name]],
          sources = sources,
          id = paste0(basemap, ".", section, ".", name)
        )
      }
    }

    available <- c(names(spec$data), pipeline_data_names())

    check_view(spec$view, available = available, id = basemap)
    check_layers(spec$layers, available = available, id = basemap)
  }

  basemaps
}

#' @noRd
check_data_entry <- function(entry, sources, id) {
  check_required_fields(entry, "source", id = id)

  if (is.null(sources[[entry$source]])) {
    cli::cli_abort(
      "{.val {id}} has a {.field source} not found in sources:
      {.val {entry$source}}"
    )
  }

  check_named_list(entry$args, id = id, field = "args")
  check_reserved_args(entry$args, id = id)
  check_processing_steps(
    entry$processing_steps,
    id = id,
    available = data_ref_names()
  )
}

#' @noRd
check_view <- function(view, available, id) {
  if (is.null(view)) {
    return(invisible(view))
  }

  bounds <- view$bounds

  if (is_data_ref(bounds)) {
    check_processing_steps(
      bounds,
      available = available,
      id = paste0(id, ".view.bounds")
    )
  } else if (!is.null(bounds) && !(is.numeric(bounds) && length(bounds) == 4)) {
    cli::cli_abort(
      "{.val {id}} {.field view.bounds} must be a reference to data or a
      [west, south, east, north] array."
    )
  }

  themes <- c("void", "minimal", "bw", "classic", "light")

  if (!is.null(view$theme) && !view$theme %in% themes) {
    cli::cli_abort(
      "{.val {id}} {.field view.theme} must be one of {.or {.val {themes}}}."
    )
  }

  invisible(view)
}

#' Allowed paint and layout properties for each layer type
#' @noRd
style_properties <- function() {
  list(
    fill = list(
      paint = c("fill-color", "fill-opacity", "fill-outline-color"),
      layout = "visibility"
    ),
    line = list(
      paint = c("line-color", "line-width", "line-opacity", "line-dasharray"),
      layout = c("visibility", "line-cap", "line-join")
    ),
    circle = list(
      paint = c(
        "circle-color",
        "circle-radius",
        "circle-opacity",
        "circle-stroke-color",
        "circle-stroke-width",
        "circle-stroke-opacity"
      ),
      layout = "visibility"
    ),
    background = list(
      paint = c("background-color", "background-opacity"),
      layout = "visibility"
    )
  )
}

#' @noRd
check_layers <- function(layers, available, id) {
  if (!is.list(layers) || length(layers) == 0 || !is.null(names(layers))) {
    cli::cli_abort("{.val {id}} {.field layers} must be a non-empty list.")
  }

  ids <- vapply(layers, \(x) x$id %||% NA_character_, "")

  if (anyNA(ids) || anyDuplicated(ids)) {
    cli::cli_abort(
      "Each layer in {.val {id}} must have a unique {.field id}."
    )
  }

  properties <- style_properties()

  for (layer in layers) {
    layer_id <- paste0(id, ".", layer$id)

    layer_keys <- c(
      "id",
      "type",
      "source",
      "description",
      "processing_steps",
      "layout",
      "paint"
    )
    unknown_keys <- setdiff(names(layer), layer_keys)

    if (length(unknown_keys) > 0) {
      cli::cli_abort(
        "Layer {.val {layer_id}} has unsupported key{?s}
        {.field {unknown_keys}}."
      )
    }

    if (!rlang::is_string(layer$type) || !layer$type %in% names(properties)) {
      cli::cli_abort(
        "Layer {.val {layer_id}} {.field type} must be one of
        {.or {.val {names(properties)}}}."
      )
    }

    if (layer$type != "background") {
      if (!rlang::is_string(layer$source) || !layer$source %in% available) {
        cli::cli_abort(
          "Layer {.val {layer_id}} {.field source} must be one of
          {.or {.val {available}}}."
        )
      }
    }

    for (field in c("paint", "layout")) {
      check_named_list(layer[[field]], id = layer_id, field = field)

      unknown <- setdiff(
        names(layer[[field]]),
        properties[[layer$type]][[field]]
      )

      if (length(unknown) > 0) {
        cli::cli_abort(
          "Layer {.val {layer_id}} has unsupported {field} propert{?y/ies}
          for a {.val {layer$type}} layer: {.field {unknown}}"
        )
      }
    }

    check_paint_values(layer$paint, id = layer_id)

    visibility <- layer$layout$visibility

    if (!is.null(visibility) && !visibility %in% c("visible", "none")) {
      cli::cli_abort(
        "Layer {.val {layer_id}} {.field layout.visibility} must be
        {.val visible} or {.val none}."
      )
    }

    check_processing_steps(
      layer$processing_steps,
      id = layer_id,
      available = available
    )
  }

  invisible(layers)
}

#' @noRd
check_paint_values <- function(paint, id) {
  for (property in names(paint)) {
    value <- paint[[property]]

    if (grepl("-color$", property)) {
      valid <- tryCatch(
        {
          grDevices::col2rgb(value)
          TRUE
        },
        error = \(cnd) FALSE
      )

      if (!valid) {
        cli::cli_abort(
          "Layer {.val {id}} {.field {property}} is not a valid color:
          {.val {value}}"
        )
      }
    } else if (property == "line-dasharray") {
      if (
        !is.numeric(value) ||
          length(value) %% 2 != 0 ||
          any(value < 1 | value > 15 | value != round(value))
      ) {
        cli::cli_abort(
          "Layer {.val {id}} {.field line-dasharray} must be an even-length
          array of integers from 1 to 15."
        )
      }
    } else if (!is.numeric(value) || length(value) != 1) {
      cli::cli_abort(
        "Layer {.val {id}} {.field {property}} must be a single number."
      )
    }
  }

  invisible(paint)
}

#' Check processing steps against the step registry
#'
#' @param available Names of data that can be referenced in step arguments. If
#'   `NULL`, references are not allowed.
#' @noRd
check_processing_steps <- function(steps, id, available = NULL) {
  registry <- processing_step_registry()

  # Recursive helpers are defined locally because targets treats recursion
  # between global functions as a dependency cycle
  check_steps <- function(steps) {
    if (is.null(steps)) {
      return(invisible(steps))
    }

    if (!is.list(steps) || !is.null(names(steps))) {
      cli::cli_abort(
        "{.val {id}} {.field processing_steps} must be a list of steps."
      )
    }

    for (step in steps) {
      check_step(step)
    }

    invisible(steps)
  }

  check_step <- function(step) {
    if (!rlang::is_string(step$name) || !step$name %in% names(registry)) {
      cli::cli_abort(
        "{.val {id}} has an unknown processing step {.val {step$name}}.
        Supported steps: {.val {names(registry)}}"
      )
    }

    unknown_fields <- setdiff(names(step), c("name", "args"))

    if (length(unknown_fields) > 0) {
      cli::cli_abort(
        "{.val {id}} processing step {.val {step$name}} has unsupported
        field{?s} {.field {unknown_fields}}."
      )
    }

    check_named_list(step$args, id = id, field = paste0(step$name, ".args"))

    unknown_args <- setdiff(names(step$args), registry[[step$name]]$args)

    if (length(unknown_args) > 0) {
      cli::cli_abort(
        "{.val {id}} processing step {.val {step$name}} has unsupported
        argument{?s} {.field {unknown_args}}. Supported arguments:
        {.field {registry[[step$name]]$args}}"
      )
    }

    if (step$name == "filter") {
      tryCatch(
        maplibre_expr_to_r(step$args$expression),
        error = \(cnd) {
          cli::cli_abort(
            "{.val {id}} has an invalid filter expression.",
            parent = cnd
          )
        }
      )
    }

    for (arg in step$args) {
      if (!is_data_ref(arg)) {
        next
      }

      if (is.null(available)) {
        cli::cli_abort(
          "{.val {id}} processing step {.val {step$name}} can't reference
          other data here."
        )
      }

      check_ref(arg)
    }
  }

  check_ref <- function(ref) {
    missing <- setdiff(ref$source, available)

    if (length(missing) > 0) {
      cli::cli_abort(
        "{.val {id}} references unavailable data {.val {missing}}. Available
        data: {.val {available}}"
      )
    }

    check_steps(ref$processing_steps)
  }

  if (is_data_ref(steps)) {
    return(invisible(check_ref(steps)))
  }

  check_steps(steps)
}

#' Get loader arguments for a source
#'
#' @param sources A named list from [read_sources()].
#' @param source Source identifier.
#' @returns A named list of the source `args` and, for ArcGIS sources, the
#'   source `url`.
source_args <- function(sources, source) {
  src <- get_config_entry(sources, source, "source")

  args <- src$args %||% list()

  if (src$type == "arcgis") {
    args <- c(list(url = src$url), args)
  }

  args
}

#' Get the default processing steps for a source
source_processing_steps <- function(sources, source) {
  get_config_entry(sources, source, "source")$processing_steps
}

#' Get loader arguments for basemap data
#'
#' The data entry `args` override the source `args` with the same name. Use
#' with `!!!` in a target command so the arguments are inserted into the
#' command.
#'
#' @param basemaps A named list from [read_basemaps()].
#' @param sources A named list from [read_sources()].
#' @param basemap Basemap identifier, e.g. "county" or "msa".
#' @param name Name of the data in the basemap `data` or `staging`.
data_source_args <- function(basemaps, sources, basemap, name) {
  entry <- get_data_entry(basemaps, basemap, name)

  utils::modifyList(
    source_args(sources, entry$source),
    entry$args %||% list()
  )
}

#' Get processing steps for basemap data
#'
#' The source processing steps (e.g. `st_make_valid`) always run first,
#' followed by the processing steps for the data entry.
#'
#' @inheritParams data_source_args
#' @param available Names of pipeline data the target passes to
#'   [run_processing_steps()] that the steps can reference, e.g. `"county"`.
#'   An error is raised when `_targets.R` is sourced if the steps reference
#'   any other data.
data_processing_steps <- function(
  basemaps,
  sources,
  basemap,
  name,
  available = character(0)
) {
  entry <- get_data_entry(basemaps, basemap, name)

  steps <- c(
    source_processing_steps(sources, entry$source),
    entry$processing_steps
  )

  check_processing_steps(
    steps,
    id = paste0(basemap, ".data.", name),
    available = if (length(available) > 0) available
  )

  steps
}

#' Get layer processing specifications for a basemap
#'
#' Returns only the parts of each layer used to process data so changes to
#' paint or layout properties do not invalidate processed layer data.
#'
#' @inheritParams data_source_args
layer_processing_specs <- function(basemaps, basemap) {
  layers <- get_config_entry(basemaps, basemap, "basemap")$layers

  lapply(layers, \(layer) {
    list(
      id = layer$id,
      source = layer$source,
      visibility = layer$layout$visibility %||% "visible",
      processing_steps = layer$processing_steps
    )
  })
}

#' Get layer style specifications for a basemap
#'
#' @inheritParams data_source_args
layer_paint_specs <- function(basemaps, basemap) {
  layers <- get_config_entry(basemaps, basemap, "basemap")$layers

  lapply(layers, \(layer) {
    list(
      id = layer$id,
      type = layer$type,
      paint = layer$paint %||% list(),
      layout = layer$layout %||% list()
    )
  })
}

#' Get the bounds specification for a basemap
#'
#' @inheritParams data_source_args
basemap_bounds <- function(basemaps, basemap) {
  get_config_entry(basemaps, basemap, "basemap")$view$bounds
}

#' Get view options (except bounds) for a basemap
#'
#' @inheritParams data_source_args
basemap_view <- function(basemaps, basemap) {
  view <- get_config_entry(basemaps, basemap, "basemap")$view %||% list()
  view[setdiff(names(view), "bounds")]
}

#' @noRd
get_config_entry <- function(config, name, type) {
  entry <- config[[name]]

  if (is.null(entry)) {
    cli::cli_abort("{.str {type}} {.val {name}} not found.")
  }

  entry
}

#' @noRd
get_data_entry <- function(basemaps, basemap, name) {
  spec <- get_config_entry(basemaps, basemap, "basemap")
  entry <- spec$data[[name]] %||% spec$staging[[name]]

  if (is.null(entry)) {
    cli::cli_abort(
      "Data {.val {name}} not found in basemap {.val {basemap}}."
    )
  }

  entry
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

#' @noRd
check_named_list <- function(x, id, field) {
  if (is.null(x)) {
    return(invisible(x))
  }

  if (!is.list(x) || is.null(names(x)) || any(names(x) == "")) {
    cli::cli_abort(
      "{.val {id}} {.field {field}} must be a set of named values."
    )
  }

  invisible(x)
}

#' Check that loader arguments exclude arguments set by the pipeline
#' @noRd
check_reserved_args <- function(args, id) {
  reserved <- c("clip", "filter_geom", "filter_by", "crs", "url", "year")
  reserved_args <- intersect(names(args), reserved)

  if (length(reserved_args) > 0) {
    cli::cli_abort(
      "{.val {id}} {.field args} can't include {.field {reserved_args}}
      as these are set by the pipeline or source."
    )
  }

  invisible(args)
}
