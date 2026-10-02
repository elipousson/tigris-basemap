#' Registry of processing steps
#'
#' Each step is named after the function it calls. `fn` is called with the
#' data as the first argument followed by the step `args`. `args` lists the
#' argument names allowed in basemaps.yml and sources.yml. See
#' docs/style-spec.md for details on each step.
#'
#' @noRd
processing_step_registry <- function() {
  list(
    filter = list(
      fn = filter_expression,
      args = "expression"
    ),
    filter_min_area = list(
      fn = filter_min_area,
      args = c("min_area", "units")
    ),
    ms_simplify = list(
      fn = rmapshaper::ms_simplify,
      args = c(
        "keep",
        "method",
        "weighting",
        "keep_shapes",
        "no_repair",
        "snap",
        "explode",
        "snap_interval"
      )
    ),
    ms_dissolve = list(
      fn = rmapshaper::ms_dissolve,
      args = c("field", "sum_fields", "copy_fields", "weight", "snap")
    ),
    ms_erase = list(
      fn = rmapshaper::ms_erase,
      args = c("erase", "remove_slivers")
    ),
    ms_clip = list(
      fn = rmapshaper::ms_clip,
      args = c("clip", "remove_slivers")
    ),
    ms_innerlines = list(
      fn = rmapshaper::ms_innerlines,
      args = character(0)
    ),
    smooth = list(
      fn = smoothr::smooth,
      args = c(
        "method",
        "refinements",
        "smoothness",
        "bandwidth",
        "n",
        "max_distance",
        "vertex_factor"
      )
    ),
    st_buffer = list(
      fn = sf::st_buffer,
      args = c(
        "dist",
        "nQuadSegs",
        "endCapStyle",
        "joinStyle",
        "mitreLimit",
        "singleSide"
      )
    ),
    st_union = list(
      fn = st_union_sf,
      args = "is_coverage"
    ),
    st_make_valid = list(
      fn = sf::st_make_valid,
      args = c("geos_method", "geos_keep_collapsed")
    ),
    st_cast = list(
      fn = sf::st_cast,
      args = c("to", "group_or_split", "warn")
    )
  )
}

#' Run processing steps on spatial data
#'
#' @param x A `sf` object.
#' @param steps A list of processing steps, each a list with a `name` and
#'   optional `args`.
#' @param data A named list of `sf` objects used to resolve references in step
#'   arguments, e.g. `erase: {source: water}`. Multiple sources in a reference
#'   are combined into a single geometry collection before the reference
#'   `processing_steps` are run.
#' @returns The processed `sf` object.
run_processing_steps <- function(x, steps = NULL, data = list()) {
  registry <- processing_step_registry()

  # Recursive helpers are defined locally because targets treats recursion
  # between global functions as a dependency cycle
  run_steps <- function(x, steps) {
    if (is.null(x) || length(steps) == 0) {
      return(x)
    }

    for (step in steps) {
      # Skip remaining steps once there is no data left to process (e.g. no
      # inner lines for an MSA with only one county besides the focal county)
      if (nrow(x) == 0) {
        return(x)
      }

      args <- lapply(step$args, \(arg) {
        if (is_data_ref(arg)) {
          return(resolve_ref(arg))
        }
        arg
      })

      x <- tryCatch(
        do.call(registry[[step$name]]$fn, c(list(x), args)),
        error = \(cnd) {
          cli::cli_abort(
            "Processing step {.val {step$name}} failed.",
            parent = cnd
          )
        }
      )
    }

    x
  }

  resolve_ref <- function(ref) {
    run_steps(combine_data_ref(ref, data), ref$processing_steps)
  }

  if (is_data_ref(x)) {
    return(resolve_ref(x))
  }

  run_steps(x, steps)
}

#' Is an argument a reference to named data?
#' @noRd
is_data_ref <- function(x) {
  is.list(x) &&
    !is.data.frame(x) &&
    !is.null(names(x)) &&
    "source" %in% names(x)
}

#' Get the data for a reference to one or more named data objects
#' @noRd
combine_data_ref <- function(ref, data) {
  missing <- setdiff(ref$source, names(data))

  if (length(missing) > 0) {
    cli::cli_abort(
      "Reference to unavailable data: {.val {missing}}. Available data:
      {.val {names(data)}}"
    )
  }

  ref_data <- data[ref$source]
  ref_data <- ref_data[!vapply(ref_data, is.null, TRUE)]

  if (length(ref_data) == 0) {
    cli::cli_abort("Referenced data {.val {ref$source}} is empty.")
  }

  if (length(ref_data) == 1) {
    return(ref_data[[1]])
  }

  sf::st_sf(
    geometry = do.call(c, lapply(unname(ref_data), sf::st_geometry))
  )
}

#' Union geometry and return a `sf` object
#' @noRd
st_union_sf <- function(x, ...) {
  sf::st_sf(geometry = sf::st_union(x, ...))
}

#' Filter features with a minimum area
#'
#' Area is measured on the ellipsoid (by transforming to longitude and
#' latitude) because planar area in Web Mercator (EPSG:3857) overstates area
#' by roughly 1 / cos(latitude)^2, e.g. about 1.67 times at 39 degrees north.
#'
#' @param min_area Minimum area in `units`.
#' @param units Area units passed to [units::set_units()].
filter_min_area <- function(x, min_area, units = "acres") {
  area <- units::set_units(
    sf::st_area(sf::st_transform(x, crs = 4326)),
    units,
    mode = "standard"
  )
  x[as.numeric(area) >= min_area, ]
}

#' Filter features with a MapLibre filter expression
#'
#' @param expression A MapLibre expression, e.g. `["==", ["get", "RTTYP"],
#'   "I"]` parsed from YAML.
filter_expression <- function(x, expression) {
  dplyr::filter(x, !!maplibre_expr_to_r(expression))
}

#' Convert a MapLibre filter expression to an R expression
#'
#' Supports `get`, `literal`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `in`, `!`,
#' `all`, and `any`.
#'
#' @param x A MapLibre expression as a list or character vector. Scalars are
#'   treated as literal values.
#' @returns A language object for use with [dplyr::filter()].
maplibre_expr_to_r <- function(x) {
  binary_ops <- c("==", "!=", "<", "<=", ">", ">=")

  # Defined locally because targets treats a recursive global function as a
  # dependency cycle
  convert <- function(x) {
    if (length(x) <= 1 && !is.list(x)) {
      return(x)
    }

    op <- x[[1]]
    args <- as.list(x[-1])

    if (op == "get") {
      return(rlang::expr(.data[[!!args[[1]]]]))
    }

    if (op == "literal") {
      return(unlist(args[[1]]))
    }

    if (op %in% binary_ops) {
      check_expr_args(op, args, n = 2)
      return(rlang::call2(op, convert(args[[1]]), convert(args[[2]])))
    }

    if (op == "in") {
      check_expr_args(op, args, n = 2)
      return(rlang::expr(!!convert(args[[1]]) %in% !!convert(args[[2]])))
    }

    if (op == "!") {
      check_expr_args(op, args, n = 1)
      return(rlang::expr(!(!!convert(args[[1]]))))
    }

    if (op %in% c("all", "any")) {
      joiner <- if (op == "all") "&" else "|"
      return(Reduce(
        \(a, b) rlang::call2(joiner, a, b),
        lapply(args, convert)
      ))
    }

    cli::cli_abort(
      "Unsupported MapLibre expression operator: {.val {op}}"
    )
  }

  convert(x)
}

#' @noRd
check_expr_args <- function(op, args, n) {
  if (length(args) != n) {
    cli::cli_abort(
      "MapLibre expression {.val {op}} requires {n} argument{?s}, not
      {length(args)}."
    )
  }
}
