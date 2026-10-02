#' Run processing steps for each basemap layer and compute the map bounds
#'
#' @param data A named list of `sf` objects. Names correspond to the `source`
#'   of each layer.
#' @param layers A list of layer processing specifications from
#'   [layer_processing_specs()].
#' @param bounds `NULL`, a numeric `[west, south, east, north]` vector in
#'   longitude and latitude, or a reference to `data` (e.g. `list(source =
#'   "county")`) with optional `processing_steps`.
#' @returns A list with `layers` (a named list of processed `sf` objects for
#'   each visible layer with data) and `bounds` (a `bbox` or `NULL`).
process_layers <- function(data, layers, bounds = NULL) {
  layer_data <- list()

  for (layer in layers) {
    if (identical(layer$visibility, "none") || is.null(layer$source)) {
      next
    }

    if (!layer$source %in% names(data)) {
      cli::cli_abort(
        "Layer {.val {layer$id}} uses unavailable data {.val {layer$source}}.
        Available data: {.val {names(data)}}"
      )
    }

    if (is.null(data[[layer$source]])) {
      next
    }

    layer_data[[layer$id]] <- tryCatch(
      run_processing_steps(
        data[[layer$source]],
        steps = layer$processing_steps,
        data = data
      ),
      error = \(cnd) {
        cli::cli_abort(
          "Processing failed for layer {.val {layer$id}}.",
          parent = cnd
        )
      }
    )
  }

  list(
    layers = layer_data,
    bounds = make_basemap_bounds(bounds, data = data)
  )
}

#' Make a bounding box from a bounds specification
#' @noRd
make_basemap_bounds <- function(bounds, data, crs = NULL) {
  if (is.null(bounds)) {
    return(NULL)
  }

  if (is_data_ref(bounds)) {
    return(sf::st_bbox(run_processing_steps(bounds, data = data)))
  }

  crs <- crs %||% sf::st_crs(data[[1]])

  bounds <- rlang::set_names(
    as.numeric(bounds),
    c("xmin", "ymin", "xmax", "ymax")
  )

  sf::st_bbox(bounds, crs = sf::st_crs(4326)) |>
    sf::st_as_sfc() |>
    sf::st_transform(crs = crs) |>
    sf::st_bbox()
}

#' Plot a basemap from processed layer data and style specifications
#'
#' @param layer_data A list from [process_layers()].
#' @param layers A list of layer style specifications from
#'   [layer_paint_specs()] in draw order.
#' @param view A list with `neatline` and `theme` from [basemap_view()].
plot_basemap <- function(layer_data, layers, view = list()) {
  plot <- ggplot2::ggplot()
  background <- list()

  for (layer in layers) {
    if (identical(layer$layout$visibility, "none")) {
      next
    }

    if (layer$type == "background") {
      background <- background_theme(layer$paint)
      next
    }

    data <- layer_data$layers[[layer$id]]

    if (is.null(data) || nrow(data) == 0) {
      next
    }

    args <- paint_to_geom_args(layer$type, layer$paint, layer$layout)
    plot <- plot + do.call(ggplot2::geom_sf, c(list(data = data), args))
  }

  bounds <- layer_data$bounds

  if (!is.null(bounds)) {
    lims <- bbox_lims(bounds)

    plot <- plot +
      ggplot2::coord_sf(
        xlim = lims$xlim,
        ylim = lims$ylim,
        expand = FALSE,
        label_axes = "----",
        default = TRUE
      )
  }

  theme_fn <- getExportedValue(
    "ggplot2",
    paste0("theme_", view$theme %||% "void")
  )

  plot <- plot + theme_fn()

  if (isTRUE(view$neatline) && !is.null(bounds)) {
    plot <- plot +
      maplayer::layer_neatline(
        data = sf::st_as_sf(sf::st_as_sfc(bounds)),
        expand = FALSE,
        default_plot_margin = ggplot2::margin(0, 0, 0, 0)
      )
  }

  # Added last so the neatline theme does not replace the background
  plot + background
}

#' Convert MapLibre paint and layout properties to `ggplot2::geom_sf()`
#' arguments
#'
#' Line widths and circle radii are converted from pixels to ggplot2 units
#' with [ggplot2::.pt]. Opacity is applied directly to colors so it applies to
#' polygon outlines as well as fills. See docs/style-spec.md for the mapping.
#'
#' @param type Layer type: "fill", "line", or "circle".
#' @param paint A named list of MapLibre paint properties.
#' @param layout A named list of MapLibre layout properties.
paint_to_geom_args <- function(type, paint = list(), layout = list()) {
  paint <- paint %||% list()
  layout <- layout %||% list()

  args <- switch(
    type,
    fill = list(
      fill = ggplot2::alpha(
        paint[["fill-color"]] %||% "#000000",
        paint[["fill-opacity"]] %||% 1
      ),
      colour = paint[["fill-outline-color"]] %||% NA,
      linewidth = 1 / ggplot2::.pt
    ),
    line = list(
      fill = NA,
      colour = ggplot2::alpha(
        paint[["line-color"]] %||% "#000000",
        paint[["line-opacity"]] %||% 1
      ),
      linewidth = (paint[["line-width"]] %||% 1) / ggplot2::.pt,
      linetype = dasharray_to_linetype(paint[["line-dasharray"]])
    ),
    circle = list(
      shape = 21,
      fill = ggplot2::alpha(
        paint[["circle-color"]] %||% "#000000",
        paint[["circle-opacity"]] %||% 1
      ),
      colour = ggplot2::alpha(
        paint[["circle-stroke-color"]] %||% "#000000",
        paint[["circle-stroke-opacity"]] %||% 1
      ),
      size = 2 * (paint[["circle-radius"]] %||% 5) / ggplot2::.pt,
      stroke = (paint[["circle-stroke-width"]] %||% 0) / ggplot2::.pt
    ),
    cli::cli_abort("Unsupported layer type: {.val {type}}")
  )

  if (!is.null(layout[["line-cap"]])) {
    args$lineend <- layout[["line-cap"]]
  }

  if (!is.null(layout[["line-join"]])) {
    args$linejoin <- layout[["line-join"]]
  }

  args
}

#' Convert a MapLibre line-dasharray to a ggplot2 linetype
#'
#' @param x `NULL` or an even-length vector of integers from 1 to 15, e.g.
#'   `c(1, 3, 4, 3)` for "1343" (equivalent to "dotdash").
#' @noRd
dasharray_to_linetype <- function(x) {
  if (is.null(x)) {
    return("solid")
  }

  paste(sprintf("%X", as.integer(x)), collapse = "")
}

#' Create a theme for a MapLibre background layer
#' @noRd
background_theme <- function(paint = list()) {
  fill <- ggplot2::alpha(
    paint[["background-color"]] %||% "#000000",
    paint[["background-opacity"]] %||% 1
  )

  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = fill, colour = NA),
    plot.background = ggplot2::element_rect(fill = fill, colour = NA)
  )
}
