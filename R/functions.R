#' @import rlang
#' @import dplyr
NULL

#' Execute a call to [ggplot2::geom_sf] splicing in a list of named parameters
#'
#' @noRd
#' @importFrom ggplot2 geom_sf
exec_geom_sf_params <- function(..., params = list2(...)) {
  exec(.fn = ggplot2::geom_sf, ..., !!!params)
}

#' Use [rmapshaper::ms_clip] to clip a target spatial data object
#'
#' @importFrom rmapshaper ms_clip
#' @importFrom sf st_crs st_transform
ms_clip_ext <- function(target, clip = NULL, allow_null = TRUE, remove_slivers = TRUE, ...) {
  if (allow_null && is.null(clip)) {
    return(target)
  }

  clip <- sf::st_transform(clip, crs = sf::st_crs(target))

  if (!inherits(clip, "sf")) {
    clip <- sf::st_as_sf(clip)
  }

  rmapshaper::ms_clip(target = target, clip = clip, remove_slivers = remove_slivers, ...)
}


#' Load a ArcGIS FeatureLayer then make valid and clip
load_arc_url <- function(url, crs = 3857,
                         filter_geom = NULL,
                         clip = NULL,
                         promote_to_multi = TRUE,
                         remove_slivers = TRUE) {
  if (is.null(url)) {
    cli::cli_alert_warning(
      "{.arg url} missing"
    )

    return(NULL)
  }

  data <- arcgislayers::arc_read(
    url = url,
    crs = crs,
    filter_geom = filter_geom
  )

  poly_geom <- sf::st_is(data, "POLYGON")

  if (any(poly_geom)) {
    data <- sf::st_make_valid(data)

    data[poly_geom, ] <- sf::st_cast(
      data[poly_geom, ],
      to = "MULTIPOLYGON"
    )
  }

  # data <- sf::st_make_valid(data)

  ms_clip_ext(
    target = data,
    clip = clip,
    remove_slivers = remove_slivers
  )
}

filter_clip <- function(target,
                        clip,
                        remove_slivers = TRUE,
                        threshold = 0.1,
                        keep = TRUE) {
  clipped_target <- target |>
    dplyr::mutate(
      geometry_area = sf::st_area(.data[[attr(target, "sf_column")]])
    ) |>
    rmapshaper::ms_clip(clip = clip, remove_slivers = remove_slivers) |>
    dplyr::mutate(
      clipped_area = sf::st_area(.data[[attr(target, "sf_column")]]),
      pct_geometry_in_clip = as.numeric(clipped_area / geometry_area)
    ) |>
    dplyr::filter(
      pct_geometry_in_clip > threshold
    )

  if (keep) {
    return(clipped_target)
  }

  clipped_target |>
    dplyr::select(
      !c(geometry_area, clipped_area, pct_geometry_in_clip)
    )
}

#' Use multiple `{rmapshaper}` and `{smoothr}` functions to format spatial data
#' for use in a basemap
#' FIXME: Add missing imports
format_basemap_data <- function(data,
                                dissolve = FALSE,
                                simplify = FALSE,
                                smooth = FALSE,
                                ...,
                                keep = 0.02,
                                method = "chaikin",
                                refinements = 1,
                                clip = NULL,
                                crs = 3857) {
  if (!is.null(crs)) {
    data <- data |>
      sf::st_transform(crs = sf::st_crs(crs))
  }

  if (dissolve) {
    data <- data |>
      rmapshaper::ms_dissolve()
  }

  if (simplify) {
    data <- data |>
      rmapshaper::ms_simplify(keep = keep, ...)
  }

  if (smooth) {
    data <- data |>
      smoothr::smooth(method = method, refinements = refinements)
  }

  ms_clip_ext(data, clip)
}

export_rds <- function(obj,
                       filename) {
  readr::write_rds(
    obj,
    fs::path(
      "output", filename
    )
  )
}

#' Filter a data frame of counties using the internal `validate_county`
#' `{tigris}` function
#'
#' @importFrom rmapshaper ms_clip
#' @importFrom sf st_crs st_transform
filter_counties <- function(counties,
                            state = NULL,
                            county = NULL,
                            multiple = TRUE,
                            validate = TRUE) {
  if (is.null(county)) {
    return(counties)
  }

  stopifnot(!is.null(state))

  if (validate) {
    county <- tigris:::validate_county(state, county, .msg = FALSE, multiple = multiple)
  }
  dplyr::filter(counties, .data[["COUNTYFP"]] %in% county)
}

#' Load USGS PAD data with [arcgislayers::arc_read]
#'
#' @inheritParams arcgislayers::arc_read
#' @inheritParams format_basemap_data
#' @importFrom rmapshaper ms_clip
#' @importFrom sf st_crs st_transform
load_usgs_pad <- function(filter_geom = NULL,
                          ...,
                          n_max = Inf,
                          clip = filter_geom,
                          keep = 0.025,
                          min_area = 20,
                          area_units = "acres",
                          crs = 3857) {
  data <- arcgislayers::arc_read(
    # FIXME: Add support for multiple PAD layers or swap this function for a dedicated PAD data package
    url = "https://services.arcgis.com/v01gqwM5QqNysAAi/ArcGIS/rest/services/Manager_Name/FeatureServer/0",
    n_max = n_max,
    filter_geom = filter_geom,
    crs = crs
  )

  # Filter by minimum area
  if (is.numeric(min_area)) {
    data <- data |>
      dplyr::mutate(
        area = units::set_units(
          sf::st_area(geometry),
          area_units,
          mode = "standard"
        )
      ) |>
      dplyr::filter(
        as.numeric(area) >= min_area
      )
  }

  format_basemap_data(data, keep = keep, clip = clip, crs = NULL, ...)
}

#' Load a county or counties using `tigris::counties`
#' @importFrom tigris counties
#' @importFrom package function
load_county <- function(state,
                        counties = NULL,
                        county = NULL,
                        ...,
                        validate = TRUE,
                        multiple = TRUE,
                        simplify = TRUE,
                        keep = 0.075,
                        crs = 3857) {
  counties <- counties %||% tigris::counties(state = state, ...)

  county <- filter_counties(counties, state = state, county = county, validate = validate)

  format_basemap_data(county, simplify = simplify, keep = keep, smooth = FALSE, dissolve = FALSE, crs = crs)
}

#' Load
#' @importFrom tigris primary_secondary_roads
#' @importFrom dplyr filter
load_primary_secondary_roads <- function(state,
                                         year = NULL,
                                         filter_by = NULL,
                                         ...,
                                         dissolve = FALSE,
                                         simplify = FALSE,
                                         smooth = FALSE,
                                         clip = NULL,
                                         road_type = c("I", "U"),
                                         crs = 3857) {
  roads <- tigris::primary_secondary_roads(
    state = state,
    year = year,
    filter_by = filter_by
  )

  if (!is.null(road_type)) {
    roads <- roads |>
      dplyr::filter(RTTYP %in% road_type)
  }

  format_basemap_data(roads, clip = clip, crs = crs, dissolve = dissolve, simplify = simplify, smooth = smooth, ...)
}

layer_primary_secondary_roads <- function(
    roads,
    type = "I",
    bg_params = list(
      linewidth = 0.28,
      color = "gray25",
      alpha = 0.7
    ),
    fg_params = list(
      linewidth = 0.3,
      color = "gray20",
      alpha = 0.7
    )) {
  list(
    exec_geom_sf_params(
      data = dplyr::filter(roads, RTTYP != type),
      params = bg_params
    ),
    exec_geom_sf_params(
      data = dplyr::filter(roads, RTTYP == type),
      params = fg_params
    )
  )
}

#' Load USGS National Hydrographical (sp?) Data (NHD)
load_usgs_nhd <- function(
    # FIXME: Add support for multiple URLs from NHD data
    url = "https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer/12",
    # url = "https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer/10",
    filter_geom = NULL,
    where = NULL,
    ...,
    keep = 0.06,
    method = "chaikin",
    refinements = 1,
    clip = NULL,
    crs = 3857) {
  nhd <- arcgislayers::arc_read(
    url = url,
    filter_geom = filter_geom,
    where = where,
    crs = crs
  )

  # FIXME: Standardize what parameters are or are not being passed to
  # `format_basemap_data`
  format_basemap_data(
    nhd,
    keep = keep,
    method = method,
    refinements = refinements,
    clip = clip,
    crs = NULL,
    ...
  )
}

#' Load water for a county or counties in a single state using `tigris::area_water()`
load_area_water <- function(state,
                            county,
                            year = NULL, ...,
                            keep = 0.004,
                            method = "chaikin",
                            refinements = 1,
                            clip = NULL,
                            crs = 3857) {
  if (length(county) > 1) {
    water <- lapply(
      county,
      \(x) {
        tigris::area_water(state = state, county = x, year = year)
      }
    )

    water <- dplyr::bind_rows(water)
  } else {
    water <- tigris::area_water(state = state, county = county, year = year)
  }

  format_basemap_data(
    water,
    keep = keep,
    method = method,
    refinements = refinements,
    clip = clip,
    smooth = FALSE,
    crs = crs,
    ...
  )
}

#' Create a set of circular buffer areas around a county centroid
make_county_buffers <- function(county,
                                dist = c(10, 20, 30, 40, 50),
                                unit = "mi") {
  # FIXME: dist could or should be set based on the county geometry
  # TODO: Move [sfext::sf_bbox_diagdist()] into a standalone script and then
  # import into this repository
  cent <- suppressWarnings(sf::st_centroid(county))

  buffer_list <- lapply(
    dist,
    \(x){
      buffer_dist <- units::as_units(x, unit)
      buffer_dist <- units::set_units(buffer_dist, value = "m")
      dplyr::bind_cols(
        dplyr::tibble(dist = x),
        sf::st_as_sf(sf::st_buffer(cent, dist = buffer_dist))
      )
    }
  )

  sf::st_as_sf(purrr::list_rbind(buffer_list))
}

#' Create Baltimore basemap
#'
plot_msa_basemap <- function(
    counties,
    urban_area,
    water,
    roads,
    parks,
    bg,
    rail_lines = NULL,
    county = NULL,
    line_dist = 100,
    crs = 3857) {
  if (is.character(county)) {
    county <- filter_counties(
      counties,
      state = unique(counties[["STUSPS"]]),
      county = county
    )
  }

  all_counties <- counties

  counties <- counties |>
    dplyr::filter(
      GEOID != county[["GEOID"]]
    )

  county_lines <- counties |>
    sf::st_combine() |>
    rmapshaper::ms_innerlines() |>
    rmapshaper::ms_erase(county, remove_slivers = TRUE) |>
    rmapshaper::ms_erase(
      sf::st_buffer(water, dist = line_dist * 3),
      remove_slivers = TRUE
    ) #|>
  # sfext::st_buffer_ext(dist = line_dist)

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = bg,
      linewidth = 0.2,
      # color = "black",
      # fill = NA,
      color = "gray45",
      # fill = "#FCFBF9",
      fill = "gray98"
    ) +
    ggplot2::geom_sf(
      data = all_counties,
      linewidth = 2,
      color = "white",
      alpha = 0.8,
      fill = NA
    ) +
    geom_sf(
      data = all_counties,
      fill = "white",
      color = "black",
      linewidth = 0.2
    ) +
    geom_sf(
      data = rmapshaper::ms_erase(
        urban_area,
        water,
        remove_slivers = TRUE
      ),
      fill = "gray80",
      color = NA,
      alpha = 0.1
    ) +
    list(
      ggplot2::geom_sf(
        # data = county_lines,
        data = rmapshaper::ms_erase(
          sf::st_union(
            county_lines
          ) |>
            sf::st_as_sf(),
          sf::st_union(
            sf::st_buffer(water, dist = 2000)
          ) |>
            sf::st_as_sf(),
          remove_slivers = TRUE
        ),
        linewidth = 0.15,
        color = "gray45",
        fill = NA
      ),
      layer_area_water(water),
      layer_usgs_pad(parks),
      ggplot2::geom_sf(
        data = rail_lines,
        linewidth = 0.28,
        linetype = "dotdash",
        color = "gray25",
        alpha = 0.7,
      ),
      layer_primary_secondary_roads(roads),
      ggplot2::geom_sf(
        data = county,
        linewidth = 0.4,
        color = "gray40",
        fill = NA
      )
    ) +
    ggplot2::theme_void()
}

#' Load an urban area using a county or counties as a spatial filter
load_urban_area <- function(county,
                            ...,
                            clip = NULL,
                            crs = 3857) {
  urban_area <- tigris::urban_areas(filter_by = county, ...)

  urban_area <- sf::st_transform(urban_area, crs = crs)

  ms_clip_ext(
    target = urban_area,
    clip = clip
  )
}


#' Create a ggplot2 layer with `ggplot2::geom_sf` using area water aesthetics
layer_area_water <- function(water,
                             dist = NULL,
                             color = NA,
                             fill = "#66B7C9",
                             alpha = 1,
                             ...) {
  if (!is.null(dist)) {
    water <- sf::st_buffer(water, dist = dist, ...)
  }

  ggplot2::geom_sf(
    data = water,
    color = color,
    fill = fill,
    alpha = alpha
  )
}

#' Create a ggplot2 layer with `ggplot2::geom_sf` using protected area/park aesthetics
layer_usgs_pad <- function(data,
                           fill = "#b9d3ba",
                           color = NA,
                           alpha = 0.6,
                           ...) {
  ggplot2::geom_sf(
    data = data,
    fill = fill,
    color = color,
    alpha = alpha
  )
}


#' Plot a county basemap with water, roads, and parks
plot_county_basemap <- function(
    water,
    roads,
    parks,
    county = NULL,
    divisions = NULL,
    theme = ggplot2::theme_void(),
    crs = 3857) {
  divisions_layer <- list()

  if (!is.null(divisions)) {
    divisions_simple <- divisions |>
      rmapshaper::ms_simplify(keep = 0.25, keep_shapes = TRUE) # |>
    # rmapshaper::ms_innerlines()

    divisions_layer <- ggplot2::geom_sf(
      data = divisions_simple,
      fill = NA,
      color = "gray70",
      linewidth = 0.22
    )
  }

  county_layer <- list()

  if (!is.null(county)) {
    county_layer <- ggplot2::geom_sf(
      data = county,
      fill = NA,
      color = "gray10",
      linewidth = 0.4
    )
  }

  ggplot2::ggplot() +
    divisions_layer +
    layer_area_water(water) +
    layer_usgs_pad(parks) +
    layer_primary_secondary_roads(roads) +
    county_layer +
    theme
}


join_division_tracts <- function(
    divisions,
    crs = 3857,
    state_fips = NULL,
    county_fips = NULL,
    largest = TRUE,
    make_divisions_valid = TRUE,
    geos_method = "valid_linework",
    division_type = NULL,
    keep = 0.3,
    division_col = "name",
    make_inner_lines = TRUE) {
  if (make_divisions_valid) {
    divisions_geom <- divisions |>
      sf::st_geometry() |>
      sf::st_make_valid(geos_method = geos_method)

    sf::st_geometry(divisions) <- divisions_geom
  }

  if (!inherits(divisions, "sf")) {
    divisions <- sf::st_as_sf(divisions)
  }

  division_type <- division_type %||%
    tigris::tracts(state = state_fips, county = county_fips) |>
    sf::st_transform(crs = crs)

  divisions <- join_county_tracts(
    divisions,
    county_tracts = division_type,
    col = division_col,
    keep = keep
  )

  if (!make_inner_lines) {
    return(divisions)
  }

  divisions |>
    rmapshaper::ms_innerlines()
}

#' Join data to tracts
join_county_tracts <- function(data,
                               county_tracts,
                               largest = TRUE,
                               col = "name",
                               keep = 0.065) {
  county_tracts |>
    sf::st_join(
      data,
      largest = largest
    ) |>
    summarise(
      geometry = sf::st_union(sf::st_combine(geometry)),
      .by = dplyr::all_of(col)
    ) |>
    sf::st_make_valid() |>
    rmapshaper::ms_simplify(keep = keep)
}
