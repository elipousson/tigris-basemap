#' @import rlang
#' @import dplyr
NULL

bbox_lims <- function(x) {
  if (inherits(x, "bbox")) {
    bbox <- x
  } else {
    bbox <- sf::st_bbox(x)
  }

  list(
    xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
    ylim = c(bbox[["ymin"]], bbox[["ymax"]])
  )
}

#' Load an ArcGIS FeatureLayer
#'
#' Geometry repair and clipping are handled by processing steps (e.g.
#' `st_make_valid` and `ms_clip`) in sources.yml, basemaps.yml, or
#' geographies.yml.
#'
#' @param where Optional SQL where clause passed to [arcgislayers::arc_read()]
#'   to filter features on the server, e.g. `"GIS_Acres >= 25"`.
load_arc_url <- function(
  url,
  filter_geom = NULL,
  where = NULL,
  crs = 3857
) {
  if (is.null(url)) {
    cli::cli_alert_warning(
      "{.arg url} missing"
    )

    return(NULL)
  }

  arcgislayers::arc_read(
    url = url,
    filter_geom = filter_geom,
    where = where,
    crs = crs
  )
}

export_rds <- function(obj, filename) {
  if (!fs::dir_exists("output")) {
    fs::dir_create("output")
  }

  readr::write_rds(
    obj,
    fs::path(
      "output",
      filename
    )
  )
}

#' Filter a data frame of counties using the internal `validate_county`
#' `{tigris}` function
#'
#' @importFrom rmapshaper ms_clip
#' @importFrom sf st_crs st_transform
filter_counties <- function(
  counties,
  state = NULL,
  county = NULL,
  multiple = TRUE,
  validate = TRUE
) {
  if (is.null(county)) {
    return(counties)
  }

  stopifnot(!is.null(state))

  if (validate) {
    county <- validate_county(
      state,
      county,
      .msg = FALSE,
      multiple = multiple
    )
  }
  dplyr::filter(counties, .data[["COUNTYFP"]] %in% county)
}

#' Load a county or counties using `tigris::counties`
#' @importFrom tigris counties
load_county <- function(
  state,
  counties = NULL,
  county = NULL,
  year = NULL,
  validate = TRUE,
  crs = 3857
) {
  counties <- counties %||% tigris::counties(state = state, year = year)

  county <- filter_counties(
    counties,
    state = state,
    county = county,
    validate = validate
  )

  sf::st_transform(county, crs = crs)
}

#' Load primary and secondary roads using `tigris::primary_secondary_roads()`
#' @importFrom tigris primary_secondary_roads
load_primary_secondary_roads <- function(
  state,
  year = NULL,
  filter_by = NULL,
  crs = 3857
) {
  roads <- tigris::primary_secondary_roads(
    state = state,
    year = year,
    filter_by = filter_by
  )

  sf::st_transform(roads, crs = crs)
}

#' Load water for one or more counties using `tigris::area_water()`
#'
#' @param state State FIPS codes, either a single state or one for each county
#'   (e.g. the `STATEFP` column for counties in a multi-state MSA).
#' @param county County FIPS codes.
load_area_water <- function(
  state,
  county,
  year = NULL,
  crs = 3857
) {
  water <- Map(
    \(state, county) {
      tigris::area_water(state = state, county = county, year = year)
    },
    rep_len(state, length(county)),
    county
  )

  sf::st_transform(do.call(rbind, unname(water)), crs = crs)
}

#' Create a set of circular buffer areas around a county centroid
make_county_buffers <- function(
  county,
  dist = c(10, 20, 30, 40, 50),
  unit = "mi"
) {
  # FIXME: dist could or should be set based on the county geometry
  # TODO: Move [sfext::sf_bbox_diagdist()] into a standalone script and then
  # import into this repository
  cent <- suppressWarnings(sf::st_centroid(county))

  buffer_list <- lapply(
    dist,
    \(x) {
      buffer_dist <- units::as_units(x, unit)
      buffer_meters <- units::set_units(buffer_dist, value = "m")
      dplyr::bind_cols(
        dplyr::tibble(dist = x),
        sf::st_as_sf(sf::st_buffer(cent, dist = buffer_meters))
      )
    }
  )

  sf::st_as_sf(purrr::list_rbind(buffer_list))
}

#' Load an urban area using a county or counties as a spatial filter
load_urban_area <- function(
  county,
  ...,
  crs = 3857
) {
  urban_area <- tigris::urban_areas(filter_by = county, ...)

  sf::st_transform(urban_area, crs = crs)
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
  make_inner_lines = TRUE
) {
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
join_county_tracts <- function(
  data,
  county_tracts,
  largest = TRUE,
  col = "name",
  keep = 0.065
) {
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

#' Load sub-county divisions for a county
#'
#' @param type Division type: "tract", "zcta", or "custom". If `NA` or `NULL`,
#'   returns `NULL`.
#' @param source For custom divisions, a file path readable by [sf::read_sf()]
#'   or an ArcGIS layer URL readable by [arcgislayers::arc_read()].
#' @param county County `sf` object used to filter divisions and available to
#'   `processing_steps` as `{source: county}`.
#' @param name_col Column identifying custom divisions. Required if
#'   `snap_to_tracts = TRUE`.
#' @param snap_to_tracts If `TRUE`, rebuild custom divisions from the census
#'   tracts with the largest overlap with each division.
#' @param processing_steps Processing steps from geographies.yml run after the
#'   divisions are loaded and before they are snapped to tracts, e.g.
#'   `st_make_valid` or `ms_clip`.
load_divisions <- function(
  type,
  source = NULL,
  county,
  state_fips,
  county_fips,
  year = NULL,
  name_col = NULL,
  snap_to_tracts = FALSE,
  processing_steps = NULL,
  crs = 3857
) {
  if (is.null(type) || is.na(type)) {
    return(NULL)
  }

  if (type == "tract") {
    divisions <- tigris::tracts(
      state = state_fips,
      county = county_fips,
      year = year
    )
  } else if (type == "zcta") {
    # ZCTAs are only available nationally for recent years so filter by county
    divisions <- tigris::zctas(year = year, filter_by = county)
  } else if (grepl("^https?://", source)) {
    divisions <- load_arc_url(url = source, crs = crs)
  } else {
    divisions <- sf::read_sf(source)
  }

  divisions <- run_processing_steps(
    sf::st_transform(divisions, crs = crs),
    steps = processing_steps,
    data = list(county = county)
  )

  if (type != "custom" || !snap_to_tracts) {
    return(divisions)
  }

  join_division_tracts(
    divisions = divisions,
    crs = crs,
    division_type = tigris::tracts(
      state = state_fips,
      county = county_fips,
      year = year
    ),
    division_col = name_col,
    # Use an st_make_valid processing step instead
    make_divisions_valid = FALSE,
    make_inner_lines = FALSE
  )
}
