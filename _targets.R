library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c(
    "rlang",
    "tigris",
    "ggplot2",
    "dplyr"
  ),
  format = "qs",
  error = "continue"
)

tar_source()

options(
  tigris_use_cache = TRUE,
  basemap.crs = 3857
)

# To adapt this pipeline for a different county or metro area, add a new entry
# to geographies.yml. Geography values are inserted into target commands by
# tar_map() so changes to a geography invalidate only the affected targets.
geography_values <- make_geography_values(read_geographies("geographies.yml"))

# Data sources are specified in sources.yml and the data, view, and style layers
# for the county and MSA basemaps are specified in basemaps.yml. Loader
# arguments, processing steps, and style specifications are inserted into target
# commands with `!!` or `!!!` so changes invalidate only the affected targets.
source_config <- read_sources("sources.yml")
basemap_config <- read_basemaps("basemaps.yml", sources = source_config)

# Get processing steps for basemap data. `available` lists the pipeline data
# passed to run_processing_steps() that the steps can reference (e.g. to clip)
data_steps <- function(basemap, name, available = character(0)) {
  data_processing_steps(
    basemap_config,
    source_config,
    basemap,
    name,
    available = available
  )
}

# Pipeline data available to processing steps for MSA data targets
msa_data_refs <- c("msa", "msa_counties", "msa_hull")

# Get loader arguments for basemap data
data_args <- function(basemap, name) {
  data_source_args(basemap_config, source_config, basemap, name)
}

# Build static branches only if there are values to map over
tar_map_values <- function(values, ...) {
  if (nrow(values) == 0) {
    return(NULL)
  }

  tar_map(values = values, ...)
}

# National data for each tigris year
prep_national <- tar_map(
  values = make_year_values(geography_values),
  names = tigris_year,
  descriptions = NULL,
  tar_target(us_states, tigris::states(year = tigris_year)),
  tar_target(
    us_msa,
    sf::st_transform(
      tidycensus::get_acs(
        year = tigris_year,
        geometry = TRUE,
        !!!source_args(source_config, "acs_msa")
      ),
      crs = getOption("basemap.crs")
    )
  )
)

# State data for each state and tigris year, including every state in each MSA.
# States shared by multiple geographies or MSAs are only loaded once.
prep_state <- tar_map(
  values = make_state_values(geography_values),
  names = state_key,
  descriptions = NULL,
  tar_target(state_fips, validate_state(state = state_usps)),
  tar_target(state, dplyr::filter(us_states_ref, STATEFP == state_fips)),
  tar_target(
    counties,
    load_county(
      state = state_fips,
      year = tigris_year,
      !!!source_args(source_config, "tigris_counties")
    ) |>
      run_processing_steps(
        steps = !!source_processing_steps(source_config, "tigris_counties")
      )
  ),
  tar_target(
    metro_areas,
    sf::st_transform(
      tigris::metro_divisions(filter_by = state, year = tigris_year),
      crs = getOption("basemap.crs")
    ) |>
      run_processing_steps(steps = !!data_steps("msa", "metro_areas"))
  ),
  # Rail lines and roads for the whole state are combined and processed for
  # each MSA (see prep_msa)
  tar_target(
    rail_lines,
    tigris::rails(filter_by = state, year = tigris_year) |>
      sf::st_transform(getOption("basemap.crs"))
  ),
  tar_target(
    state_roads,
    load_primary_secondary_roads(
      state = state_fips,
      year = tigris_year,
      !!!source_args(source_config, "tigris_primary_secondary_roads")
    ) |>
      run_processing_steps(
        steps = !!source_processing_steps(
          source_config,
          "tigris_primary_secondary_roads"
        )
      )
  )
)

# MSA data for each MSA and tigris year. Columns ending in `_refs` (e.g.
# `counties_refs`) are lists of the state-level targets for every MSA state.
prep_msa <- tar_map(
  values = make_msa_values(geography_values),
  names = msa_key,
  descriptions = NULL,
  tar_target(msa, filter_msa(us_msa_ref, msa_name = msa_name)),
  tar_target(
    msa_counties,
    select_msa_counties(
      bind_sf(counties_refs),
      msa = msa,
      state_fips = msa_state_fips
    )
  ),
  tar_target(
    msa_hull,
    msa |>
      sf::st_union(is_coverage = TRUE) |>
      sf::st_geometry() |>
      sf::st_concave_hull(0.1, allow_holes = FALSE) |>
      sf::st_union(sf::st_union(msa)),
    description = "Concave hull around the MSA used to query and clip parks"
  ),
  tar_target(
    msa_water,
    load_area_water(
      state = msa_counties[["STATEFP"]],
      county = msa_counties[["COUNTYFP"]],
      year = tigris_year,
      !!!data_args("msa", "water")
    ) |>
      run_processing_steps(
        steps = !!data_steps("msa", "water", msa_data_refs),
        data = list(
          msa = msa,
          msa_counties = msa_counties,
          msa_hull = msa_hull
        )
      )
  ),
  tar_target(
    msa_roads,
    bind_sf(state_roads_refs, filter_by = msa_counties) |>
      run_processing_steps(
        steps = !!data_steps("msa", "roads", msa_data_refs),
        data = list(
          msa = msa,
          msa_counties = msa_counties,
          msa_hull = msa_hull
        )
      )
  ),
  tar_target(
    msa_parks,
    load_arc_url(
      filter_geom = msa_hull,
      crs = getOption("basemap.crs"),
      !!!data_args("msa", "parks")
    ) |>
      run_processing_steps(
        steps = !!data_steps("msa", "parks", msa_data_refs),
        data = list(
          msa = msa,
          msa_counties = msa_counties,
          msa_hull = msa_hull
        )
      )
  ),
  tar_target(
    msa_rail_lines,
    # Rail lines are loaded from a national file for each state so features
    # near state borders are duplicated
    bind_sf(rail_lines_refs, distinct_by = "LINEARID", filter_by = msa_hull) |>
      run_processing_steps(
        steps = !!data_steps("msa", "rail_lines", msa_data_refs),
        data = list(
          msa = msa,
          msa_counties = msa_counties,
          msa_hull = msa_hull
        )
      )
  )
)

# Tracked files for custom divisions loaded from a file path
prep_division_files <- tar_map_values(
  values = dplyr::filter(geography_values, !is.na(division_path)) |>
    dplyr::select(id, division_path),
  names = id,
  descriptions = NULL,
  tar_file(division_file, division_path)
)

# County data, basemaps, and exports for each geography
prep_geography <- tar_map(
  values = geography_values,
  names = id,
  descriptions = NULL,
  tar_target(
    county_fips,
    validate_county(state = state_fips_ref, county = county_name)
  ),
  tar_target(
    county,
    load_county(
      counties = counties_ref,
      state = state_fips_ref,
      county = county_fips
    )
  ),
  tar_target(
    county_msa,
    check_county_in_msa(county = county, msa = msa_ref),
    description = "MSA validated to include the focal county"
  ),
  tar_target(
    urban_area,
    load_urban_area(
      county = county,
      year = tigris_year,
      crs = getOption("basemap.crs"),
      !!!data_args("msa", "urban_area")
    ) |>
      # Use the MSA validated to include the focal county
      run_processing_steps(
        steps = !!data_steps("msa", "urban_area", "msa"),
        data = list(msa = county_msa)
      )
  ),
  tar_target(
    combined_area,
    tigris::combined_statistical_areas(filter_by = county, year = tigris_year) |>
      run_processing_steps(steps = !!data_steps("msa", "combined_area"))
  ),
  tar_target(
    divisions,
    load_divisions(
      type = division_type,
      source = division_src_ref,
      county = county,
      state_fips = state_fips_ref,
      county_fips = county_fips,
      year = tigris_year,
      name_col = division_name_col,
      snap_to_tracts = division_snap_to_tracts,
      processing_steps = division_steps,
      crs = getOption("basemap.crs")
    )
  ),
  tar_target(
    water,
    load_area_water(
      state = state_fips_ref,
      county = county_fips,
      year = tigris_year,
      !!!data_args("county", "area_water")
    ) |>
      run_processing_steps(
        steps = !!data_steps("county", "area_water", "county"),
        data = list(county = county)
      )
  ),
  tar_target(
    water_nhd,
    load_arc_url(
      filter_geom = sf::st_bbox(county),
      crs = getOption("basemap.crs"),
      !!!data_args("county", "water")
    ) |>
      run_processing_steps(
        steps = !!data_steps("county", "water", "county"),
        data = list(county = county)
      )
  ),
  tar_target(
    water_nhd_lines,
    load_arc_url(
      filter_geom = sf::st_bbox(county),
      crs = getOption("basemap.crs"),
      !!!data_args("county", "water_lines")
    ) |>
      run_processing_steps(
        steps = !!data_steps("county", "water_lines", "county"),
        data = list(county = county)
      )
  ),
  tar_target(
    parks,
    load_arc_url(
      filter_geom = county$geometry,
      crs = getOption("basemap.crs"),
      !!!data_args("county", "parks")
    ) |>
      run_processing_steps(
        steps = !!data_steps("county", "parks", "county"),
        data = list(county = county)
      )
  ),
  tar_target(
    roads,
    load_primary_secondary_roads(
      state = state_fips_ref,
      year = tigris_year,
      filter_by = county,
      !!!data_args("county", "roads")
    ) |>
      run_processing_steps(
        steps = !!data_steps("county", "roads", "county"),
        data = list(county = county)
      )
  ),
  tar_target(county_buffers, make_county_buffers(county)),
  # Layer data targets depend only on processing steps and bounds so paint and
  # layout changes only invalidate the basemap targets
  tar_target(
    county_layer_data,
    process_layers(
      data = list(
        county = county,
        divisions = divisions,
        water = water_nhd,
        roads = roads,
        parks = parks
      ),
      layers = !!layer_processing_specs(basemap_config, "county"),
      bounds = !!basemap_bounds(basemap_config, "county")
    )
  ),
  tar_target(
    county_basemap,
    plot_basemap(
      county_layer_data,
      layers = !!layer_paint_specs(basemap_config, "county"),
      view = !!basemap_view(basemap_config, "county")
    )
  ),
  tar_target(
    msa_layer_data,
    process_layers(
      data = list(
        counties = msa_counties_ref,
        msa = county_msa,
        county = county,
        buffer = county_buffers[5, ],
        water = msa_water_ref,
        roads = msa_roads_ref,
        parks = msa_parks_ref,
        urban_area = urban_area,
        rail_lines = msa_rail_lines_ref
      ),
      layers = !!layer_processing_specs(basemap_config, "msa"),
      bounds = !!basemap_bounds(basemap_config, "msa")
    )
  ),
  tar_target(
    msa_basemap,
    plot_basemap(
      msa_layer_data,
      layers = !!layer_paint_specs(basemap_config, "msa"),
      view = !!basemap_view(basemap_config, "msa")
    )
  ),
  tar_target(
    county_basemap_export,
    export_rds(county_basemap, paste0(id, "_county_basemap.rds"))
  ),
  tar_target(
    county_buffers_export,
    export_rds(county_buffers, paste0(id, "_county_buffers.rds"))
  ),
  tar_target(
    msa_basemap_export,
    export_rds(msa_basemap, paste0(id, "_msa_basemap.rds"))
  ),
  tar_target(
    county_export,
    export_rds(county, paste0(id, "_county.rds"))
  ),
  tar_target(
    msa_counties_export,
    export_rds(
      msa_layer_data$layers$counties,
      paste0(id, "_msa_counties.rds")
    )
  )
)

list(
  prep_national,
  prep_state,
  prep_msa,
  prep_division_files,
  prep_geography
)
