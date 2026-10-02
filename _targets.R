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

# Data sources are specified in sources.yml and the processing for each layer of
# the county and MSA basemaps is specified in basemaps.yml. Loader arguments are
# inserted into target commands with `!!!` so changes to a source or layer
# invalidate only the affected targets.
source_config <- read_sources("sources.yml")
basemap_config <- read_basemaps("basemaps.yml", sources = source_config)

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

# State data for each state and tigris year
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
    )
  ),
  tar_target(
    metro_areas,
    sf::st_transform(
      tigris::metro_divisions(filter_by = state, year = tigris_year),
      crs = getOption("basemap.crs")
    )
  ),
  tar_target(
    rail_lines,
    tigris::rails(filter_by = state, year = tigris_year) |>
      sf::st_transform(getOption("basemap.crs"))
  )
)

# MSA data for each MSA and tigris year
# TODO: Add support for multi-state MSAs
prep_msa <- tar_map(
  values = make_msa_values(geography_values),
  names = msa_key,
  descriptions = NULL,
  tar_target(msa, filter_msa(us_msa_ref, msa_name = msa_name)),
  tar_target(msa_counties_init, filter_clip(counties_ref, clip = msa)),
  tar_target(
    msa_counties_full,
    dplyr::filter(
      counties_ref,
      .data[["COUNTYFP"]] %in% msa_counties_init[["COUNTYFP"]]
    )
  ),
  tar_target(
    msa_counties,
    ms_clip_ext(target = msa_counties_full, clip = msa)
  ),
  tar_target(
    msa_water,
    load_area_water(
      state = state_fips_ref,
      county = msa_counties[["COUNTYFP"]],
      year = tigris_year,
      clip = msa_counties_full,
      !!!layer_args(basemap_config, "msa", "water")
    )
  ),
  tar_target(
    msa_roads,
    load_primary_secondary_roads(
      state = state_fips_ref,
      year = tigris_year,
      filter_by = msa,
      clip = msa_counties_full,
      !!!layer_args(basemap_config, "msa", "roads")
    )
  ),
  tar_target(
    msa_filter_geom,
    msa |>
      sf::st_union(is_coverage = TRUE) |>
      sf::st_geometry() |>
      sf::st_concave_hull(0.1, allow_holes = FALSE) |>
      sf::st_union(sf::st_union(msa))
  ),
  tar_target(
    msa_parks,
    load_usgs_pad(
      filter_geom = msa_filter_geom,
      crs = getOption("basemap.crs"),
      !!!layer_args(basemap_config, "msa", "parks")
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
      county = county_fips,
      simplify = FALSE
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
      clip = county_msa,
      year = tigris_year,
      crs = getOption("basemap.crs")
    )
  ),
  tar_target(
    combined_area,
    tigris::combined_statistical_areas(filter_by = county, year = tigris_year)
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
      crs = getOption("basemap.crs")
    )
  ),
  tar_target(
    water,
    load_area_water(
      state = state_fips_ref,
      county = county_fips,
      year = tigris_year,
      clip = county,
      !!!layer_args(basemap_config, "county", "area_water")
    )
  ),
  tar_target(
    water_nhd,
    load_usgs_nhd(
      filter_geom = sf::st_bbox(county),
      clip = county,
      !!!layer_args(basemap_config, "county", "water")
    )
  ),
  tar_target(
    water_nhd_lines,
    load_usgs_nhd(
      filter_geom = sf::st_bbox(county),
      clip = county,
      !!!layer_args(basemap_config, "county", "water_lines")
    )
  ),
  tar_target(
    parks,
    load_usgs_pad(
      filter_geom = county$geometry,
      crs = getOption("basemap.crs"),
      !!!layer_args(basemap_config, "county", "parks")
    )
  ),
  tar_target(
    roads,
    load_primary_secondary_roads(
      state = state_fips_ref,
      year = tigris_year,
      filter_by = county,
      clip = county,
      !!!layer_args(basemap_config, "county", "roads")
    )
  ),
  tar_target(
    msa_urban_area,
    rmapshaper::ms_erase(urban_area, erase = msa_water_ref)
  ),
  tar_target(
    county_basemap,
    plot_county_basemap(
      water = water_nhd,
      roads = roads,
      parks = parks # ,
      # divisions = divisions
    )
  ),
  tar_target(county_buffers, make_county_buffers(county)),
  tar_target(
    msa_basemap,
    plot_msa_basemap(
      counties = msa_counties_ref,
      urban_area = urban_area,
      water = msa_water_ref,
      roads = msa_roads_ref,
      county = county,
      parks = msa_parks_ref,
      bg = county_buffers[5, ]
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
    export_rds(msa_counties_ref, paste0(id, "_msa_counties.rds"))
  )
)

list(
  prep_national,
  prep_state,
  prep_msa,
  prep_division_files,
  prep_geography
)
