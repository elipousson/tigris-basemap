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
  format = "qs"
)

options(
  tigris_use_cache = TRUE,
  # TODO: Implement tigris year option
  tigris_year = 2023,
  basemap.crs = 3857,

  # to adapt this pipeline for a different metro area
  state.usps = "MD",
  county.name = "Baltimore city",
  division.url = "https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services/Community_Statistical_Areas_(CSAs)__Reference_Boundaries/FeatureServer/0"
)

tar_source()

prep_tigris_admin <- tar_plan(
  # FIXME: Allow handling of multiple states
  state_fips = tigris:::validate_state(getOption("state.usps")),
  county_fips = tigris:::validate_county(
    state = state_fips,
    county = getOption("county.name")
  ),
  state = dplyr::filter(
    tigris::states(),
    STATEFP == state_fips
  ),
  counties = load_county(state = state_fips),
  county = load_county(
    counties = counties,
    state = state_fips,
    county = county_fips,
    simplify = FALSE
  ),
  metro_areas = sf::st_transform(
    metro_divisions(filter_by = state),
    crs = getOption("basemap.crs")
  ),
  rail_lines = rails(
    filter_by = state,
    crs = getOption("basemap.crs")
  ),
  us_msa = sf::st_transform(
    tidycensus::get_acs(
      geography = "metropolitan statistical area/micropolitan statistical area",
      variables = "B01001_001",
      geometry = TRUE
    ),
    crs = getOption("basemap.crs")
  ),
  msa = sf::st_filter(us_msa, county),
  urban_area = load_urban_area(
    county = county,
    clip = msa,
    crs = getOption("basemap.crs")
  )
)

prep_division_geography <- tar_plan(
  divisions_src = load_arc_url(
    url = getOption("division.url"),
    crs = getOption("basemap.crs") # ,
    # clip = county,
    # remove_slivers = FALSE
  ),

  # FIXME: Assumption that divisions are composed of component tracts may not be
  # true in all cases
  divisions = join_division_tracts(
    divisions = divisions_src,
    state_fips = state_fips,
    county_fips = county_fips,
    division_type = mapbaltimore::baltimore_tracts |>
      sf::st_transform(crs = 3857),
    # division_type = tigris::block_groups(
    #   state = state_fips,
    #   county = county_fips
    # ),
    division_col = "Community"
  )
)

prep_county_features <- tar_plan(
  water = load_area_water(
    state = state_fips,
    county = county_fips,
    clip = county
  ),
  water_nhd = load_usgs_nhd(
    filter_geom = sf::st_bbox(county),
    clip = county,
    smooth = FALSE,
    simplify = TRUE
  ),
  water_nhd_lines = load_usgs_nhd(
    url = "https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer/6",
    filter_geom = sf::st_bbox(county),
    dissolve = FALSE,
    smooth = FALSE,
    # where = "FCODE='46006'",
    clip = county
  ),
  parks = load_usgs_pad(
    filter_geom = county$geometry,
    smooth = FALSE,
    min_area = 25,
    crs = getOption("basemap.crs")
  ),
  roads = load_primary_secondary_roads(
    state = state_fips,
    filter_by = county,
    clip = county
  )
)

prep_msa_features <- tar_plan(
  msa_water = load_area_water(
    state = state_fips,
    county = msa_counties[["COUNTYFP"]],
    clip = msa_counties_full
  ),
  msa_roads = load_primary_secondary_roads(
    state = state_fips,
    filter_by = msa,
    clip = msa_counties_full
  ),
  msa_filter_geom = msa |>
    sf::st_union(is_coverage = TRUE) |>
    sf::st_geometry() |>
    sf::st_concave_hull(0.1, allow_holes = FALSE) |>
    sf::st_union(sf::st_union(msa)),
  msa_parks = load_usgs_pad(
    filter_geom = msa_filter_geom,
    min_area = 40,
    crs = getOption("basemap.crs")
  ),
  msa_urban_area = rmapshaper::ms_erase(
    urban_area,
    erase = msa_water
  )
)

format_msa_counties <- tar_plan(
  msa_counties_init = filter_clip(counties, clip = msa),
  msa_counties_full = dplyr::filter(
    counties,
    .data[["COUNTYFP"]] %in% msa_counties_init[["COUNTYFP"]]
  ),
  msa_counties = ms_clip_ext(
    target = msa_counties_full,
    clip = msa
  )
)

# Replace the target list below with your own:
tigris_basemap_plan <- tar_plan(
  prep_tigris_admin,
  prep_division_geography,
  format_msa_counties,
  combined_area = combined_statistical_areas(
    filter_by = county
  ),
  prep_county_features,
  prep_msa_features,
  county_basemap = plot_county_basemap(
    water = water_nhd,
    roads = roads,
    parks = parks,
    divisions = divisions
  ),
  county_basemap_export = export_rds(
    county_basemap,
    paste0(
      state_fips,
      county_fips,
      "_county_basemap.rds"
    )
  ),
  county_buffers = make_county_buffers(county),
  msa_basemap = plot_msa_basemap(
    counties = msa_counties,
    urban_area = urban_area,
    water = msa_water,
    roads = msa_roads,
    county = county,
    parks = msa_parks,
    bg = county_buffers[5, ]
  ),
  county_buffers_export = export_rds(
    county_buffers,
    paste0(
      state_fips,
      county_fips,
      "_county_buffers.rds"
    )
  ),
  msa_basemap_export = export_rds(
    msa_basemap,
    paste0(
      state_fips,
      county_fips,
      "_msa_basemap.rds"
    )
  )
)

tigris_basemap_plan
