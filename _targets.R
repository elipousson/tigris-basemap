# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(tigris)

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
  basemap.crs = 3857,

  # to adapt this pipeline for a different metro area
  state.usps = "MD",
  county.name = "Baltimore city",
  division.url = "https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services/Community_Statistical_Areas_(CSAs)__Reference_Boundaries/FeatureServer/0"

)

tar_source()

# Replace the target list below with your own:
tar_plan(
  # FIXME: Allow handling of multiple states
  state_fips = tigris:::validate_state(getOption("state.usps")),
  county_fips = tigris:::validate_county(state = state_fips, county = getOption("county.name")),
  state = dplyr::filter(
    tigris::states(), STATEFP == state_fips
  ),
  counties = load_county(state = state_fips),
  # counties_5m = load_county(state = state_fips, cb = TRUE, resolution = "5m"),
  county = load_county(
    counties = counties,
    state = state_fips,
    county = county_fips,
    simplify = FALSE
  ),
  county_buffers = make_county_buffers(county),
  divisions = load_arc_url(
    url = getOption("division.url"),
    crs = getOption("basemap.crs"),
    clip = county
  ),
  metro_areas = sf::st_transform(
    metro_divisions(filter_by = state),
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
  msa_counties_init = filter_clip(counties, clip = msa),
  msa_counties_full = dplyr::filter(
    counties,
    .data[["COUNTYFP"]] %in% msa_counties_init[["COUNTYFP"]]
  ),
  msa_counties = ms_clip_ext(
    target = msa_counties_full,
    clip = msa
  ),
  urban_area = load_urban_area(
    county = county,
    clip = msa,
    crs = getOption("basemap.crs")
  ),
  combined_area = combined_statistical_areas(filter_by = county),
  water = load_area_water(
    state = state_fips,
    county = county_fips,
    clip = county
  ),
  water_nhd = load_usgs_nhd(
    filter_geom = sf::st_bbox(county),
    n_max = Inf,
    clip = county
  ),
  parks = load_usgs_pad(
    filter_geom = sf::st_geometry(county),
    keep = 0.04,
    smooth = FALSE,
    crs = getOption("basemap.crs")
  ),
  roads = load_primary_secondary_roads(
    state = state_fips,
    filter_by = county,
    clip = county
  ),
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
  msa_parks = load_usgs_pad(
    filter_geom = sf::st_geometry(sf::st_union(msa, is_coverage = TRUE)),
    crs = getOption("basemap.crs")
  ),
  msa_urban_area = rmapshaper::ms_erase(
    urban_area,
    erase = msa_water
  ),
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
  msa_basemap = plot_msa_basemap(
    counties = msa_counties,
    urban_area = urban_area,
    water = msa_water,
    roads = msa_roads,
    county = county,
    parks = msa_parks,
    bg = county_buffers[5, ]
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
