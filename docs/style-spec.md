# Basemap style specification

The data, view, and style layers for each basemap are specified in `basemaps.yml`. Style layers follow the [MapLibre Style Spec](https://maplibre.org/maplibre-style-spec/layers/) where possible and are drawn with `ggplot2::geom_sf()`. This document explains the YAML schemas, how MapLibre keys map onto ggplot2 arguments, and the processing steps used to prepare data for each layer.

## Overview

The pipeline handles data in three stages:

1. **Load:** data from a source in `sources.yml` is downloaded with a spatial query for the area of interest (e.g. `filter_geom` or `filter_by`) and an optional `where` clause, then transformed to the basemap CRS (EPSG:3857). Loaders don't change geometry otherwise: repairing, clipping, and generalizing geometry are all explicit processing steps. Loader functions are in `R/functions.R` and are called from `_targets.R`.
2. **Process data:** the `processing_steps` for the source in `sources.yml` (e.g. `st_make_valid`) run first, followed by the `processing_steps` for the data entry in `basemaps.yml` (e.g. `ms_clip` to clip to the area of interest or `filter` to select features). See [Data processing steps](#data-processing-steps).
3. **Style:** each layer in `basemaps.yml` takes data, runs the layer `processing_steps` (e.g. simplifying, buffering, or erasing geometry for cartographic purposes), and draws it using the layer `paint` and `layout` properties.

For each geography and basemap, the pipeline creates two targets:

- `<basemap>_layer_data_<id>` runs the layer processing steps and computes the map bounds. It is invalidated only when data, processing steps, or bounds change.
- `<basemap>_basemap_<id>` draws the basemap. It is invalidated when paint, layout, or view properties change without re-running any processing steps.

## `sources.yml`

| Key | Required | Description |
|---|---|---|
| `name` | yes | Display name for the source. |
| `description` | yes | What the data is, how it is loaded, and any fixed filtering. |
| `type` | yes | `tigris`, `arcgis`, or `acs`. |
| `function` | `tigris` and `acs` | Function used to load the data, e.g. `tigris::area_water`. |
| `url` | `arcgis` | ArcGIS layer URL. Passed to `load_arc_url()`. |
| `attribution` | no | Data provider. |
| `args` | no | Arguments passed to the loader function, e.g. `where: GIS_Acres >= 20` to filter an ArcGIS layer on the server. Data entries in `basemaps.yml` can override these arguments. `clip`, `filter_geom`, `filter_by`, `crs`, `url`, and `year` are set by the pipeline and can't be used. |
| `processing_steps` | no | Processing steps required wherever the source is used, e.g. `st_make_valid` for a source with invalid geometry. These always run first and can't reference other data. |

Sources can return invalid geometry (e.g. polygons with reversed ring orientation), which can cause wrong or negative areas from `sf::st_area()` or errors from rmapshaper. The pipeline doesn't repair geometry automatically, so add an `st_make_valid` step to any source where `sf::st_is_valid()` returns `FALSE` for some features. As of October 2026, this applies to `usgs_pad` (about 7% of features) and the Baltimore Community Statistical Areas used as custom divisions.

## `basemaps.yml`

Each entry under `basemaps` has these keys:

| Key | Required | Description |
|---|---|---|
| `description` | no | Description of the basemap. |
| `data` | yes | Named data used by the basemap. Each entry has a `source` from `sources.yml`, an optional `description`, optional `args` that override the source `args` with the same name, and optional `processing_steps` that run after the source processing steps. See [Data processing steps](#data-processing-steps). |
| `staging` | no | Data loaded by the pipeline but not yet used by the basemap. Same keys as `data`. |
| `view` | no | Map extent and frame. See [View](#view). |
| `layers` | yes | Style layers in draw order. See [Layers](#layers). |

The data available to layers includes the names in `data` and the data provided by the pipeline:

| Name | Description |
|---|---|
| `county` | The focal county for the geography. |
| `counties` | The full (unclipped) counties in the MSA, selected by matching the county `CBSAFP` to the MSA `GEOID` (MSA basemap only). TIGER/Line counties extend into open water, so clip them to `msa` (e.g. with the `*clip_msa` anchor) to follow the shoreline. |
| `msa` | The ACS boundary of the MSA after checking that it includes the focal county (MSA basemap only). |
| `buffer` | A 50-mile buffer around the focal county (MSA basemap only). |
| `divisions` | Sub-county divisions from `geographies.yml` (county basemap only). `NULL` if not specified, in which case layers using it are skipped. |

The data names listed in `data` must match the names passed to `process_layers()` in `_targets.R`. Changing the `source` of a data entry to a source of a different `type` also requires changing the loader called in `_targets.R`.

### View

| Key | Description | MapLibre equivalent |
|---|---|---|
| `bounds` | Map extent. Either a `[west, south, east, north]` array in longitude and latitude or a reference to data (e.g. `{source: county}` or `{source: [counties, buffer, water]}`) with optional `processing_steps` (e.g. `st_buffer` to add padding). | The array uses the same format as [`bounds`](https://maplibre.org/maplibre-style-spec/sources/#bounds) for sources, but sets the map extent instead of limiting the tiles requested. A reference works like `fitBounds()` in the MapLibre JavaScript API. |
| `neatline` | If `true`, draw a frame at the bounds with `maplayer::layer_neatline()`. | None. |
| `theme` | ggplot2 theme: `void` (default), `minimal`, `bw`, `classic`, or `light`. | None. |

### Layers

| Key | Required | Description | MapLibre equivalent |
|---|---|---|---|
| `id` | yes | Unique layer identifier. | [`id`](https://maplibre.org/maplibre-style-spec/layers/#id) |
| `type` | yes | `fill`, `line`, `circle`, or `background`. | [`type`](https://maplibre.org/maplibre-style-spec/layers/#type) |
| `source` | yes, except `background` | Name of the data used by the layer. | [`source`](https://maplibre.org/maplibre-style-spec/layers/#source) |
| `processing_steps` | no | Steps run on the data before drawing. See [Processing steps](#processing-steps). | None. A leading `filter` step corresponds to the MapLibre [`filter`](https://maplibre.org/maplibre-style-spec/layers/#filter). |
| `layout` | no | Layout properties. | [`layout`](https://maplibre.org/maplibre-style-spec/layers/#layout) |
| `paint` | no | Paint properties. | [`paint`](https://maplibre.org/maplibre-style-spec/layers/#paint) |

Layers are drawn in the order listed. The same data can be used by multiple layers, e.g. a wide white `line` layer under a `fill` layer to create a halo.

## Mapping MapLibre properties to ggplot2

| Layer type | MapLibre property | ggplot2 `geom_sf()` argument | Conversion |
|---|---|---|---|
| all | `layout.visibility` | — | `none` skips the layer (and its processing steps). Default `visible`. |
| `fill` | — | `colour = NA` | Polygons are drawn without an outline unless `fill-outline-color` is set. |
| `fill` | `fill-color` | `fill` | Default `#000000`. |
| `fill` | `fill-opacity` | `fill` | Applied to the fill color with `ggplot2::alpha()`. 0–1, default 1. |
| `fill` | `fill-outline-color` | `colour` | Drawn 1 px wide, as in MapLibre. Use a separate `line` layer for other widths. |
| `line` | — | `fill = NA` | A `line` layer on polygon data draws the polygon outlines. |
| `line` | `line-color` | `colour` | Default `#000000`. |
| `line` | `line-opacity` | `colour` | Applied to the line color with `ggplot2::alpha()` so it also applies to polygon outlines. 0–1, default 1. |
| `line` | `line-width` | `linewidth` | Pixels divided by `ggplot2::.pt` (≈ 2.845). One ggplot2 `linewidth` unit is 1/96 inch × `.pt`, so the conversion is exact. Default 1. |
| `line` | `line-dasharray` | `linetype` | Dash and gap lengths are converted to a hexadecimal string, e.g. `[1, 3, 4, 3]` → `"1343"` (ggplot2's `dotdash`). Must be an even number of integers from 1 to 15. Both MapLibre and ggplot2 scale dashes with the line width. |
| `line` | `layout.line-cap` | `lineend` | Same values: `butt`, `round`, `square`. |
| `line` | `layout.line-join` | `linejoin` | Same values: `bevel`, `round`, `miter`. |
| `circle` | `circle-color` | `fill` | Points use `shape = 21`. Default `#000000`. |
| `circle` | `circle-opacity` | `fill` | Applied to the fill color. |
| `circle` | `circle-radius` | `size` | 2 × radius divided by `ggplot2::.pt` (approximate). Default 5. |
| `circle` | `circle-stroke-color` | `colour` | Default `#000000`. |
| `circle` | `circle-stroke-opacity` | `colour` | Applied to the stroke color. |
| `circle` | `circle-stroke-width` | `stroke` | Pixels divided by `ggplot2::.pt`. Default 0. |
| `background` | `background-color` | `theme(panel.background, plot.background)` | Fills the whole plot. |
| `background` | `background-opacity` | `theme(...)` | Applied to the background color. |

Colors can use any value accepted by `grDevices::col2rgb()`, but hex colors (e.g. `#737373`) are recommended because R color names like `gray45` are not valid CSS colors and won't work in MapLibre.

### Unsupported MapLibre features

These MapLibre features don't apply to static basemaps or are not yet supported, and are rejected when `basemaps.yml` is read:

- Layer types: `symbol`, `raster`, `hillshade`, `heatmap`, `fill-extrusion`, `color-relief`.
- Layer properties: `minzoom`, `maxzoom`, `source-layer`, `metadata`.
- Paint properties not listed above, e.g. `fill-pattern`, `line-pattern`, `line-offset`, `line-blur`, `line-gap-width`, `fill-translate`.
- Data-driven or zoom-dependent property values (expressions in paint properties).

## Processing steps

Processing steps run in the order listed. Each step has a `name` matching the function it calls and optional `args`. The data is passed as the first argument of the function and can't be set in `args`.

```yaml
processing_steps:
  - name: ms_simplify
    args:
      keep: 0.02
  - name: st_union
```

| Step | Function | Supported `args` |
|---|---|---|
| `filter` | `dplyr::filter()` with a MapLibre expression | `expression` |
| `filter_min_area` | `filter_min_area()` (custom) | `min_area`, `units` (default `acres`). Area is measured on the ellipsoid, not in Web Mercator (which overstates area by about 1.67 times at 39° N). For ArcGIS sources, prefer a `where` clause in `args`. |
| `ms_simplify` | `rmapshaper::ms_simplify()` | `keep`, `method`, `weighting`, `keep_shapes`, `no_repair`, `snap`, `explode`, `snap_interval` |
| `ms_dissolve` | `rmapshaper::ms_dissolve()` | `field`, `sum_fields`, `copy_fields`, `weight`, `snap` |
| `ms_erase` | `rmapshaper::ms_erase()` | `erase` (reference), `remove_slivers` |
| `ms_clip` | `rmapshaper::ms_clip()` | `clip` (reference), `remove_slivers` |
| `ms_innerlines` | `rmapshaper::ms_innerlines()` | — |
| `smooth` | `smoothr::smooth()` | `method`, `refinements`, `smoothness`, `bandwidth`, `n`, `max_distance`, `vertex_factor` |
| `st_buffer` | `sf::st_buffer()` | `dist` (in CRS units, i.e. meters in EPSG:3857), `nQuadSegs`, `endCapStyle`, `joinStyle`, `mitreLimit`, `singleSide` |
| `st_union` | `sf::st_union()` (returned as `sf`) | `is_coverage` |
| `st_make_valid` | `sf::st_make_valid()` | `geos_method`, `geos_keep_collapsed` |
| `st_cast` | `sf::st_cast()` | `to`, `group_or_split`, `warn` |

`ms_clip` and `ms_erase` call rmapshaper directly, so `remove_slivers` defaults to `false` and invalid geometry is not repaired first. Add `remove_slivers: true` where needed and use `st_make_valid` on sources with invalid geometry.

New steps can be added to `processing_step_registry()` in `R/processing-steps.R`.

### Data processing steps

Data processing steps run when data is loaded, in this order:

1. The source `processing_steps` from `sources.yml` (which can't reference other data).
2. The data entry `processing_steps` from `basemaps.yml`.

Data entry steps can reference the pipeline data available to the target that loads the data:

| Data | Available references |
|---|---|
| County basemap `data` and `staging` | `county` |
| MSA basemap `water`, `roads`, and `parks` | `msa` (the ACS MSA boundary), `msa_counties` (the full, unclipped counties in the MSA), and `msa_hull` (a concave hull around the MSA) |
| MSA basemap `urban_area` | `msa` (the MSA after checking that it includes the focal county) |
| MSA basemap `rail_lines`, `metro_areas`, and `combined_area` | none |

MSA data is loaded once for each MSA and shared by every geography in the MSA, so it can't reference `county`. References are checked when `_targets.R` is sourced.

Division `processing_steps` in `geographies.yml` run after the divisions are loaded and before custom divisions are snapped to tracts. They can reference `county`, e.g. to clip ZCTAs or unsnapped custom divisions to the county.

### Reusing steps with anchors and aliases

Repeated steps can be defined once with a YAML anchor (`&name`) under the top-level `definitions` key in `basemaps.yml` and inserted with an alias (`*name`). yaml12 resolves aliases into copies when the file is read:

```yaml
definitions:
  clip_county: &clip_county
    name: ms_clip
    args:
      clip:
        source: county
      remove_slivers: true

basemaps:
  county:
    data:
      water:
        source: usgs_nhd_waterbody
        processing_steps:
          - *clip_county
```

Each alias must refer to a single step. Merge keys (`<<: *name`) are not supported by yaml12 and fail validation as an unsupported key or property.

### References to other data

In layer processing steps, data processing steps (see above), and `view.bounds`, an argument written as `{source: <name>}` is replaced with that data. A reference can list multiple sources (combined into one geometry collection) and can have its own `processing_steps`:

```yaml
- name: ms_erase
  args:
    erase:
      source: water
      processing_steps:
        - name: st_buffer
          args:
            dist: 2000
        - name: st_union
    remove_slivers: true
```

### Filter expressions

The `filter` step uses a subset of [MapLibre expressions](https://maplibre.org/maplibre-style-spec/expressions/):

| Expression | R equivalent |
|---|---|
| `["get", "RTTYP"]` | `.data[["RTTYP"]]` |
| `["literal", ["I", "U"]]` | `c("I", "U")` |
| `["==", a, b]`, `["!=", a, b]`, `["<", a, b]`, `["<=", a, b]`, `[">", a, b]`, `[">=", a, b]` | `a == b`, etc. |
| `["in", a, b]` | `a %in% b` |
| `["!", a]` | `!a` |
| `["all", a, b, ...]` | `a & b & ...` |
| `["any", a, b, ...]` | `a \| b \| ...` |

For example, to keep Interstates and U.S. highways:

```yaml
- name: filter
  args:
    expression: ["in", ["get", "RTTYP"], ["literal", ["I", "U"]]]
```
