# AKCatchments hex sticker: Natural Earth rivers over Alaska, with the
# Tanana River basin (WBD HUC6 190803) as the delineated catchment.

library(sf)
library(dplyr)
library(ggplot2)

alaska <- rnaturalearthdata::states50 |>
  st_as_sf() |>
  filter(name == "Alaska") |>
  st_transform(3338) |>
  st_geometry() |>
  st_cast("POLYGON") |>
  # drop the far Aleutians so the state fits the hex at a readable size
  st_crop(xmin = -1.15e6, ymin = 4e5, xmax = 1.6e6, ymax = 2.4e6) |>
  st_union()

catchment <- st_read(
  "https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/3/query?where=huc6%3D%27190803%27&outFields=huc6%2Cname&f=geojson",
  quiet = TRUE
) |>
  st_transform(3338) |>
  st_geometry() |>
  st_simplify(dTolerance = 2000)

# rivers_north_america adds the smaller Alaska rivers (scalerank 10-12)
rivers <- c("rivers_lake_centerlines", "rivers_north_america") |>
  purrr::map(\(layer) {
    sprintf("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/v5.1.2/geojson/ne_10m_%s.geojson", layer) |>
      st_read(quiet = TRUE) |>
      select(name, scalerank)
  }) |>
  bind_rows() |>
  st_transform(3338) |>
  st_set_agr("constant") |>
  st_intersection(st_union(alaska, catchment))

catchment_rivers <- st_intersection(st_set_agr(rivers, "constant"), catchment)
outlet <- st_sfc(st_point(c(-151.98, 65.17)), crs = 4326) # Tanana-Yukon confluence

# Pointy-top hex sized around the state, with room for the name below it
hex_radius <- 1.65e6
hex_centre <- c(1.7e5, 1.3e6)
hex <- st_polygon(list(cbind(
  hex_centre[1] + hex_radius * cos(pi / 2 + c(0:5, 0) * pi / 3),
  hex_centre[2] + hex_radius * sin(pi / 2 + c(0:5, 0) * pi / 3)
))) |>
  st_sfc(crs = 3338)

sticker <- ggplot() +
  geom_sf(data = hex, fill = "#0B2545", colour = NA) +
  geom_sf(data = alaska, fill = "#1B3F6E", colour = NA) +
  geom_sf(data = rivers, aes(linewidth = scalerank), colour = "#4B89B5", lineend = "round") +
  geom_sf(data = catchment, fill = "#2EC4B6", alpha = 0.35, colour = "#2EC4B6", linewidth = 0.3) +
  geom_sf(data = catchment_rivers, aes(linewidth = scalerank), colour = "white", lineend = "round") +
  geom_sf(data = outlet, shape = 21, size = 1.2, stroke = 0.5, fill = "white", colour = "#2EC4B6") +
  annotate(
    "text", hex_centre[1], hex_centre[2] - 0.6 * hex_radius,
    label = "AKCatchments", family = "Lato", fontface = "bold", size = 3.6, colour = "white"
  ) +
  geom_sf(data = hex, fill = NA, colour = "#2EC4B6", linewidth = 1.6) +
  scale_linewidth(range = c(0.4, 0.08), guide = "none") +
  coord_sf(
    xlim = hex_centre[1] + c(-1, 1) * hex_radius * sqrt(3) / 2,
    ylim = hex_centre[2] + c(-1, 1) * hex_radius,
    expand = FALSE, datum = NA
  ) +
  theme_void()

# standard hex sticker size
ggsave(
  "man/figures/logo.png", sticker,
  width = 43.9, height = 50.8, units = "mm", dpi = 600, bg = "transparent", device = ragg::agg_png
)
