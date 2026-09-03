library(sf)
library(ggplot2)
library(dplyr)
library(geodata)
library(rnaturalearth)
library(patchwork)

# Uganda administrative boundaries (district level)
uga <- geodata::gadm("UGA", level = 2, path = tempdir()) |>
  st_as_sf()

# Karamoja districts (adjust depending on your definition/year)
karamoja_districts <- c(
  "Abim",
  "Amudat",
  "Kaabong",
  "Karenga",
  "Kotido",
  "Moroto",
  "Nabilatuk",
  "Nakapiripirit",
  "Napak"
)

tribal_area <- c(
  Abim = "Labwor",
  Amudat = "Pokot",
  Kaabong = "Dodoth",
  Karenga = "Dodoth",
  Kotido = "Jie",
  Moroto = "Matheniko",
  Nabilatuk = "Chekwii",
  Nakapiripirit = "Pian",
  Napak = "Bokora"
)
karamoja <- uga |>
  filter(NAME_1 %in% karamoja_districts)

# Regional outline
karamoja_outline <- st_union(karamoja)

district_labels <- st_point_on_surface(karamoja)

# Approximate major towns
towns <- data.frame(
  city = c("Moroto", "Kotido", "Kaabong", "Nakapiripirit"),
  lon = c(34.67, 34.10, 34.13, 34.65),
  lat = c(2.53, 2.98, 3.51, 1.91)
) |>
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# Main map
p_main <- ggplot() +
  geom_sf(data = karamoja, fill = "khaki", color = "grey40") +
  geom_sf(data = karamoja_outline,
          fill = NA,
          linewidth = 1.2,
          color = "black") +
  # geom_sf_text(
  #   data = district_labels,
  #   aes(label = NAME_2),
  #   size = 3.5,
  #   fontface = "bold"
  # ) +
  geom_sf(data = towns, size = 2, color = "red") +
  # geom_sf_text(data = towns,
  #              aes(label = city),
  #              nudge_y = 0.08,
  #              size = 3) +
  theme_minimal() +
  scale_x_continuous("")+
  scale_y_continuous("")+
  labs(
    title = "Karamoja Region",
    subtitle = "Districts and major towns"
  )

# Uganda inset
uganda <- geodata::gadm("UGA", level = 0, path = tempdir()) |>
  st_as_sf()

p_uganda <- ggplot() +
  geom_sf(data = uganda, fill = "grey90") +
  geom_sf(data = karamoja_outline,
          fill = "red",
          color = "red") +
  theme_void() +
  labs(title = "Uganda")

# East Africa inset
ea <- rnaturalearth::ne_countries(
  scale = "medium",
  country = c(
    "Uganda",
    "Kenya",
    "South Sudan",
    "Ethiopia",
    "Tanzania",
    "Rwanda",
    "Burundi",
    "Democratic Republic of the Congo"
  ),
  returnclass = "sf"
)

africa <- rnaturalearth::ne_countries(
  continent = "Africa",
  returnclass = "sf"
)

p_africa <- ggplot() +
  geom_sf(
    data = africa,
    fill = "grey90",
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = karamoja_outline,
    fill = "red",
    color = "red"
  ) +
  coord_sf(
    xlim = c(-20, 55),
    ylim = c(-35, 38),
    expand = FALSE
  ) +
  theme_void() +
  labs(title = "Africa")

# Combine
(p_main | p_uganda / p_africa) +
  plot_layout(widths = c(4, 1)) 

p_ea <- ggplot() +
  geom_sf(data = ea,
          fill = "grey90",
          color = "white") +
  geom_sf(data = karamoja_outline,
          color = "red",
          fill = "red") +
  coord_sf(
    xlim = c(25, 42),
    ylim = c(-6, 12)
  ) +
  theme_void() +
  labs(title = "East Africa")

# Combine
(p_main | (p_uganda / p_ea)) +
  plot_layout(widths = c(3, 1))
