# =========================================================
# QUALITAT DE L'AIRE I MAPES POLARS — BAIX LLOBREGAT I BARCELONA (2026)
# =========================================================

# ---------------------------------------------------------
# 1. INSTAL·LACIÓ DE PAQUETS
# ---------------------------------------------------------

# Executar només la primera vegada
# install.packages(c(
#   "sf",
#   "ggplot2",
#   "rnaturalearth",
#   "rnaturalearthdata",
#   "dplyr",
#   "openair",
#   "openairmaps",
#   "htmlwidgets"
# ))

# ---------------------------------------------------------
# 2. CARREGAR LLIBRERIES
# ---------------------------------------------------------

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

library(openair)
library(openairmaps)

library(htmlwidgets)

# ---------------------------------------------------------
# 3. PREPARACIÓ DE DADES
# ---------------------------------------------------------

# Convertir data a format POSIXct
martorellhospitalet$date <- as.POSIXct(
  martorellhospitalet$date
)

# Convertir a objecte espacial sf
dades_sf <- st_as_sf(
  martorellhospitalet,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# Mostrar estructura
print(dades_sf)
head(dades_sf)

# ---------------------------------------------------------
# 4. MAPA SIMPLE DE PUNTS
# ---------------------------------------------------------

plot(
  st_geometry(dades_sf),
  pch = 19,
  cex = 2
)

text(
  st_coordinates(dades_sf),
  labels = dades_sf$site,
  pos = 3
)

# ---------------------------------------------------------
# 5. MAPA BASE AMB GGPLOT2
# ---------------------------------------------------------

# Mapa del món
mon <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

ggplot() +

  geom_sf(
    data = mon,
    fill = "grey95",
    color = "white"
  ) +

  geom_sf(
    data = dades_sf,
    aes(color = incidencia_1000),
    size = 4
  ) +

  coord_sf(
    xlim = c(1.7, 2.3),
    ylim = c(41.2, 41.6),
    expand = FALSE
  ) +

  scale_color_viridis_c(
    option = "plasma",
    name = "Incidència\n(per 1000)"
  ) +

  theme_minimal() +

  labs(
    title = "Incidència a Martorell",
    caption = "Font: Dades pròpies"
  )

# ---------------------------------------------------------
# 6. MAPA MINIMALISTA
# ---------------------------------------------------------

ggplot(data = dades_sf) +

  geom_sf(
    color = "red",
    size = 10
  ) +

  theme_minimal()

# ---------------------------------------------------------
# 7. RESUM ESPACIAL
# ---------------------------------------------------------

resum_martorell <- dades_sf %>%

  summarise(
    incidencia_mitjana = mean(
      incidencia_1000,
      na.rm = TRUE
    ),
    geometry = st_union(geometry)
  )

# Límits del mapa
xlim <- c(1.85, 2.15)
ylim <- c(41.30, 41.55)

ggplot() +

  geom_sf(
    data = mon,
    fill = "grey90",
    color = "white"
  ) +

  geom_sf(
    data = resum_martorell,
    aes(fill = incidencia_mitjana),
    color = "black",
    size = 10,
    shape = 21,
    stroke = 1.5
  ) +

  geom_sf_text(
    data = resum_martorell,
    aes(label = paste0(
      round(incidencia_mitjana, 2),
      " /1000"
    )),
    nudge_y = 0.02,
    fontface = "bold"
  ) +

  annotate(
    "point",
    x = 2.100,
    y = 41.360,
    shape = 1,
    size = 8,
    color = "blue",
    stroke = 1.5
  ) +

  annotate(
    "text",
    x = 2.100,
    y = 41.380,
    label = "Hospitalet (sense dades)",
    color = "blue",
    size = 4,
    fontface = "italic"
  ) +

  coord_sf(
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  ) +

  scale_fill_gradient(
    low = "yellow",
    high = "red3",
    name = "Incidència mitjana\n(per 1000)"
  ) +

  theme_minimal() +

  labs(
    title = "Incidència mitjana a Martorell (2012)",
    subtitle = paste(
      "Basat en",
      nrow(dades_sf),
      "dies"
    ),
    caption = "Font: Dades pròpies"
  )

# ---------------------------------------------------------
# 8. POLAR PLOT
# ---------------------------------------------------------

p_polar <- polarPlot(
  martorellhospitalet,
  pollutant = "no2",
  main = "NO₂ Polar Plot – Martorell"
)

# Mostrar gràfic
p_polar

# ---------------------------------------------------------
# 9. GUARDAR POLAR PLOT COM HTML
# ---------------------------------------------------------

htmlwidgets::saveWidget(
  p_polar,
  file = "polarplot_martorell.html",
  selfcontained = TRUE
)

# ---------------------------------------------------------
# 10. POLAR MAP INTERACTIU
# ---------------------------------------------------------

p_map <- polarMap(
  martorellhospitalet,

  pollutant = "no2",

  latitude = "lat",
  longitude = "lon",

  ws = "ws",
  wd = "wd",

  provider = "CartoDB.Positron",

  cols = "turbo",

  d.icon = 150
)

# Mostrar mapa
p_map

# ---------------------------------------------------------
# 11. GUARDAR POLAR MAP COM HTML
# ---------------------------------------------------------

htmlwidgets::saveWidget(
  p_map,
  file = "polarmap_martorell.html",
  selfcontained = TRUE
)

# ---------------------------------------------------------
# 12. EXPORTAR DIVERSOS CONTAMINANTS
# ---------------------------------------------------------

contaminants <- c(
  "no2",
  "nox",
  "so2",
  "h2s"
)

for (pol in contaminants) {

  p <- polarMap(
    martorellhospitalet,

    pollutant = pol,

    latitude = "lat",
    longitude = "lon",

    ws = "ws",
    wd = "wd",

    provider = "CartoDB.Positron",

    d.icon = 150
  )

  saveWidget(
    p,
    paste0("polarmap_", pol, ".html"),
    selfcontained = TRUE
  )

  print(
    paste("Creat:", pol)
  )
}

# ---------------------------------------------------------
# 13. INFORMACIÓ DE DADES
# ---------------------------------------------------------

names(martorellhospitalet)

head(martorellhospitalet)

unique(martorellhospitalet$site)

summary(martorellhospitalet$ws)

summary(martorellhospitalet$wd)

# =========================================================
# FI DE L'SCRIPT
# =========================================================
