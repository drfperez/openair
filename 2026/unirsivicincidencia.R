
# ===================================================
# UNIR CSV MANTENINT TOTES LES COLUMNES ORIGINALS
# ===================================================

library(tidyverse)

# 1️⃣ Carpeta on tens els CSV
carpeta <- "ruta/a/teus/arxius"  # Canvia-ho a la teva ruta

# 2️⃣ Llistar fitxers CSV
fitxers <- list.files(path = carpeta, pattern = "\\.csv$", full.names = TRUE)

# 3️⃣ Llegir i combinar fitxers sense modificar columnes
totes_dades <- purrr::map_df(fitxers, function(f){
  read_csv(f, col_types = cols())  # Manté tots els tipus de columna originals
})

# 4️⃣ Revisar que totes les columnes es mantinguin correctes
glimpse(totes_dades)

# 5️⃣ Guardar dataset combinat
write_csv(totes_dades, "dades_combinades.csv")

cat("✅ Dataset combinat correctament! Total files:", nrow(totes_dades), "\n")


*****************************************




# ===============================================
# CALCULAR INCIDÈNCIA DIÀRIA GLOBAL (tots els ABS)
# ===============================================

library(tidyverse)

# Suposem que el dataset combinat ja es diu 'totes_dades'
# i té les columnes: data, casos, poblacio

# 1️⃣ Agrupar per data i sumar casos i població de tots els ABS
incid_diaria_global <- totes_dades %>%
  group_by(data) %>%
  summarise(
    casos_total = sum(casos, na.rm = TRUE),
    poblacio_total = sum(poblacio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 2️⃣ Calcular incidència per 1000 habitants
  mutate(incidencia_1000 = (casos_total / poblacio_total) * 1000) %>%
  arrange(data)  # ordenar per data

# 3️⃣ Revisar el resultat
glimpse(incid_diaria_global)
head(incid_diaria_global)

# 4️⃣ Guardar com CSV
write_csv(incid_diaria_global, "incidencia_diaria_global.csv")

cat("✅ Incidència diària global calculada i guardada!\n")





**************************


library(tidyverse)

# 1️⃣ Llegir el dataset epidemiològic combinat
totes_dades <- read_csv("dades_combinades.csv", col_types = cols()) %>%
  mutate(data = as.Date(data))  # convertir la columna de data a Date

# 2️⃣ Llegir el CSV de contaminants diaris
contaminants <- read_csv("daily.csv", col_types = cols()) %>%
  rename(data = date) %>%        # si la columna es diu 'date', la renombrem a 'data'
  mutate(data = as.Date(data))    # convertir a Date

# 3️⃣ Combinar per la columna data
dades_combinades <- left_join(contaminants, totes_dades, by = "data")  
# 🔹 left_join perquè volem conservar totes les dates dels contaminants i afegir casos si existeixen

# 4️⃣ Revisar
glimpse(dades_combinades)
head(dades_combinades)

# 5️⃣ Guardar el resultat
write_csv(dades_combinades, "epidemiologia_i_contaminants.csv")
cat("✅ Dataset combinat amb data uniforme guardat!\n")
