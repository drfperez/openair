
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

# =========================================================
# COMBINAR CONTAMINANTS + EPIDEMIOLOGIA I FILTRAR PER DATES
# =========================================================

library(tidyverse)

# 1️⃣ Assegurar que les columnes de data tenen el mateix nom i format
mitjanes_diaries <- mitjanes_diaries %>%
  rename(date = dia) %>%              # canviem 'dia' a 'date'
  mutate(data = as.Date(data))

incid_diaria_globatl <- incid_diaria_global %>%
  mutate(date = as.Date(date))

# 2️⃣ Combinar per data
csivic <- left_join(mitjanes_diaries, totes_dades, by = "data")

# 3️⃣ Guardar dataset complet
write_csv(csivic, "csivic.csv")

# 4️⃣ Filtrar període 2012-01-01 a 2025-12-31
csivic1225 <- csivic %>%
  filter(data >= as.Date("2012-01-01") & data <= as.Date("2025-12-31"))

# 5️⃣ (Opcional) Guardar també el dataset filtrat
write_csv(csivic1225, "csivic_2012_2025.csv")

# 6️⃣ Missatge final
cat("✅ Dataset combinat creat (csivic)\n",
    "✅ Dataset filtrat creat (csivic1225)\n",
    "Files totals csivic:", nrow(csivic), "\n",
    "Files totals csivic1225:", nrow(csivic1225), "\n")




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


**************************

ciutatwide<-read.csv("processed_data_wide.csv")
View(ciutatwide)


******
# =========================================================
# CALCULAR MITJANES DIÀRIES DE CIUTATWIDE.CSV
# =========================================================

library(tidyverse)
library(lubridate)  # per treballar amb dates i hores

# 1️⃣ Llegir el CSV
ciutat <- read_csv("processed_data_wide.csv", col_types = cols())  # manté tipus automàtics

# 2️⃣ Convertir la columna 'date' a POSIXct (data i hora)
ciutat <- ciutat %>%
  mutate(date = ymd_hms(date))  # utilitza lubridate per assegurar el format correcte

# 3️⃣ Crear columna 'dia' només amb la data (sense hora)
ciutat <- ciutat %>%
  mutate(dia = as.Date(date))

# 4️⃣ Seleccionar només les columnes de contaminants
contaminants <- ciutat %>%
  select(dia, h2s, hcnm, hct, no, no2, nox, o3, pm10, pm2.5, so2)

# 5️⃣ Agrupar per dia i calcular la mitjana ignorar NA
mitjanes_diaries <- contaminants %>%
  group_by(dia) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  arrange(dia)

# 6️⃣ Veure les primeres files
head(mitjanes_diaries)

# 7️⃣ Guardar el resultat com CSV
write_csv(mitjanes_diaries, "mitjanes_diaries.csv")
cat("✅ Mitjanes diàries calculades i guardades en mitjanes_diaries.csv\n")




# =========================================================
# COMBINAR CONTAMINANTS + EPIDEMIOLOGIA I FILTRAR PER DATES
# =========================================================

library(tidyverse)

# 1️⃣ Assegurar que les columnes de data tenen el mateix nom i format
mitjanes_diaries <- mitjanes_diaries %>%
  rename(data = dia) %>%              # canviem 'dia' a 'data'
  mutate(data = as.Date(data))

incid_diaries_globals <- incid_diaries_globals %>%
  mutate(data = as.Date(data))

# 2️⃣ Combinar per data
csivic <- left_join(mitjanes_diaries, totes_dades, by = "data")

# 3️⃣ Guardar dataset complet
write_csv(csivic, "csivic.csv")

# 4️⃣ Filtrar període 2012-01-01 a 2025-12-31
csivic1225 <- csivic %>%
  filter(data >= as.Date("2012-01-01") & data <= as.Date("2025-12-31"))

# 5️⃣ (Opcional) Guardar també el dataset filtrat
write_csv(csivic1225, "csivic_2012_2025.csv")

# 6️⃣ Missatge final
cat("✅ Dataset combinat creat (csivic)\n",
    "✅ Dataset filtrat creat (csivic1225)\n",
    "Files totals csivic:", nrow(csivic), "\n",
    "Files totals csivic1225:", nrow(csivic1225), "\n")


library(tidyverse)
library(lubridate)

# Suposem que ja tens el dataframe 'mitjanes_diaries'

# 1️⃣ Convertir data a Date
mitjanes_diaries <- mitjanes_diaries %>%
  mutate(data = as.Date(data))

# 2️⃣ Renombrar columna 'data' → 'date'
mitjanes_diaries <- mitjanes_diaries %>%
  rename(date = data)

# 3️⃣ Comprovació ràpida
glimpse(mitjanes_diaries)
head(mitjanes_diaries)


> x4 <- x4 %>%
+     mutate(date = as.Date(date, format = "%Y-%m-%d"))

merged_df <- x4 %>%
  full_join(incid_diaria_global, by = "date") %>%
  full_join(mitjanes_diaries, by = "date")

# Veure el resultat
head(merged_df)

library(tidyverse)
library(lubridate)

# ⚡ Suposem que ja tens els datasets:
# x4 (meteorologia), mitjanes_diaries (contaminants), incid_diaria_global (epidemiologia)
# i que tots tenen la columna 'date' en classe Date.

# 1️⃣ Combinar tots en un únic dataframe
merged_df <- x4 %>%
  full_join(incid_diaria_global, by = "date") %>%
  full_join(mitjanes_diaries, by = "date")

# 2️⃣ Filtrar per període 2012-01-01 a 2025-12-31
merged_filtered <- merged_df %>%
  filter(date >= as.Date("2012-01-01") & date <= as.Date("2025-12-31"))

# 3️⃣ Guardar en CSV únic
write_csv(merged_filtered, "csivic_2012_2025_full.csv")

# 4️⃣ Missatge de confirmació
cat("✅ CSV únic creat amb dades de 2012-01-01 a 2025-12-31\n",
    "Files totals:", nrow(merged_filtered), "\n")







