# =========================================================

# ANÀLISI EPIDEMIOLÒGICA AMBIENTAL EN R

# =========================================================

# -------------------------------

# 0. LLIBRERIES

# -------------------------------

# Instal·la si cal:

# install.packages(c("tidyverse","lubridate","mgcv","splines","broom"))

library(tidyverse)
library(lubridate)
library(mgcv)
library(splines)
library(broom)

# -------------------------------

# 1. PREPARACIÓ DE DADES

# -------------------------------

# Llegeix el fitxer CSV (canvia el nom si cal)

data <- read.csv("dades.csv", stringsAsFactors = FALSE)

# Convertir columna de data

data$date <- as.Date(data$date)

# Convertir "NA" text a NA real

data[data == "NA"] <- NA

# Convertir totes les variables (excepte data) a numèriques

data <- data %>%
mutate(across(-date, as.numeric))

# Neteja bàsica: eliminar files amb NA en variables clau

data_clean <- data %>%
drop_na(casos_total, poblacio_total, temp_mean, no2, o3, pm10)

# Crear incidència si vols recalcular-la

data_clean <- data_clean %>%
mutate(incidencia_calc = casos_total / poblacio_total * 1000)

# -------------------------------

# 2. ANÀLISI DESCRIPTIVA

# -------------------------------

# Resum estadístic

summary(data_clean)

# Matriu de correlació

cor_matrix <- data_clean %>%
select(temp_mean, no2, o3, pm10, so2, casos_total) %>%
cor(use = "complete.obs")

print(cor_matrix)

# -------------------------------

# 3. VISUALITZACIÓ

# -------------------------------

# Evolució temporal de casos

ggplot(data_clean, aes(x = date, y = casos_total)) +
geom_line(color = "steelblue") +
theme_minimal() +
labs(title = "Casos totals al llarg del temps")

# Relació NO2 - casos

ggplot(data_clean, aes(x = no2, y = casos_total)) +
geom_point() +
geom_smooth(method = "lm", color = "red") +
theme_minimal() +
labs(title = "Relació NO2 i casos")

# -------------------------------

# 4. MODEL DE REGRESSIÓ POISSON

# -------------------------------

# Model per dades de recompte amb offset poblacional

model_poisson <- glm(
casos_total ~ temp_mean + no2 + o3 + pm10 + so2,
family = poisson(link = "log"),
offset = log(poblacio_total),
data = data_clean
)

summary(model_poisson)

# -------------------------------

# 5. MODEL GAM (NO LINEAL)

# -------------------------------

# Captura relacions no lineals amb splines

model_gam <- gam(
casos_total ~ s(temp_mean) + s(no2) + s(o3) + s(pm10),
family = poisson(link = "log"),
offset = log(poblacio_total),
data = data_clean
)

summary(model_gam)

# Visualitzar efectes no lineals

plot(model_gam, pages = 1)

# -------------------------------

# 6. EFECTES RETARDATS (LAGS)

# -------------------------------

# Ordenar per data i crear retard (lag)

data_clean <- data_clean %>%
arrange(date) %>%
mutate(
no2_lag1 = lag(no2, 1),
pm10_lag1 = lag(pm10, 1)
)

# Model amb retard

model_lag <- glm(
casos_total ~ no2 + no2_lag1 + temp_mean,
family = poisson(link = "log"),
offset = log(poblacio_total),
data = data_clean
)

summary(model_lag)

# -------------------------------

# 7. INTERPRETACIÓ (RISC RELATIU)

# -------------------------------

# Convertir coeficients a risc relatiu (RR)

rr <- exp(coef(model_poisson))
print(rr)

# Interval de confiança del RR

rr_ci <- exp(confint(model_poisson))
print(rr_ci)

# -------------------------------

# 8. EXTENSIONS AVANÇADES

# -------------------------------

# 8.1 Control de tendència temporal (important!)

model_time <- glm(
casos_total ~ temp_mean + no2 + ns(as.numeric(date), df = 4),
family = poisson(link = "log"),
offset = log(poblacio_total),
data = data_clean
)

summary(model_time)

# 8.2 Estacionalitat (mes)

data_clean$month <- month(data_clean$date)

model_season <- glm(
casos_total ~ temp_mean + no2 + factor(month),
family = poisson(link = "log"),
offset = log(poblacio_total),
data = data_clean
)

summary(model_season)

# 8.3 Diagnòstic simple del model

par(mfrow = c(2,2))
plot(model_poisson)

# =========================================================

# FI DE L'ANÀLISI

# =========================================================
