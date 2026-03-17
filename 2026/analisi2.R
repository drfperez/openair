# =========================================================

# ANÀLISI EPIDEMIOLÒGICA AMBIENTAL AMB DLNM (R)

# =========================================================

# -------------------------------

# 0. LLIBRERIES

# -------------------------------

# Instal·la si cal:

# install.packages(c("tidyverse","lubridate","mgcv","splines","dlnm"))

library(tidyverse)
library(lubridate)
library(mgcv)
library(splines)
library(dlnm)

# -------------------------------

# 1. PREPARACIÓ DE DADES

# -------------------------------

data <- read.csv("dades.csv", stringsAsFactors = FALSE)

data$date <- as.Date(data$date)
data[data == "NA"] <- NA

data <- data %>%
mutate(across(-date, as.numeric)) %>%
arrange(date)

# Eliminar NA en variables clau

data_clean <- data %>%
drop_na(casos_total, poblacio_total, temp_mean, no2, pm10)

# -------------------------------

# 2. DEFINICIÓ DEL DLNM

# -------------------------------

# Definir màxim retard (lags)

lag_max <- 7   # pots provar 3, 7, 14 segons hipòtesi

# ---- Exemple amb NO2 ----

# Crear funció base creuada (exposició + retard)

cb_no2 <- crossbasis(
data_clean$no2,
lag = lag_max,
argvar = list(fun = "ns", df = 3),   # no lineal en exposició
arglag = list(fun = "ns", df = 3)    # no lineal en retard
)

# ---- Exemple amb temperatura ----

cb_temp <- crossbasis(
data_clean$temp_mean,
lag = lag_max,
argvar = list(fun = "ns", df = 3),
arglag = list(fun = "ns", df = 3)
)

# -------------------------------

# 3. MODEL DLNM

# -------------------------------

model_dlnm <- glm(
casos_total ~ cb_no2 + cb_temp + ns(as.numeric(date), df = 4),
family = poisson(link = "log"),
offset = log(poblacio_total),
data = data_clean
)

summary(model_dlnm)

# -------------------------------

# 4. PREDICCIONS DLNM

# -------------------------------

# Prediccions per NO2

pred_no2 <- crosspred(
cb_no2,
model_dlnm,
cen = median(data_clean$no2, na.rm = TRUE), # valor de referència
by = 1
)

# Prediccions per temperatura

pred_temp <- crosspred(
cb_temp,
model_dlnm,
cen = median(data_clean$temp_mean, na.rm = TRUE),
by = 1
)

# -------------------------------

# 5. VISUALITZACIÓ RESULTATS

# -------------------------------

# ---- Superfície 3D (exposició-retard) ----

plot(pred_no2,
xlab = "NO2",
zlab = "RR",
ylab = "Lag (dies)",
main = "Efecte NO2 (DLNM)",
theta = 200, phi = 40)

# ---- Efecte acumulat ----

plot(pred_no2, "overall",
xlab = "NO2",
ylab = "Risc Relatiu",
main = "Efecte acumulat NO2")

# ---- Slice a un retard concret ----

plot(pred_no2, "slices",
var = quantile(data_clean$no2, 0.75, na.rm = TRUE),
main = "Efecte a alt NO2")

# -------------------------------

# 6. INTERPRETACIÓ

# -------------------------------

# Risc relatiu acumulat per increments

summary(pred_no2)

# Exemple: RR per valors alts vs mediana

high_no2 <- quantile(data_clean$no2, 0.75, na.rm = TRUE)

rr_high <- crosspred(
cb_no2,
model_dlnm,
at = high_no2,
cen = median(data_clean$no2, na.rm = TRUE)
)

print(rr_high)

# -------------------------------

# 7. MODEL MULTI-CONTAMINANT

# -------------------------------

cb_pm10 <- crossbasis(
data_clean$pm10,
lag = lag_max,
argvar = list(fun = "ns", df = 3),
arglag = list(fun = "ns", df = 3)
)

model_multi <- glm(
casos_total ~ cb_no2 + cb_pm10 + cb_temp + ns(as.numeric(date), df = 4),
family = poisson(link = "log"),
offset = log(poblacio_total),
data = data_clean
)

summary(model_multi)

# -------------------------------

# 8. ANÀLISI DE SENSIBILITAT

# -------------------------------

# Canviar graus de llibertat

cb_no2_sens <- crossbasis(
data_clean$no2,
lag = lag_max,
argvar = list(fun = "ns", df = 4),
arglag = list(fun = "ns", df = 4)
)

model_sens <- glm(
casos_total ~ cb_no2_sens + ns(as.numeric(date), df = 6),
family = poisson(link = "log"),
offset = log(poblacio_total),
data = data_clean
)

summary(model_sens)

# -------------------------------

# 9. NOTES IMPORTANTS

# -------------------------------

# - DLNM permet capturar:

# * relacions no lineals

# * efectes retardats

# * efectes acumulats

# - Ajusta:

# * lag_max segons malaltia (3-21 dies típic)

# * df segons mida de mostra

# - Sempre controla:

# * tendència temporal

# * estacionalitat

# * meteorologia

# =========================================================

# FI DLNM

# =========================================================
