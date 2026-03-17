# =========================================================

# FIGURES PER SÈRIE TEMPORAL (2012–2025)

# =========================================================

# -------------------------------

# 0. LLIBRERIES

# -------------------------------

# install.packages(c("tidyverse","lubridate","scales","viridis"))

library(tidyverse)
library(lubridate)
library(scales)
library(viridis)

# -------------------------------

# 1. LLEGIR I PREPARAR DADES

# -------------------------------

data <- read.csv("dades.csv",
na.strings = c("NA",""," "),
stringsAsFactors = FALSE)

data$date <- as.Date(data$date)

data <- data %>%
mutate(across(-date, as.numeric)) %>%
arrange(date)

# Variables temporals

data <- data %>%
mutate(
year = year(date),
month = month(date),
week = isoweek(date),
yday = yday(date)
)

# -------------------------------

# 2. SÈRIE TEMPORAL DE CASOS

# -------------------------------

ggplot(data, aes(x = date, y = casos_total)) +
geom_line(color = "steelblue", alpha = 0.7) +
geom_smooth(se = FALSE, color = "red") +
theme_minimal() +
labs(title = "Casos diaris (2012–2025)",
x = "Data", y = "Casos")

# -------------------------------

# 3. CONTAMINACIÓ AL LLARG DEL TEMPS

# -------------------------------

ggplot(data, aes(x = date)) +
geom_line(aes(y = no2), color = "darkgreen") +
geom_line(aes(y = pm10), color = "orange") +
theme_minimal() +
labs(title = "Contaminants al llarg del temps",
y = "Concentració")

# -------------------------------

# 4. ESTACIONALITAT (BOXPLOT)

# -------------------------------

ggplot(data, aes(x = factor(month), y = casos_total)) +
geom_boxplot(fill = "lightblue") +
theme_minimal() +
labs(title = "Estacionalitat mensual",
x = "Mes", y = "Casos")

# -------------------------------

# 5. HEATMAP ANUAL (CALENDARI)

# -------------------------------

ggplot(data, aes(x = week, y = factor(year), fill = casos_total)) +
geom_tile() +
scale_fill_viridis(option = "C") +
theme_minimal() +
labs(title = "Heatmap anual de casos",
x = "Setmana", y = "Any")

# -------------------------------

# 6. RELACIÓ CONTAMINACIÓ - SALUT

# -------------------------------

ggplot(data, aes(x = no2, y = casos_total)) +
geom_point(alpha = 0.4) +
geom_smooth(method = "loess", color = "red") +
theme_minimal() +
labs(title = "NO2 vs Casos")

# -------------------------------

# 7. DISTRIBUCIÓ VARIABLES

# -------------------------------

# Histograma casos

ggplot(data, aes(x = casos_total)) +
geom_histogram(bins = 30, fill = "steelblue") +
theme_minimal() +
labs(title = "Distribució casos")

# Histograma NO2

ggplot(data, aes(x = no2)) +
geom_histogram(bins = 30, fill = "darkgreen") +
theme_minimal() +
labs(title = "Distribució NO2")

# -------------------------------

# 8. MITJANES ANUALS

# -------------------------------

annual <- data %>%
group_by(year) %>%
summarise(
casos = mean(casos_total, na.rm = TRUE),
no2 = mean(no2, na.rm = TRUE)
)

ggplot(annual, aes(x = year, y = casos)) +
geom_line() +
geom_point() +
theme_minimal() +
labs(title = "Mitjana anual de casos")

# -------------------------------

# 9. MULTI-PANEL (IMPORTANT)

# -------------------------------

library(patchwork)

p1 <- ggplot(data, aes(date, casos_total)) + geom_line()
p2 <- ggplot(data, aes(date, no2)) + geom_line(color="green")
p3 <- ggplot(data, aes(no2, casos_total)) + geom_point()

p1 / p2 / p3

# -------------------------------

# 10. GUARDAR FIGURES

# -------------------------------

ggsave("serie_temporal.png", width = 10, height = 5)
ggsave("heatmap.png", width = 8, height = 6)

# =========================================================

# FI

# =========================================================
