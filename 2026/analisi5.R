# =========================================================

# FIGURES DLNM (LAGS + NO LINEALITAT)

# =========================================================

# REQUISIT: ja has d'haver ajustat un model amb:

# cb_no2 i model (glm amb DLNM)

# Si no, crea ràpidament:

# (pots saltar això si ja tens model)

library(dlnm)
library(splines)
library(ggplot2)

lag_max <- 7

cb_no2 <- crossbasis(
data_clean$no2,
lag = lag_max,
argvar = list(fun = "ns", df = 3),
arglag = list(fun = "ns", df = 3)
)

model <- glm(
casos_total ~ cb_no2 + ns(time, df = 7*length(unique(data_clean$year))),
family = quasipoisson(link = "log"),
offset = log(poblacio_total),
data = data_clean
)

# -------------------------------

# 1. PREDICCIONS DLNM

# -------------------------------

pred_no2 <- crosspred(
cb_no2,
model,
cen = median(data_clean$no2, na.rm = TRUE),
by = 1
)

# -------------------------------

# 2. SUPERFÍCIE 3D (CLAU)

# -------------------------------

plot(pred_no2,
xlab = "NO2",
ylab = "Lag (dies)",
zlab = "Risc Relatiu",
main = "Superfície DLNM NO2",
theta = 200,
phi = 40,
ltheta = 170)

# -------------------------------

# 3. CONTOUR (MOLT IMPORTANT)

# -------------------------------

plot(pred_no2,
"contour",
xlab = "NO2",
ylab = "Lag (dies)",
key.title = title("RR"),
main = "Contour DLNM")

# -------------------------------

# 4. EFECTE ACUMULAT

# -------------------------------

plot(pred_no2,
"overall",
xlab = "NO2",
ylab = "Risc Relatiu",
main = "Efecte acumulat")

# -------------------------------

# 5. EFECTES PER LAG (SLICES)

# -------------------------------

# Percentils interessants

p25 <- quantile(data_clean$no2, 0.25, na.rm = TRUE)
p75 <- quantile(data_clean$no2, 0.75, na.rm = TRUE)

plot(pred_no2,
"slices",
var = p75,
col = "red",
main = "Efecte retardat (NO2 alt)")

plot(pred_no2,
"slices",
var = p25,
col = "blue",
main = "Efecte retardat (NO2 baix)")

# -------------------------------

# 6. EXTRAURE DADES PER GGplot2

# -------------------------------

# Efecte acumulat en data.frame

df_overall <- data.frame(
no2 = pred_no2$predvar,
rr = pred_no2$allRRfit,
low = pred_no2$allRRlow,
high = pred_no2$allRRhigh
)

# Gràfic tipus publicació

ggplot(df_overall, aes(x = no2, y = rr)) +
geom_line(color = "black") +
geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.3) +
theme_classic() +
labs(title = "Exposició-resposta (DLNM)",
y = "Risc Relatiu")

# -------------------------------

# 7. HEATMAP 2D (PUBLICACIÓ)

# -------------------------------

# Crear grid manual

grid <- expand.grid(
no2 = pred_no2$predvar,
lag = 0:lag_max
)

grid$rr <- as.vector(pred_no2$matRRfit)

ggplot(grid, aes(x = no2, y = lag, fill = rr)) +
geom_tile() +
scale_fill_viridis_c() +
theme_minimal() +
labs(title = "Heatmap DLNM",
x = "NO2",
y = "Lag",
fill = "RR")

# -------------------------------

# 8. GUARDAR FIGURES

# -------------------------------

png("dlnm_surface.png", width=800, height=600)
plot(pred_no2)
dev.off()

ggsave("dlnm_overall.png", width = 7, height = 5)

# =========================================================

# FI FIGURES DLNM

# =========================================================
