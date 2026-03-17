# =========================================================

# FIGURES DLNM MULTI-CONTAMINANT

# =========================================================

# REQUISITS:

# - data_clean preparat

# - variables: no2, pm10, temp_mean

library(dlnm)
library(ggplot2)
library(patchwork)
library(viridis)

lag_max <- 7

# -------------------------------

# 1. CROSSBASIS PER CADA CONTAMINANT

# -------------------------------

cb_no2 <- crossbasis(data_clean$no2, lag=lag_max,
argvar=list(fun="ns", df=3),
arglag=list(fun="ns", df=3))

cb_pm10 <- crossbasis(data_clean$pm10, lag=lag_max,
argvar=list(fun="ns", df=3),
arglag=list(fun="ns", df=3))

cb_temp <- crossbasis(data_clean$temp_mean, lag=lag_max,
argvar=list(fun="ns", df=4),
arglag=list(fun="ns", df=3))

# -------------------------------

# 2. MODEL MULTI-CONTAMINANT

# -------------------------------

model_multi <- glm(
casos_total ~ cb_no2 + cb_pm10 + cb_temp +
ns(time, df = 7*length(unique(data_clean$year))) +
dow,
family = quasipoisson(link="log"),
offset = log(poblacio_total),
data = data_clean
)

summary(model_multi)

# -------------------------------

# 3. PREDICCIONS

# -------------------------------

pred_no2 <- crosspred(cb_no2, model_multi,
cen = median(data_clean$no2, na.rm=TRUE))

pred_pm10 <- crosspred(cb_pm10, model_multi,
cen = median(data_clean$pm10, na.rm=TRUE))

pred_temp <- crosspred(cb_temp, model_multi,
cen = median(data_clean$temp_mean, na.rm=TRUE))

# -------------------------------

# 4. EFECTES ACUMULATS COMPARATS

# -------------------------------

df_no2 <- data.frame(x=pred_no2$predvar,
rr=pred_no2$allRRfit,
low=pred_no2$allRRlow,
high=pred_no2$allRRhigh,
var="NO2")

df_pm10 <- data.frame(x=pred_pm10$predvar,
rr=pred_pm10$allRRfit,
low=pred_pm10$allRRlow,
high=pred_pm10$allRRhigh,
var="PM10")

df_temp <- data.frame(x=pred_temp$predvar,
rr=pred_temp$allRRfit,
low=pred_temp$allRRlow,
high=pred_temp$allRRhigh,
var="Temperatura")

df_all <- rbind(df_no2, df_pm10, df_temp)

# Figura combinada (clau)

ggplot(df_all, aes(x=x, y=rr, color=var, fill=var)) +
geom_line() +
geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, color=NA) +
theme_classic() +
labs(title="Comparació multi-contaminant",
x="Exposició",
y="Risc Relatiu")

# -------------------------------

# 5. HEATMAP PER CADA CONTAMINANT

# -------------------------------

make_heatmap <- function(pred, name){

grid <- expand.grid(
x = pred$predvar,
lag = 0:lag_max
)

grid$rr <- as.vector(pred$matRRfit)

ggplot(grid, aes(x=x, y=lag, fill=rr)) +
geom_tile() +
scale_fill_viridis_c() +
theme_minimal() +
labs(title=paste("DLNM:", name),
x=name, y="Lag")
}

p_no2  <- make_heatmap(pred_no2, "NO2")
p_pm10 <- make_heatmap(pred_pm10, "PM10")
p_temp <- make_heatmap(pred_temp, "Temperatura")

# Mostrar junts

p_no2 / p_pm10 / p_temp

# -------------------------------

# 6. SLICES COMPARATIUS

# -------------------------------

# Percentil alt

p75_no2 <- quantile(data_clean$no2, 0.75, na.rm=TRUE)
p75_pm10 <- quantile(data_clean$pm10, 0.75, na.rm=TRUE)

plot(pred_no2, "slices", var=p75_no2, col="red",
main="NO2 alt")

plot(pred_pm10, "slices", var=p75_pm10, col="blue",
main="PM10 alt")

# -------------------------------

# 7. FIGURA FINAL TIPUS ARTICLE

# -------------------------------

p1 <- ggplot(df_no2, aes(x, rr)) + geom_line() + ggtitle("NO2")
p2 <- ggplot(df_pm10, aes(x, rr)) + geom_line() + ggtitle("PM10")
p3 <- ggplot(df_temp, aes(x, rr)) + geom_line() + ggtitle("Temp")

(p1 | p2 | p3)

# -------------------------------

# 8. EXPORTACIÓ

# -------------------------------

ggsave("multi_contaminant.png", width=10, height=6)

# =========================================================

# FI

# =========================================================
