# =========================================================

# PIPELINE COMPLET: CSV → DLNM → FIGURES NATURE/LANCET

# =========================================================

# -------------------------------

# 0. LLIBRERIES

# -------------------------------

# install.packages(c("tidyverse","lubridate","splines","dlnm","mgcv","ggplot2","patchwork","scales","viridis"))

library(tidyverse)
library(lubridate)
library(splines)
library(dlnm)
library(mgcv)
library(ggplot2)
library(patchwork)
library(scales)
library(viridis)

# -------------------------------

# 1. LLEGIR I NETEJAR CSV

# -------------------------------

data <- read.csv("dades.csv", na.strings=c("NA",""," "), stringsAsFactors=FALSE)
data$date <- as.Date(data$date)
data <- data %>% mutate(across(-date, as.numeric)) %>% arrange(date)

# Variables temporals

data <- data %>% mutate(
time = as.numeric(date),
dow = factor(weekdays(date)),
year = year(date)
)

# Selecció de dades completes

data_clean <- data %>% drop_na(casos_total, poblacio_total, temp_mean, no2, pm10)

cat("Files després neteja:", nrow(data_clean), "\n")

# -------------------------------

# 2. DEFINIR CROSSBASIS

# -------------------------------

lag_max <- 7

cb_no2 <- crossbasis(data_clean$no2, lag=lag_max, argvar=list(fun="ns", df=3), arglag=list(fun="ns", df=3))
cb_pm10 <- crossbasis(data_clean$pm10, lag=lag_max, argvar=list(fun="ns", df=3), arglag=list(fun="ns", df=3))
cb_temp <- crossbasis(data_clean$temp_mean, lag=lag_max, argvar=list(fun="ns", df=4), arglag=list(fun="ns", df=3))

# -------------------------------

# 3. AJUSTAR MODEL MULTI-CONTAMINANT

# -------------------------------

nyears <- length(unique(data_clean$year))
model_multi <- glm(
casos_total ~ cb_no2 + cb_pm10 + cb_temp + ns(time, df=7*nyears) + dow,
family=quasipoisson(link="log"),
offset=log(poblacio_total),
data=data_clean
)

summary(model_multi)

# -------------------------------

# 4. PREDICCIONS DLNM

# -------------------------------

pred_no2  <- crosspred(cb_no2, model_multi, cen=median(data_clean$no2, na.rm=TRUE))
pred_pm10 <- crosspred(cb_pm10, model_multi, cen=median(data_clean$pm10, na.rm=TRUE))
pred_temp <- crosspred(cb_temp, model_multi, cen=median(data_clean$temp_mean, na.rm=TRUE))

# -------------------------------

# 5. PREPARAR DATAFRAME PER FIGURES

# -------------------------------

df_no2 <- data.frame(x=pred_no2$predvar, rr=pred_no2$allRRfit, low=pred_no2$allRRlow, high=pred_no2$allRRhigh, var="NO2")
df_pm10 <- data.frame(x=pred_pm10$predvar, rr=pred_pm10$allRRfit, low=pred_pm10$allRRlow, high=pred_pm10$allRRhigh, var="PM10")
df_temp <- data.frame(x=pred_temp$predvar, rr=pred_temp$allRRfit, low=pred_temp$allRRlow, high=pred_temp$allRRhigh, var="Temp")
df_all <- rbind(df_no2, df_pm10, df_temp)

# -------------------------------

# 6. DEFINIR TEMA PROFESSIONAL

# -------------------------------

theme_pub <- function(){
theme_classic(base_size = 14) +
theme(
text = element_text(family = "sans"),
plot.title = element_text(face = "bold", size=16),
axis.title = element_text(size=14),
axis.text = element_text(size=12),
legend.position = "top",
legend.title = element_blank(),
panel.grid = element_blank()
)
}

# Colors sobris

col_no2  <- "#1b9e77"
col_pm10 <- "#d95f02"
col_temp <- "#7570b3"

# -------------------------------

# 7. FIGURA 1 — EXPOSICIÓ-RESPOSTA MULTI

# -------------------------------

p_exp <- ggplot(df_all, aes(x=x, y=rr, color=var, fill=var)) +
geom_line(size=1) +
geom_ribbon(aes(ymin=low, ymax=high), alpha=0.15, color=NA) +
scale_color_manual(values=c(col_no2, col_pm10, col_temp)) +
scale_fill_manual(values=c(col_no2, col_pm10, col_temp)) +
geom_hline(yintercept=1, linetype="dashed") +
labs(title="Exposure–response relationships", x="Exposure", y="Relative Risk (RR)") +
theme_pub()

# -------------------------------

# 8. FIGURA 2 — HEATMAP DLNM (NO2)

# -------------------------------

grid_no2 <- expand.grid(x=pred_no2$predvar, lag=0:lag_max)
grid_no2$rr <- as.vector(pred_no2$matRRfit)
p_heat <- ggplot(grid_no2, aes(x=x, y=lag, fill=rr)) +
geom_tile() +
scale_fill_gradientn(colours=c("navy","lightblue","yellow","red"), values=rescale(c(min(grid_no2$rr),1,max(grid_no2$rr)))) +
labs(title="Lag-response surface (NO2)", x="NO2", y="Lag (days)", fill="RR") +
theme_pub()

# -------------------------------

# 9. FIGURA 3 — SLICES (NO2 alt)

# -------------------------------

p75 <- quantile(data_clean$no2, 0.75, na.rm=TRUE)
slice <- data.frame(lag=0:lag_max, rr=pred_no2$matRRfit[which.min(abs(pred_no2$predvar-p75)),])
p_slice <- ggplot(slice, aes(x=lag, y=rr)) +
geom_line(color=col_no2, size=1) +
geom_point(color=col_no2) +
geom_hline(yintercept=1, linetype="dashed") +
labs(title="Lag effect at high NO2", x="Lag (days)", y="RR") +
theme_pub()

# -------------------------------

# 10. FIGURA FINAL COMBINADA

# -------------------------------

final_plot <- (p_exp) / (p_heat | p_slice)
final_plot

# -------------------------------

# 11. EXPORTACIÓ

# -------------------------------

ggsave("figure_nature_style.png", final_plot, width=10, height=8, dpi=300)
ggsave("figure_nature_style.pdf", final_plot, width=10, height=8)

# =========================================================

# FI PIPELINE COMPLET

# =========================================================
