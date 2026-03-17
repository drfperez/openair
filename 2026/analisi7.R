# =========================================================

# FIGURES ESTIL THE LANCET / NATURE (DLNM)

# =========================================================

# -------------------------------

# 0. LLIBRERIES

# -------------------------------

# install.packages(c("ggplot2","dlnm","patchwork","scales"))

library(ggplot2)
library(dlnm)
library(patchwork)
library(scales)

# -------------------------------

# 1. TEMA PROFESSIONAL

# -------------------------------

theme_pub <- function(){
theme_classic(base_size = 14) +
theme(
text = element_text(family = "sans"),
plot.title = element_text(face = "bold", size = 16),
axis.title = element_text(size = 14),
axis.text = element_text(size = 12),
legend.position = "top",
legend.title = element_blank(),
panel.grid = element_blank()
)
}

# -------------------------------

# 2. COLORS SOBRIS (ESTIL NATURE)

# -------------------------------

col_no2  <- "#1b9e77"
col_pm10 <- "#d95f02"
col_temp <- "#7570b3"

# -------------------------------

# 3. DATAFRAME DLNM (EXPOSICIÓ-RESPOSTA)

# -------------------------------

df_no2 <- data.frame(
x = pred_no2$predvar,
rr = pred_no2$allRRfit,
low = pred_no2$allRRlow,
high = pred_no2$allRRhigh,
var = "NO2"
)

df_pm10 <- data.frame(
x = pred_pm10$predvar,
rr = pred_pm10$allRRfit,
low = pred_pm10$allRRlow,
high = pred_pm10$allRRhigh,
var = "PM10"
)

df_temp <- data.frame(
x = pred_temp$predvar,
rr = pred_temp$allRRfit,
low = pred_temp$allRRlow,
high = pred_temp$allRRlow,
var = "Temp"
)

df_all <- rbind(df_no2, df_pm10, df_temp)

# -------------------------------

# 4. FIGURA 1 — EXPOSICIÓ-RESPOSTA

# -------------------------------

p_exp <- ggplot(df_all, aes(x = x, y = rr, color = var)) +
geom_line(size = 1) +
geom_ribbon(aes(ymin = low, ymax = high, fill = var),
alpha = 0.15, color = NA) +
scale_color_manual(values = c(col_no2, col_pm10, col_temp)) +
scale_fill_manual(values = c(col_no2, col_pm10, col_temp)) +
geom_hline(yintercept = 1, linetype = "dashed") +
labs(
title = "Exposure–response relationships",
x = "Exposure",
y = "Relative Risk (RR)"
) +
theme_pub()

# -------------------------------

# 5. FIGURA 2 — HEATMAP DLNM (NO2)

# -------------------------------

grid_no2 <- expand.grid(
x = pred_no2$predvar,
lag = 0:lag_max
)

grid_no2$rr <- as.vector(pred_no2$matRRfit)

p_heat <- ggplot(grid_no2, aes(x = x, y = lag, fill = rr)) +
geom_tile() +
scale_fill_gradientn(
colours = c("navy","lightblue","yellow","red"),
values = rescale(c(min(grid_no2$rr),
1,
max(grid_no2$rr)))
) +
labs(
title = "Lag-response surface (NO2)",
x = "NO2",
y = "Lag (days)",
fill = "RR"
) +
theme_pub()

# -------------------------------

# 6. FIGURA 3 — SLICES NETS

# -------------------------------

p75 <- quantile(data_clean$no2, 0.75, na.rm = TRUE)

# extreure slice manual

slice <- data.frame(
lag = 0:lag_max,
rr = pred_no2$matRRfit[which.min(abs(pred_no2$predvar - p75)),]
)

p_slice <- ggplot(slice, aes(x = lag, y = rr)) +
geom_line(color = col_no2, size = 1) +
geom_point(color = col_no2) +
geom_hline(yintercept = 1, linetype = "dashed") +
labs(
title = "Lag effect at high NO2",
x = "Lag (days)",
y = "RR"
) +
theme_pub()

# -------------------------------

# 7. FIGURA COMBINADA (TIPUS ARTICLE)

# -------------------------------

final_plot <- (p_exp) / (p_heat | p_slice)

final_plot

# -------------------------------

# 8. EXPORTACIÓ (ALTA QUALITAT)

# -------------------------------

ggsave("figure_lancet_style.png",
final_plot,
width = 10,
height = 8,
dpi = 300)

# Per publicació → millor PDF

ggsave("figure_lancet_style.pdf",
final_plot,
width = 10,
height = 8)

# =========================================================

# FI

# =========================================================
