################################################################################
# Trabajo de Econometría: análisis y previsión de una serie temporal española
# Serie: Viajeros de transporte urbano por autobús. Nivel nacional (INE)
# Frecuencia: mensual
# Objetivo: identificación, estimación, diagnosis y previsión SARIMA
################################################################################

rm(list = ls())

# -----------------------------------------------------------------------------
# 0. Paquetes y opciones generales
# -----------------------------------------------------------------------------

paquetes <- c(
  "readxl", "zoo", "ggplot2", "forecast", "tseries", "lmtest", "nortest",
  "tsoutliers", "scales"
)

instalar_si_falta <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(paquetes, instalar_si_falta))
invisible(lapply(paquetes, library, character.only = TRUE))

# Carpeta de salida para gráficos y tablas.
dir_salida <- "salidas_econometria_transporte"
if (!dir.exists(dir_salida)) dir.create(dir_salida)

# Archivo Excel. Deja el Excel en la misma carpeta que este script.
ruta_excel <- "transporte AUTOBUS SERIE COMPLETA.xls"
if (!file.exists(ruta_excel)) {
  message("No encuentro el Excel en el directorio actual. Selecciónalo manualmente.")
  ruta_excel <- file.choose()
}

# Activar dummies COVID como tratamiento de atípicos/intervención.
# Si el profesor prefiere un ARIMA puro, cambia TRUE por FALSE.
usar_dummies_covid <- TRUE

# -----------------------------------------------------------------------------
# 1. Importación de datos y construcción de la serie
# -----------------------------------------------------------------------------

datos_raw <- readxl::read_excel(ruta_excel)

# El Excel original suele venir con las columnas Date y Data.
# Este bloque lo hace robusto aunque cambien ligeramente los nombres.
names(datos_raw)[1:2] <- c("Mes", "Unidades")
datos <- datos_raw[, c("Mes", "Unidades")]
datos$Mes <- zoo::as.yearmon(as.character(datos$Mes), format = "%Y/%m")
datos$Mes <- as.Date(datos$Mes)
datos$Unidades <- as.numeric(datos$Unidades)
datos <- datos[!is.na(datos$Mes) & !is.na(datos$Unidades), ]

# Nos quedamos con la muestra 2012:01-2023:12.
datos <- subset(datos, Mes >= as.Date("2012-01-01") & Mes <= as.Date("2023-12-01"))
stopifnot(nrow(datos) >= 100)

serie_total <- ts(datos$Unidades, start = c(2012, 1), frequency = 12)
fechas_total <- datos$Mes

# Reserva del último año para evaluar previsiones: 2023:01-2023:12.
serie_train <- window(serie_total, start = c(2012, 1), end = c(2022, 12))
serie_test  <- window(serie_total, start = c(2023, 1), end = c(2023, 12))
fechas_train <- fechas_total[fechas_total <= as.Date("2022-12-01")]
fechas_test  <- fechas_total[fechas_total >= as.Date("2023-01-01") & fechas_total <= as.Date("2023-12-01")]

cat("Observaciones totales:", length(serie_total), "\n")
cat("Observaciones de estimación:", length(serie_train), "\n")
cat("Observaciones reservadas para previsión:", length(serie_test), "\n")

# -----------------------------------------------------------------------------
# 2. Análisis gráfico inicial
# -----------------------------------------------------------------------------

grafico_serie <- ggplot(datos, aes(x = Mes, y = Unidades)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Viajeros de transporte urbano por autobús en España",
    subtitle = "Serie mensual 2012-2023. La línea discontinua marca el inicio del año reservado para previsión",
    x = "Año",
    y = "Miles de viajeros"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(dir_salida, "01_serie_total_2012_2023.png"), grafico_serie, width = 10, height = 5, dpi = 300)
print(grafico_serie)

png(file.path(dir_salida, "02_descomposicion_aditiva_train.png"), width = 1200, height = 800)
plot(decompose(serie_train, type = "additive"))
dev.off()

png(file.path(dir_salida, "03_descomposicion_multiplicativa_train.png"), width = 1200, height = 800)
plot(decompose(serie_train, type = "multiplicative"))
dev.off()

# -----------------------------------------------------------------------------
# 3. Transformaciones e identificación
# -----------------------------------------------------------------------------

# Como la serie es estrictamente positiva, se analiza el logaritmo.
# La estimación posterior se hace con lambda = 0, equivalente a logaritmos,
# y las previsiones se devuelven en la escala original.
log_train <- log(serie_train)

# Diferencias recomendadas por funciones auxiliares.
D_sugerida <- forecast::nsdiffs(log_train)
log_para_ndiffs <- if (D_sugerida > 0) diff(log_train, lag = 12, differences = D_sugerida) else log_train
d_sugerida <- forecast::ndiffs(log_para_ndiffs)
cat("Diferencias estacionales sugeridas:", D_sugerida, "\n")
cat("Diferencias regulares sugeridas tras estacional:", d_sugerida, "\n")

# Para seguir el protocolo Box-Jenkins mensual usamos una diferencia regular y una estacional.
log_d1 <- diff(log_train, lag = 1, differences = 1)
log_d1_d12 <- diff(log_d1, lag = 12, differences = 1)

png(file.path(dir_salida, "04_log_serie_train.png"), width = 1200, height = 600)
plot(log_train, main = "Logaritmo de la serie: muestra de estimación", ylab = "log(viajeros)", xlab = "Tiempo")
dev.off()

png(file.path(dir_salida, "05_serie_estacionaria_d1_d12.png"), width = 1200, height = 600)
plot(log_d1_d12, main = "Serie transformada: diferencia regular y estacional del logaritmo", ylab = "D12 D log(y_t)", xlab = "Tiempo")
abline(h = 0, lty = 2)
dev.off()

png(file.path(dir_salida, "06_fas_fap_serie_estacionaria.png"), width = 1200, height = 700)
par(mfrow = c(1, 2))
acf(log_d1_d12, lag.max = 48, main = "FAS: D12 D log(y_t)", ylab = "FAS")
pacf(log_d1_d12, lag.max = 48, main = "FAP: D12 D log(y_t)", ylab = "FAP")
par(mfrow = c(1, 1))
dev.off()

# Contraste ADF sobre la serie transformada.
adf_resultado <- tseries::adf.test(log_d1_d12)
print(adf_resultado)
capture.output(adf_resultado, file = file.path(dir_salida, "contraste_adf_serie_estacionaria.txt"))

# Detección automática de atípicos en la muestra de estimación.
outliers_train <- tryCatch(
  tsoutliers::tso(log_train, types = c("AO", "LS", "TC"), maxit.iloop = 10),
  error = function(e) e
)
capture.output(print(outliers_train), file = file.path(dir_salida, "deteccion_outliers_train.txt"))

# -----------------------------------------------------------------------------
# 4. Variables de intervención para COVID-19
# -----------------------------------------------------------------------------

crear_xreg <- function(fechas) {
  data.frame(
    D_COVID_2020M03_2020M06 = as.integer(fechas >= as.Date("2020-03-01") & fechas <= as.Date("2020-06-01")),
    D_RECUP_2020M07_2021M12 = as.integer(fechas >= as.Date("2020-07-01") & fechas <= as.Date("2021-12-01"))
  )
}

xreg_train <- if (usar_dummies_covid) as.matrix(crear_xreg(fechas_train)) else NULL
xreg_test  <- if (usar_dummies_covid) as.matrix(crear_xreg(fechas_test)) else NULL
xreg_total <- if (usar_dummies_covid) as.matrix(crear_xreg(fechas_total)) else NULL

# -----------------------------------------------------------------------------
# 5. Estimación de modelos candidatos
# -----------------------------------------------------------------------------

# Modelos candidatos inspirados por FAS/FAP y por el trabajo previo.
especificaciones <- list(
  "M1_SARIMA_211_011" = list(order = c(2, 1, 1), seasonal = c(0, 1, 1), etiqueta = "SARIMA(2,1,1)(0,1,1)[12]"),
  "M2_SARIMA_210_210" = list(order = c(2, 1, 0), seasonal = c(2, 1, 0), etiqueta = "SARIMA(2,1,0)(2,1,0)[12]"),
  "M3_SARIMA_111_011" = list(order = c(1, 1, 1), seasonal = c(0, 1, 1), etiqueta = "SARIMA(1,1,1)(0,1,1)[12]"),
  "M4_SARIMA_210_011" = list(order = c(2, 1, 0), seasonal = c(0, 1, 1), etiqueta = "SARIMA(2,1,0)(0,1,1)[12]")
)

ajustar_modelo <- function(y, spec, xreg = NULL) {
  tryCatch(
    forecast::Arima(
      y,
      order = spec$order,
      seasonal = list(order = spec$seasonal, period = 12),
      method = "ML",
      lambda = 0,
      biasadj = TRUE,
      xreg = xreg
    ),
    error = function(e) e
  )
}

modelos <- lapply(especificaciones, function(spec) ajustar_modelo(serie_train, spec, xreg_train))
modelos <- modelos[!vapply(modelos, inherits, logical(1), what = "error")]

# Auto-ARIMA como referencia adicional, no necesariamente como modelo final.
auto_modelo <- tryCatch(
  forecast::auto.arima(
    serie_train,
    seasonal = TRUE,
    lambda = 0,
    biasadj = TRUE,
    xreg = xreg_train,
    allowdrift = FALSE,
    stepwise = FALSE,
    approximation = FALSE,
    trace = TRUE
  ),
  error = function(e) e
)
capture.output(print(auto_modelo), file = file.path(dir_salida, "auto_arima_referencia.txt"))

# Tabla de coeficientes.
tabla_coeficientes <- function(modelo, nombre, etiqueta) {
  coefs <- coef(modelo)
  if (length(coefs) == 0) return(data.frame())
  se <- sqrt(diag(modelo$var.coef))
  z <- coefs / se
  pvalor <- 2 * (1 - pnorm(abs(z)))
  data.frame(
    Modelo = nombre,
    Especificacion = etiqueta,
    Coeficiente = names(coefs),
    Estimacion = as.numeric(coefs),
    Error_tipico = as.numeric(se),
    Estadistico_z = as.numeric(z),
    P_valor = as.numeric(pvalor),
    Significativo_5 = ifelse(pvalor < 0.05, "Sí", "No"),
    row.names = NULL
  )
}

tablas_coef <- list()
for (nm in names(modelos)) {
  tablas_coef[[nm]] <- tabla_coeficientes(modelos[[nm]], nm, especificaciones[[nm]]$etiqueta)
}
tabla_estimacion <- do.call(rbind, tablas_coef)
write.csv(tabla_estimacion, file.path(dir_salida, "tabla_01_estimacion_coeficientes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(tabla_estimacion)

# -----------------------------------------------------------------------------
# 6. Diagnosis de los modelos candidatos
# -----------------------------------------------------------------------------

safe_pvalue <- function(expr) {
  out <- tryCatch(expr, error = function(e) NA)
  if (is.list(out) && !is.null(out$p.value)) return(out$p.value)
  return(NA_real_)
}

min_root <- function(modelo, tipo = c("ar", "ma")) {
  tipo <- match.arg(tipo)
  r <- tryCatch({
    if (tipo == "ar") forecast::arroots(modelo) else forecast::maroots(modelo)
  }, error = function(e) NULL)
  if (is.null(r) || length(r) == 0) return(NA_real_)
  min(Mod(r))
}

tabla_diagnosis <- data.frame()
for (nm in names(modelos)) {
  mod <- modelos[[nm]]
  res <- residuals(mod)
  lb_p <- safe_pvalue(Box.test(res, lag = 24, type = "Ljung-Box", fitdf = length(coef(mod))))
  jb_p <- safe_pvalue(tseries::jarque.bera.test(na.omit(res)))
  bp_p <- safe_pvalue(lmtest::bptest(na.omit(res) ~ seq_along(na.omit(res))))
  tabla_diagnosis <- rbind(
    tabla_diagnosis,
    data.frame(
      Modelo = nm,
      Especificacion = especificaciones[[nm]]$etiqueta,
      AIC = AIC(mod),
      BIC = BIC(mod),
      Ljung_Box_pvalor = lb_p,
      Residuos_ruido_blanco_5 = ifelse(is.na(lb_p), NA, ifelse(lb_p > 0.05, "Sí", "No")),
      Jarque_Bera_pvalor = jb_p,
      Normalidad_residuos_5 = ifelse(is.na(jb_p), NA, ifelse(jb_p > 0.05, "Sí", "No")),
      Breusch_Pagan_pvalor = bp_p,
      Homocedasticidad_5 = ifelse(is.na(bp_p), NA, ifelse(bp_p > 0.05, "Sí", "No")),
      Min_raiz_AR = min_root(mod, "ar"),
      Min_raiz_MA = min_root(mod, "ma"),
      row.names = NULL
    )
  )
}
write.csv(tabla_diagnosis, file.path(dir_salida, "tabla_02_diagnosis_modelos.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(tabla_diagnosis)

# Gráficos de residuos de cada modelo.
for (nm in names(modelos)) {
  png(file.path(dir_salida, paste0("diagnosis_residuos_", nm, ".png")), width = 1200, height = 800)
  forecast::checkresiduals(modelos[[nm]], main = paste("Residuos -", especificaciones[[nm]]$etiqueta))
  dev.off()
}

# -----------------------------------------------------------------------------
# 7. Previsión del año reservado y comparación por ECM, EMA y EMAP
# -----------------------------------------------------------------------------

predecir <- function(modelo, h, xreg_futuro = NULL) {
  if (is.null(xreg_futuro)) forecast::forecast(modelo, h = h, level = 95)
  else forecast::forecast(modelo, h = h, level = 95, xreg = xreg_futuro)
}

previsiones_2023 <- list()
tabla_errores <- data.frame()
tabla_pred_2023 <- data.frame(Mes = fechas_test, Real = as.numeric(serie_test))

for (nm in names(modelos)) {
  fc <- predecir(modelos[[nm]], h = length(serie_test), xreg_futuro = xreg_test)
  previsiones_2023[[nm]] <- fc
  pred <- as.numeric(fc$mean)
  error <- as.numeric(serie_test) - pred
  tabla_pred_2023[[nm]] <- pred
  tabla_errores <- rbind(
    tabla_errores,
    data.frame(
      Modelo = nm,
      Especificacion = especificaciones[[nm]]$etiqueta,
      ECM = mean(error^2, na.rm = TRUE),
      RMSE = sqrt(mean(error^2, na.rm = TRUE)),
      EMA = mean(abs(error), na.rm = TRUE),
      EMAP = mean(abs(error / as.numeric(serie_test)), na.rm = TRUE) * 100,
      row.names = NULL
    )
  )
}

tabla_errores <- tabla_errores[order(tabla_errores$ECM), ]
write.csv(tabla_errores, file.path(dir_salida, "tabla_03_errores_prevision_2023.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tabla_pred_2023, file.path(dir_salida, "tabla_04_previsiones_2023.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(tabla_errores)
print(tabla_pred_2023)

# Gráfico comparativo de previsiones en 2023.
png(file.path(dir_salida, "07_comparacion_previsiones_2023.png"), width = 1400, height = 800)
matplot(
  fechas_test,
  tabla_pred_2023[, -1],
  type = "l",
  lty = 1,
  lwd = c(3, rep(2, ncol(tabla_pred_2023) - 2)),
  xlab = "Mes",
  ylab = "Miles de viajeros",
  main = "Previsión del último año reservado: 2023"
)
legend("topleft", legend = names(tabla_pred_2023)[-1], lty = 1, lwd = 2, bty = "n", cex = 0.85)
dev.off()

# -----------------------------------------------------------------------------
# 8. Modelo final y previsión genuina para 2024
# -----------------------------------------------------------------------------

mejor_modelo_nombre <- tabla_errores$Modelo[1]
mejor_spec <- especificaciones[[mejor_modelo_nombre]]
cat("Modelo elegido por menor ECM en 2023:", mejor_modelo_nombre, "-", mejor_spec$etiqueta, "\n")

modelo_final <- ajustar_modelo(serie_total, mejor_spec, xreg_total)

# Fechas futuras 2024 y dummies futuras en cero salvo que se defina otra intervención.
fechas_2024 <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
xreg_2024 <- if (usar_dummies_covid) as.matrix(crear_xreg(fechas_2024)) else NULL
forecast_2024 <- predecir(modelo_final, h = 12, xreg_futuro = xreg_2024)

tabla_2024 <- data.frame(
  Mes = fechas_2024,
  Prevision = as.numeric(forecast_2024$mean),
  IC95_inferior = as.numeric(forecast_2024$lower[, 1]),
  IC95_superior = as.numeric(forecast_2024$upper[, 1])
)
write.csv(tabla_2024, file.path(dir_salida, "tabla_05_prevision_genuina_2024.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(tabla_2024)

png(file.path(dir_salida, "08_prevision_genuina_2024.png"), width = 1400, height = 800)
plot(forecast_2024, main = paste("Previsión genuina 2024 -", mejor_spec$etiqueta), xlab = "Año", ylab = "Miles de viajeros")
dev.off()

# Diagnosis final del modelo reestimado con toda la muestra.
png(file.path(dir_salida, "09_diagnosis_modelo_final.png"), width = 1200, height = 800)
forecast::checkresiduals(modelo_final, main = paste("Residuos modelo final -", mejor_spec$etiqueta))
dev.off()

capture.output(
  list(
    modelo_elegido = mejor_modelo_nombre,
    especificacion = mejor_spec$etiqueta,
    usa_dummies_covid = usar_dummies_covid,
    resumen_modelo_final = summary(modelo_final),
    errores_2023 = tabla_errores,
    prevision_2024 = tabla_2024
  ),
  file = file.path(dir_salida, "resumen_final_modelo.txt")
)

cat("\nProceso terminado. Revisa la carpeta:", dir_salida, "\n")
cat("Archivos clave: tabla_03_errores_prevision_2023.csv, tabla_05_prevision_genuina_2024.csv y resumen_final_modelo.txt\n")
