# Pronóstico GARCH(1,1) a varios pasos: cada día hábil del 5 al 30 de octubre de 2026,
# usando solo la información disponible al cierre del 2 de octubre (el corte de ARCH_GARCH.Rmd).
# Se corre una sola vez; no forma parte del workflow diario.

suppressPackageStartupMessages({
  library(quantmod)
  library(rugarch)
  library(openxlsx)
})

# ---- Parámetros ----
clave   <- "GMEXICOB.MX"
fin_rmd <- as.Date("2026-10-05")        # fecha 'to' de ARCH_GARCH.Rmd: solo datos anteriores
hasta   <- as.Date("2026-10-30")        # último día a pronosticar
archivo <- "pronostico_garch_octubre.xlsx"

# ---- Calendario bursátil (días inhábiles de la BMV, por regla) ----
pascua <- function(anio) {
  a <- anio %% 19; b <- anio %/% 100; c <- anio %% 100
  d <- b %/% 4; e <- b %% 4; f <- (b + 8) %/% 25; g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4; k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  mm <- (a + 11 * h + 22 * l) %/% 451
  mes <- (h + l - 7 * mm + 114) %/% 31
  dia <- ((h + l - 7 * mm + 114) %% 31) + 1
  as.Date(sprintf("%d-%02d-%02d", anio, mes, dia))
}

n_esimo_lunes <- function(anio, mes, n) {
  d <- seq(as.Date(sprintf("%d-%02d-01", anio, mes)), by = "day", length.out = 28)
  d[format(d, "%u") == "1"][n]
}

festivos_bmv <- function(anio) {
  fijo <- function(md) as.Date(sprintf("%d-%s", anio, md))
  p <- pascua(anio)
  f <- c(
    fijo("01-01"),               # Año Nuevo
    n_esimo_lunes(anio, 2, 1),   # Constitución
    n_esimo_lunes(anio, 3, 3),   # Natalicio de Benito Juárez
    p - 3, p - 2,                # Jueves y Viernes Santo
    fijo("05-01"),               # Día del Trabajo
    fijo("09-16"),               # Independencia
    fijo("11-02"),               # Día de Muertos
    n_esimo_lunes(anio, 11, 3),  # Revolución
    fijo("12-12"),               # Día del empleado bancario
    fijo("12-25")                # Navidad
  )
  if ((anio - 2024) %% 6 == 0) f <- c(f, fijo("10-01"))  # transmisión del Poder Ejecutivo
  f
}

siguiente_habil <- function(fecha) {
  repeat {
    fecha <- fecha + 1
    if (format(fecha, "%u") %in% c("6", "7")) next
    if (fecha %in% festivos_bmv(as.integer(format(fecha, "%Y")))) next
    return(fecha)
  }
}

# ---- Datos ----
datos  <- getSymbols(clave, from = "2000-01-01", auto.assign = FALSE)
precio <- Ad(datos)

rend <- na.omit(dailyReturn(precio, type = "log"))
rend <- rend[index(rend) < fin_rmd]     # información disponible al 2 de octubre

# ---- Modelo: mismo GARCH(1,1) de ARCH_GARCH.Rmd ----
# Media ARMA(2,1) con constante, varianza sGARCH(1,1), distribución normal.
# Se estima con el mismo 70 % de entrenamiento que el Rmd (datos anteriores a fin_rmd).
spec <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(2, 1), include.mean = TRUE),
  distribution.model = "norm"
)

rend_rmd <- rend[index(rend) < fin_rmd]
corte    <- floor(0.70 * NROW(rend_rmd))
r_train  <- rend_rmd[2:corte]

fit <- ugarchfit(spec, data = r_train, solver = "hybrid")
if (fit@fit$convergence != 0) stop("El GARCH(1,1) no convergió; no se actualiza el Excel.")

cf <- coef(fit)
message("Coeficientes: ", paste(names(cf), signif(cf, 6), sep = " = ", collapse = " | "))

# Filtra toda la historia con los parámetros fijos. n.old fija el arranque de la
# recursión en el periodo de entrenamiento, así el resultado no depende de cuántos datos haya después.
spec_fija <- spec
setfixed(spec_fija) <- as.list(cf)
r_todo <- rend[-1]                      # el primer rendimiento es 0 por construcción
filtro <- ugarchfilter(spec_fija, data = r_todo, n.old = NROW(r_train))

eps <- as.numeric(residuals(filtro))    # innovaciones epsilon_t
sig <- as.numeric(sigma(filtro))        # sigma_t, estimada con información hasta t-1
w   <- unname(cf["omega"]); a <- unname(cf["alpha1"]); b <- unname(cf["beta1"])

# Pronóstico a un paso con información hasta el día t (para el siguiente día hábil):
#   sigma2[t+1] = omega + alpha * eps[t]^2 + beta * sigma[t]^2
pron <- w + a * eps^2 + b * sig^2

# ---- Días hábiles a pronosticar ----
ultimo  <- tail(index(r_todo), 1)
fechas  <- siguiente_habil(ultimo)
while (tail(fechas, 1) < hasta) fechas <- c(fechas, siguiente_habil(tail(fechas, 1)))
fechas  <- fechas[fechas <= hasta]
H       <- length(fechas)

# ---- Pronóstico a h pasos ----
#   h = 1:  sigma2[T+1] = omega + alpha * eps[T]^2 + beta * sigma[T]^2
#   h > 1:  sigma2[T+h] = omega + (alpha + beta) * sigma2[T+h-1]
# (equivale a ugarchforecast con n.ahead = H; converge a la varianza de largo plazo)
var_h    <- numeric(H)
var_h[1] <- tail(pron, 1)
if (H > 1) for (h in 2:H) var_h[h] <- w + (a + b) * var_h[h - 1]

var_lp <- w / (1 - a - b)
message("Dato más reciente: ", ultimo, " | días pronosticados: ", H,
        " | volatilidad de largo plazo: ", signif(sqrt(var_lp), 4))

salida <- data.frame(
  fecha                        = as.character(fechas),
  horizonte                    = seq_len(H),
  varianza_estimada            = var_h,
  desviacion_estandar_estimada = sqrt(var_h)
)

write.xlsx(salida, archivo, overwrite = TRUE)
message("Pronóstico guardado en ", archivo, ": del ", salida$fecha[1], " al ", tail(salida$fecha, 1))
