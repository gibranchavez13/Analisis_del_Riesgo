# Varianza observada (rendimientos logarítmicos al cuadrado) de todo octubre de 2026.
# Se corre una sola vez; no forma parte del workflow diario.

suppressPackageStartupMessages({
  library(quantmod)
  library(openxlsx)
})

# ---- Parámetros ----
clave       <- "GMEXICOB.MX"
desde       <- as.Date("2026-10-01")
hasta       <- as.Date("2026-10-30")    # último día hábil de octubre
archivo     <- "varianza_observada_octubre.xlsx"
zona        <- "America/Mexico_City"
hora_segura <- 16                       # la BMV cierra a las 15:00; antes de las 16:00 el dato de hoy se descarta

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
# Se descarga desde un mes antes para tener el cierre previo al primer día
datos  <- getSymbols(clave, from = desde - 30, to = hasta + 1, auto.assign = FALSE)
precio <- na.omit(Ad(datos))

# Si el mercado sigue abierto, el renglón de hoy es un precio parcial: se descarta
ahora <- as.POSIXlt(Sys.time(), tz = zona)
hoy   <- as.Date(format(ahora, "%Y-%m-%d"))
if (ahora$hour < hora_segura) precio <- precio[index(precio) < hoy]

# Rendimiento logarítmico, igual que en los otros scripts.
# [-1] quita el primer rendimiento, que dailyReturn pone en 0 porque no tiene cierre previo
rend <- dailyReturn(precio, type = "log")[-1]
rend <- rend[index(rend) >= desde & index(rend) <= hasta]

# ---- Revisar que estén todos los días hábiles del mes ----
esperados <- siguiente_habil(desde - 1)
while (tail(esperados, 1) < hasta) esperados <- c(esperados, siguiente_habil(tail(esperados, 1)))
esperados <- esperados[esperados <= hasta]
faltan    <- esperados[!esperados %in% index(rend)]

if (tail(esperados, 1) %in% faltan) {
  stop("Yahoo aún no tiene el cierre del ", tail(esperados, 1),
       ". Vuelve a correrlo más tarde; no se guardó nada.")
}
if (length(faltan) > 0) {
  message("Aviso: Yahoo no tiene datos de: ", paste(faltan, collapse = ", "))
}

# ---- Guardar ----
salida <- data.frame(
  fecha              = as.character(index(rend)),
  varianza_observada = as.numeric(rend)^2
)

write.xlsx(salida, archivo, overwrite = TRUE)
message("Guardados ", nrow(salida), " días hábiles en ", archivo,
        ": del ", salida$fecha[1], " al ", tail(salida$fecha, 1))
