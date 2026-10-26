# Estimación diaria de volatilidad con el modelo DDEWMA (data-driven EWMA)
# Añade al Excel la estimación para el siguiente día hábil bursátil.
# Si no hay un dato nuevo (fin de semana, festivo, mercado aún abierto), no hace nada.

suppressPackageStartupMessages({
  library(quantmod)
  library(openxlsx)
})

# ---- Parámetros ----
clave       <- "GMEXICOB.MX"
alpha       <- 0.06679819               # alpha óptimo (mínimo error cuadrático) de DDEWMA.Rmd
rho_rmd     <- 0.6911883                # rho de DDEWMA.Rmd, solo para comparar
fin_rmd     <- as.Date("2026-10-05")    # fecha 'to' del Rmd: los datos de entrenamiento son anteriores a ella
archivo     <- "volatilidad_ddewma.xlsx"
zona        <- "America/Mexico_City"
hora_segura <- 16                       # la BMV cierra a las 15:00; antes de las 16:00 el dato de hoy se descarta
inicio      <- "2026-10-05"             # en la primera corrida, rellena desde aquí (igual que los otros Excel)

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

# Si el mercado sigue abierto, el renglón de hoy es un precio parcial: se descarta
ahora <- as.POSIXlt(Sys.time(), tz = zona)
hoy   <- as.Date(format(ahora, "%Y-%m-%d"))
if (ahora$hour < hora_segura) precio <- precio[index(precio) < hoy]

rend <- na.omit(dailyReturn(precio, type = "log"))

# ---- R_barra y rho: mismo 70 % de entrenamiento que DDEWMA.Rmd ----
rend_rmd <- rend[index(rend) < fin_rmd]
corte    <- floor(0.70 * NROW(rend_rmd))
r_train  <- as.numeric(rend_rmd[2:corte])
R_barra  <- mean(r_train)
rho      <- cor(r_train - R_barra, sign(r_train - R_barra))
message("R_barra = ", signif(R_barra, 6), " | rho = ", signif(rho, 7),
        " (en DDEWMA.Rmd: ", rho_rmd, ")")

# ---- DDEWMA ----
# z[t]     = |r[t] - R_barra| / rho
# vol[t]   = volatilidad estimada PARA el día t, con información hasta t-1:
#   vol[t] = alpha * z[t-1] + (1 - alpha) * vol[t-1]
# El pronóstico con información hasta el día t (para el siguiente día hábil) es:
#   pron[t] = alpha * z[t] + (1 - alpha) * vol[t]
r <- as.numeric(rend)[-1]               # el primer rendimiento es 0 por construcción
z <- abs(r - R_barra) / rho
n <- length(z)

vol    <- numeric(n)
vol[1] <- z[1]
for (t in 2:n) vol[t] <- alpha * z[t - 1] + (1 - alpha) * vol[t - 1]
pron <- alpha * z + (1 - alpha) * vol

# Solo el último año para las fechas (el resto ya está en el Excel o no se necesita)
usar   <- seq(max(1, n - 250), n)
fechas <- index(rend)[-1][usar]

nuevos <- data.frame(
  fecha                        = as.character(do.call(c, lapply(fechas, siguiente_habil))),
  varianza_estimada            = pron[usar]^2,
  desviacion_estandar_estimada = pron[usar]
)

# ---- Añadir solo lo que falta ----
if (file.exists(archivo)) {
  previo <- read.xlsx(archivo)
  nuevos <- nuevos[nuevos$fecha > max(previo$fecha), ]   # también recupera días que hayan fallado
  if (nrow(nuevos) == 0) {
    message("Sin dato nuevo: la última estimación ya es la del ", max(previo$fecha), ". No se actualiza el Excel.")
    quit(status = 0)
  }
  salida <- rbind(previo, nuevos)
} else {
  nuevos <- nuevos[nuevos$fecha >= inicio, ]
  salida <- nuevos
}

write.xlsx(salida, archivo, overwrite = TRUE)
message("Filas añadidas: ", nrow(nuevos),
        ". Última estimación DDEWMA para ", tail(salida$fecha, 1),
        ": varianza = ", signif(tail(salida$varianza_estimada, 1), 4),
        ", volatilidad = ", signif(tail(salida$desviacion_estandar_estimada, 1), 4))
