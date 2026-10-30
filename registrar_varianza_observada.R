# Registro diario de la varianza observada: rendimientos logarítmicos al cuadrado
# En la primera corrida llena todo el periodo desde 'inicio'; después añade cada día hábil nuevo.
# Si no hay un dato nuevo (fin de semana, festivo, mercado aún abierto), no hace nada.

suppressPackageStartupMessages({
  library(quantmod)
  library(openxlsx)
})

# ---- Parámetros ----
clave       <- "GMEXICOB.MX"
archivo     <- "varianza_observada.xlsx"
zona        <- "America/Mexico_City"
hora_segura <- 16                       # la BMV cierra a las 15:00; antes de las 16:00 el dato de hoy se descarta
inicio      <- "2026-10-05"             # primer día del periodo (el mismo que los Excel de pronósticos)

# ---- Datos ----
# Se descarga desde un mes antes de 'inicio' para tener el cierre previo al primer día
datos  <- getSymbols(clave, from = as.Date(inicio) - 30, auto.assign = FALSE)
precio <- na.omit(Ad(datos))

# Si el mercado sigue abierto, el renglón de hoy es un precio parcial: se descarta
ahora <- as.POSIXlt(Sys.time(), tz = zona)
hoy   <- as.Date(format(ahora, "%Y-%m-%d"))
if (ahora$hour < hora_segura) precio <- precio[index(precio) < hoy]

# Rendimiento logarítmico, igual que en los otros scripts.
# [-1] quita el primer rendimiento, que dailyReturn pone en 0 porque no tiene cierre previo
rend <- dailyReturn(precio, type = "log")[-1]

# ---- Varianza observada ----
# 'fecha' es el día del rendimiento: se compara directamente con la fila de esa
# misma fecha en los Excel de pronósticos (el pronóstico hecho el día hábil anterior).
nuevos <- data.frame(
  fecha              = as.character(index(rend)),
  varianza_observada = as.numeric(rend)^2
)
nuevos <- nuevos[nuevos$fecha >= inicio, ]

# ---- Añadir solo lo que falta ----
if (file.exists(archivo)) {
  previo <- read.xlsx(archivo)
  nuevos <- nuevos[nuevos$fecha > max(previo$fecha), ]   # también recupera días que hayan fallado
  if (nrow(nuevos) == 0) {
    message("Sin dato nuevo: el último registro ya es el del ", max(previo$fecha), ". No se actualiza el Excel.")
    quit(status = 0)
  }
  salida <- rbind(previo, nuevos)
} else {
  salida <- nuevos
}

write.xlsx(salida, archivo, overwrite = TRUE)
message("Filas añadidas: ", nrow(nuevos),
        ". Último día registrado: ", tail(salida$fecha, 1),
        " | varianza observada = ", signif(tail(salida$varianza_observada, 1), 4))
