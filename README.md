# Análisis del Riesgo: Grupo México (GMEXICOB)

Proyecto final de la materia **Análisis del Riesgo** (ITESO, otoño 2026), con la docente María del Rosario Ruiz Hernández. El proyecto estudia el riesgo de la acción de Grupo México (`GMEXICOB.MX`), desde el análisis fundamental hasta el VaR de un portafolio. Cada parte es un R Markdown (`.Rmd`) con su informe compilado en `.html`.

**Equipo:** Gibrán Chávez, Carlos Nieves, Pablo Tafolla y Carlos Romo.

## Contenido por parte

| Parte | Tema | Archivos |
|---|---|---|
| 1 | Análisis fundamental y de riesgos de la empresa | [Rmd](Proyecto_parte_1.Rmd) · [HTML](Proyecto_parte_1.html) |
| 2 | Historia del precio, eventos y relación con el PIB | [Rmd](Proyecto_parte_2.Rmd) · [HTML](Proyecto_parte_2.html) |
| 3 | Rendimientos, volatilidad, normalidad y movimiento browniano geométrico | [Rmd](Proyecto_parte_3.Rmd) · [HTML](Proyecto_parte_3.html) |
| 4 | Modelos de volatilidad (MA, EWMA, DDEWMA y GARCH) y su evaluación en octubre | [Rmd](Proyecto_parte_4.Rmd) · [HTML](Proyecto_Parte_4.html) |
| 5 | VaR de un portafolio, backtesting, optimización y Monte Carlo | [Rmd](Proyecto_parte_5.Rmd) · [HTML](Proyecto_parte_5.html) |

### Parte 1: La empresa y sus riesgos

Describe el negocio de Grupo México: su giro, sus líneas de negocio (minería, transporte e infraestructura), su escala y su posición competitiva. Analiza los estados financieros de 2023 a 2025 (ingresos, márgenes, apalancamiento y liquidez), la evolución del EPS, los dividendos y los splits. También clasifica los riesgos en sistemáticos y no sistemáticos.

**Conclusión:** la empresa tiene fundamentos sólidos dentro del ciclo alcista de los metales. Los ingresos crecieron 26.5% y la razón deuda bancaria/EBITDA bajó a 0.96x. Aun así, no es un negocio de bajo riesgo: depende mucho del cobre, que representa el 76.7% de las ventas mineras.

### Parte 2: El precio en el tiempo

Explora la serie histórica del precio y de los rendimientos y calcula el máximo drawdown. Relaciona los movimientos del precio con eventos concretos: la crisis de 2008, las tensiones comerciales, la pandemia, el endurecimiento monetario de 2022 y el caso Banamex. Además, compara el rendimiento anual de la acción con el crecimiento del PIB de México.

**Conclusión:** la acción se apreció mucho durante la muestra, pero sufrió un drawdown máximo de 84%. El PIB da contexto, pero no explica por sí solo la cotización. Hay indicios de que el mercado se anticipa a la economía: el rendimiento de un año se relaciona con el crecimiento del año siguiente.

### Parte 3: Rendimientos, volatilidad y GBM

Calcula los rendimientos logarítmicos y la volatilidad de 2021 a 2026, tanto para todo el periodo como año por año. Aplica pruebas de normalidad (Jarque-Bera) y de media igual a cero. Después estima el movimiento browniano geométrico con parámetros diarios y anuales, con intervalos de confianza para el precio a distintos horizontes.

**Conclusión:** es una acción de riesgo alto, con una volatilidad anual cercana a 38%, que llegó a cerca de 46% en 2022. Los rendimientos no son normales y tienen colas pesadas. El GBM sirve como aproximación, pero subestima el riesgo en crisis y en horizontes largos.

### Parte 4: Modelos de volatilidad

Estima la varianza diaria con promedio móvil, EWMA, EWMA basado en datos (DDEWMA) y GARCH(1,1). Compara los pronósticos contra la varianza observada del 5 al 30 de octubre de 2026 con RMSE, MAE y QLIKE.

**Conclusión:** se recomienda EWMA (λ ≈ 0.9685) para el seguimiento diario del riesgo, porque es sencillo y se actualiza de forma continua. Tiene el menor RMSE entre los pronósticos a un paso y la menor QLIKE. La recomendación es provisional, porque el periodo de evaluación es corto.

Para esta parte, las estimaciones diarias se generaron en automático con GitHub Actions (ver [Automatización](#automatización)).

### Parte 5: VaR de un portafolio

Construye un portafolio de seis acciones (ORBIA, WALMEX, GMEXICOB, ALSEA, GAPB y BBAJIOO) con 100,000 MXN en cada una, es decir, pesos iguales por valor. Sobre ese portafolio:

- Calcula el **VaR histórico** y el **VaR paramétrico** a 1 y 20 días al 95%, 97.5% y 99%, junto con el Expected Shortfall y el beneficio de diversificación.
- Hace **backtesting** con ventanas móviles: pruebas de Kupiec y Christoffersen y semáforo de Basilea.
- Calcula el **portafolio de mínima varianza** sin ventas en corto y muestra cómo reduce el VaR.
- Estima el **VaR y el ES de la posición en GMEXICOB por Monte Carlo**.

**Conclusión:** el VaR histórico supera al paramétrico, porque la normalidad no capta las colas pesadas. Ningún modelo se rechaza en el backtesting y ambos quedan en la zona verde de Basilea. La optimización reduce el VaR del portafolio alrededor de 7%.

## Automatización

Los scripts de R y los workflows de `.github/workflows/` generaron los datos de la Parte 4:

| Script | Qué hace | Archivo de salida |
|---|---|---|
| `estimar_varianza.R` | Varianza diaria por promedio móvil | `varianza_estimada.xlsx` |
| `estimar_ewma.R` | Volatilidad diaria con EWMA | `volatilidad_ewma.xlsx` |
| `estimar_ddewma.R` | Volatilidad diaria con DDEWMA | `volatilidad_ddewma.xlsx` |
| `estimar_garch.R` | Volatilidad diaria con GARCH(1,1) | `volatilidad_garch.xlsx` |
| `pronostico_garch_octubre.R` | Pronóstico GARCH a varios pasos para el 5–30 de octubre | `pronostico_garch_octubre.xlsx` |
| `varianza_observada_octubre.R` | Varianza observada de octubre (rendimientos al cuadrado) | `varianza_observada_octubre.xlsx` |

- El workflow **Estimar varianza diaria** (`actualizar_varianza.yml`) corría de lunes a viernes después del cierre del mercado. Ejecutaba los cuatro primeros scripts y hacía commit de los Excel actualizados.
- Los otros dos workflows (`pronostico_garch.yml` y `varianza_observada_octubre.yml`) se lanzan a mano y se corrieron una sola vez.

## Cómo reproducir

1. Abrir `Analisis_del_Riesgo.Rproj` en RStudio.
2. Instalar los paquetes que usan los Rmd, entre ellos `quantmod`, `rugarch`, `TTR`, `openxlsx`, `quadprog`, `knitr` y `rmarkdown`.
3. Compilar el Rmd de la parte que se quiera revisar con **Knit**.

Los precios se descargan de Yahoo Finance al compilar. Como Yahoo ajusta los precios de vez en cuando, las cifras pueden variar un poco respecto a los HTML publicados.
