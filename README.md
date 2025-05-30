# Análisis de Natalidad y Muerte en Guatemala (2021-2023)

Proyecto **Modelación y Simulación 1 – Sección A**,  
Universidad de San Carlos de Guatemala.

Autor: **Luis Emilio Maldonado Rodríguez – 201931707**

---

## Descripción

Este repositorio contiene el código, los datos y la documentación necesaria para reproducir el análisis exploratorio de los hechos vitales registrados en Guatemala entre 2021 y 2023. Se utilizan fuentes del Instituto Nacional de Estadística (INE) y R para procesamiento y visualización.

---

## Estructura de carpetas

```
├── Data
│   ├── Muerte
│   │   ├── defunciones-2021.xlsx
│   │   ├── defunciones-2022.xlsx
│   │   └── defunciones-2023.xlsx
│   └── Natalidad
│       ├── nacimientos-2021.xlsx
│       ├── nacimientos-2022.xlsx
│       └── nacimientos-2023.xlsx
│
├── Documentacion
│   └── Documentacion Mortalidad y Natalidad.pdf
│
├── index.Rmd
├── index.R
├── index.html
└── README.md
```

---

## Requisitos

| Software | Versión |
|----------|---------|
| **R** | ≥ 4.2 |
| **RStudio** (opcional) | ≥ 2022.12 |
| **Paquetes R** | readxl, dplyr, purrr, janitor, readr, ggplot2, scales, lubridate, tidyr, ggridges, knitr, rmarkdown |

El bloque `setup` de `index.Rmd` instala automáticamente los paquetes faltantes.

---

## Reproducibilidad

- Los archivos fuente (`*.xlsx`) están en la carpeta *Data*.  
- Todo el procesamiento se realiza en `index.Rmd`, garantizando reproducibilidad completa.

---

## Créditos 

- **Datos**: Instituto Nacional de Estadística (INE) – Estadísticas Vitales 2021‑2023.  

---
