## ---------------------------------------------------------------------
## index.R
## Análisis de natalidad y muerte en Guatemala (2021-2023)
## Autor: Luis Emilio Maldonado Rodríguez – 201931707
## Fecha: 17/05/2025
## ---------------------------------------------------------------------

# Carpeta raíz
root_dir <- "C:/Users/PC/Documents/Modelacion y Simulacion/Analisis de natalidad y muerte en Guatemala"

# Paquetes
pkgs <- c(
  "readxl",   # leer XLSX
  "dplyr",    # manipulación
  "purrr",    # programación funcional
  "janitor",  # limpieza de nombres
  "readr",    # detección / conversión de tipos
  "ggplot2",  # visualizaciones base
  "scales",   # etiquetas y formatos
  "lubridate",# fechas
  "tidyr",    # pivot y reshaping
  "ggridges"  # density-ridge (gráfica 10)
)

new_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new_pkgs) > 0) install.packages(new_pkgs, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

# Rutas a los archivos
years <- 2021:2023
path_birth <- file.path(root_dir, "Data", "Natalidad",
                        paste0("nacimientos-", years, ".xlsx"))
path_death <- file.path(root_dir, "Data", "Muerte",
                        paste0("defunciones-", years, ".xlsx"))

# Función de lectura (siempre texto)
leer_vitales <- function(path, anio) {
  readxl::read_excel(path,
                     sheet        = 1,
                     col_types    = "text",
                     .name_repair = "unique") |>
    janitor::clean_names() |>
    dplyr::mutate(anio = as.integer(anio))
}

# Carga individual en listas 
birth_list <- purrr::map2(path_birth, years, leer_vitales)
death_list <- purrr::map2(path_death, years, leer_vitales)

# Conversión preliminar de tipos numéricos
birth_list <- purrr::map(birth_list, readr::type_convert,
                         na = c("", "NA", "999"))
death_list <- purrr::map(death_list, readr::type_convert,
                         na = c("", "NA", "999"))

# Armonizar tipos (carácter gana)
harmonizar_tipos <- function(lst) {
  cols_char <- unique(unlist(
    purrr::map(lst, ~ names(.x)[vapply(.x, is.character, logical(1))])
  ))
  purrr::map(lst, ~ dplyr::mutate(.x,
                                  dplyr::across(all_of(cols_char), as.character)))
}
birth_list <- harmonizar_tipos(birth_list)
death_list <- harmonizar_tipos(death_list)

# Nombrar objetos por año y exponerlos
names(birth_list) <- paste0("nac_", years)
names(death_list) <- paste0("def_", years)

list2env(c(birth_list, death_list), .GlobalEnv)

# Consolidar en un solo data frame por tema
nacimientos <- dplyr::bind_rows(birth_list)
defunciones <- dplyr::bind_rows(death_list)

# Limpieza de NAs y valores “vacíos” 
recode_to_na <- function(df) {
  df |>
    dplyr::mutate(across(where(is.character),
                         ~ dplyr::na_if(.x, ""))) |>
    dplyr::mutate(across(where(is.character),
                         ~ dplyr::na_if(.x, "NA"))) |>
    dplyr::mutate(across(where(is.character),
                         ~ dplyr::na_if(.x, "999")))
}
nacimientos <- recode_to_na(nacimientos)
defunciones <- recode_to_na(defunciones)

# b) ELIMINAR filas donde TODAS las columnas son NA,

nacimientos <- janitor::remove_empty(nacimientos, which = "rows")
defunciones <- janitor::remove_empty(defunciones, which = "rows")

# c) Elimina columnas que estén 100 % vacías
nacimientos <- janitor::remove_empty(nacimientos, which = "cols")
defunciones <- janitor::remove_empty(defunciones, which = "cols")

## ---------------------------------------------------------------------
## Análisis exploratorio
## ---------------------------------------------------------------------

# Carga de Diccionario de departamentos de Guatemala
departamentos <- c(
  `1`  = "Guatemala",
  `2`  = "El Progreso",
  `3`  = "Sacatepéquez",
  `4`  = "Chimaltenango",
  `5`  = "Escuintla",
  `6`  = "Santa Rosa",
  `7`  = "Sololá",
  `8`  = "Totonicapán",
  `9`  = "Quetzaltenango",
  `10` = "Suchitepéquez",
  `11` = "Retalhuleu",
  `12` = "San Marcos",
  `13` = "Huehuetenango",
  `14` = "Quiché",
  `15` = "Baja Verapaz",
  `16` = "Alta Verapaz",
  `17` = "Petén",
  `18` = "Izabal",
  `19` = "Zacapa",
  `20` = "Chiquimula",
  `21` = "Jalapa",
  `22` = "Jutiapa"
)

# TIPOS DE DATOS
cat("Tipos de datos por variable (nacimientos):\n")
print(sapply(nacimientos, class))

cat("\nTipos de datos por variable (defunciones):\n")
print(sapply(defunciones, class))


# DIMENSIÓN DE LOS DATA FRAMES
cat("\n Dimensiones:\n")
cat("  Nacimientos  -> ", nrow(nacimientos), "filas |",
    ncol(nacimientos), "columnas\n")
cat("  Defunciones  -> ", nrow(defunciones), "filas |",
    ncol(defunciones), "columnas\n")


# VALORES PERDIDOS
na_birth <- colSums(is.na(nacimientos))
na_death <- colSums(is.na(defunciones))

cat("\n Variables con algún NA (nacimientos):\n")
print(na_birth[na_birth > 0])

cat("\n Variables con algún NA (defunciones):\n")
print(na_death[na_death > 0])

# RESUMEN ESTADISTICO DE VARIABLES NUMERICAS
nacimientos %>% select(where(is.numeric)) %>% summary()
defunciones %>% select(where(is.numeric)) %>% summary()

# CONTEO DE NACIMIENTOS POR AÑO-MES
nacimientos %>% 
  count(anio, mesocu, name = "nacimientos") %>% 
  arrange(anio, mesocu) %>% 
  print(n = 36)

# PROPORCION DE SEXO (NACIMIENTOS VS DEFUNCIONES)

sexo_table <- function(df, etiqueta){
  df %>% 
    count(sexo) %>% 
    mutate(prop = round(n / sum(n), 3),
           tipo = etiqueta)
}
bind_rows(
  sexo_table(nacimientos, "Nacimientos"),
  sexo_table(defunciones, "Defunciones")
)

# TOP 10 CAUSAS DE MUERTE

defunciones %>% 
  count(caudef, sort = TRUE) %>% 
  slice_head(n = 10)

# TOP 10 DEPARTAMENTOS DE NACIMIENTO

nacimientos %>% 
  mutate(dep_nombre = departamentos[as.character(depreg)]) %>% 
  count(dep_nombre, sort = TRUE) %>% 
  slice_head(n = 10)

## ---------------------------------------------------------------------
## 10 GRÁFICAS EXPLORATORIAS
## ---------------------------------------------------------------------

# Pre-cálculos rápidos 
nacimientos <- nacimientos %>% 
  mutate(
    peso_gr = libras * 453.592 + onzas * 28.3495,
    mes_lab = lubridate::month(mesocu, label = TRUE, abbr = TRUE)
  )

defunciones <- defunciones %>% 
  mutate(mes_lab = lubridate::month(mesocu, label = TRUE, abbr = TRUE))

# 1. Barras simples – nacimientos totales por año 
# Descripción: altura de cada barra = total anual de nacimientos.

p1 <- ggplot(nacimientos %>% count(anio),
             aes(x = factor(anio), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(n)),
            vjust = -0.4, size = 3.8) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, .05))) +
  labs(title = "Nacimientos totales por año (2021-2023)",
       x = "Año",
       y = "Número de nacimientos")

print(p1)


# 2. Barras agrupadas – nacimientos por sexo y año 
# Descripción: altura de cada barra = número de nacimientos.
p2 <- ggplot(nacimientos %>% count(anio, sexo),
             aes(x = factor(anio), y = n,
                 fill = factor(sexo, labels = c("Hombre", "Mujer")))) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"), name = "Sexo") +
  labs(title = "Nacimientos por sexo y año",
       x = "Año", y = "Número de nacimientos")

print(p2)


# 3. Boxplot – edad de la madre por año
# Descripción: mediana, IQR y valores atípicos por año.
p3 <- ggplot(nacimientos,
             aes(factor(anio), edadm)) +
  geom_boxplot() +
  labs(title = "Distribución de edad materna por año",
       x = "Año", y = "Edad de la madre")
print(p3)


# 4. Histograma “depurado” de peso al nacer 
# Descripción: muestra la distribución de pesos de recién nacidos
#             entre 1 000 y 6 000 gramos. La línea vertical indica la media.

nac_limpio <- nacimientos %>% 
  # convertir a NA los códigos 99 y medidas imposibles
  mutate(
    libras = ifelse(libras <= 15, libras, NA),
    onzas  = ifelse(onzas  <= 15, onzas,  NA)
  ) %>% 
  mutate(
    peso_gr = libras * 453.592 + onzas * 28.3495
  ) %>% 
  filter(between(peso_gr, 1000, 6000))

media_peso <- mean(nac_limpio$peso_gr, na.rm = TRUE)

p4 <- ggplot(nac_limpio, aes(peso_gr)) +
  geom_histogram(binwidth = 250,  # barras de 250 g
                 fill = "skyblue", color = "black") +
  geom_vline(xintercept = media_peso,
             linetype = "dashed", linewidth = 1, color = "red") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Histograma del peso al nacer (1 000 – 6 000 g)",
    subtitle = paste("Línea roja = media",
                     scales::comma(round(media_peso, 0)), "g"),
    x = "Peso (gramos)",
    y = "Frecuencia"
  )

print(p4)

# 5. Heat-map Nacidos por mes

p5 <- nacimientos %>% 
  filter(between(mesocu, 1, 12)) %>%            
  mutate(
    mes_lab = factor(mesocu,                      
                     levels = 1:12,
                     labels = month.abb)             
  ) %>% 
  count(anio, mes_lab) %>%                          
  tidyr::complete(anio, mes_lab, fill = list(n = 0)) %>% 
  ggplot(aes(mes_lab, factor(anio), fill = n)) +
  geom_tile(color = "grey90") +
  scale_fill_viridis_c(option = "C", name = "Nacimientos",
                       labels = scales::comma) +
  labs(
    title = "Nacimientos por mes y año (2021-2023)",
    x = "Mes",
    y = "Año"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid  = element_blank(),
    axis.ticks  = element_blank()
  )

print(p5)


# 6. Lollipop – top-10 departamentos de nacimiento 
# Descripción: compara conteos con línea-punto.
top_dep <- nacimientos %>% 
  mutate(dep_nombre = departamentos[as.character(depreg)]) %>% 
  count(dep_nombre, sort = TRUE) %>% 
  slice_head(n = 10) %>% 
  arrange(n)

p6 <- ggplot(top_dep,
             aes(n, reorder(dep_nombre, n))) +
  geom_segment(aes(xend = 0, yend = dep_nombre), linewidth = 1) +
  geom_point(size = 3, color = "steelblue") +
  labs(title = "Top-10 departamentos con más nacimientos (2021-2023)",
       x = "Número de nacimientos", y = "")
print(p6)

# 7. Línea – peso medio del recién nacido por edad materna 
# Descripción:
# • Cada punto es la media del peso (g) para madres de esa edad.
# • Permite ver si a mayor edad de la madre aumenta o disminuye el peso promedio.

# 1. preparar los datos (rango de edad razonable y peso válido)
nac_line <- nacimientos %>% 
  mutate(
    libras  = ifelse(libras <= 15, libras, NA),
    onzas   = ifelse(onzas  <= 15, onzas,  NA),
    peso_gr = libras * 453.592 + onzas * 28.3495
  ) %>% 
  filter(
    between(peso_gr, 1000, 6000),
    between(edadm,   10,   50)
  ) %>% 
  group_by(edadm) %>% 
  summarise(peso_medio = mean(peso_gr, na.rm = TRUE), .groups = "drop")

# 2. gráfico
p7 <- ggplot(nac_line, aes(x = edadm, y = peso_medio)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 1.8, color = "steelblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Peso medio al nacer vs edad de la madre",
    x     = "Edad de la madre (años)",
    y     = "Peso medio al nacer (g)"
  )

print(p7)


# 8. Violin – peso por sexo 
# Descripción: densidad de peso separada por sexo; punto = mediana.

nac_violin <- nacimientos %>% 
  mutate(
    libras  = ifelse(libras <= 15, libras, NA),
    onzas   = ifelse(onzas  <= 15, onzas,  NA),
    peso_gr = libras * 453.592 + onzas * 28.3495
  ) %>% 
  filter(
    between(peso_gr, 1000, 6000), 
    sexo %in% 1:2
  )

p8 <- ggplot(nac_violin,
             aes(x = factor(sexo, labels = c("Hombre", "Mujer")),
                 y = peso_gr, fill = factor(sexo))) +
  geom_violin(trim = FALSE, show.legend = FALSE, alpha = .8) +
  stat_summary(fun = median, geom = "point", size = 2, color = "black") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribución de peso al nacer por sexo (RN 1 000-6 000 g)",
    x     = "Sexo",
    y     = "Peso al nacer (g)"
  )

print(p8)

# 9. Donut – defunciones por sexo 
# Descripción: participa-ción relativa de hombres y mujeres fallecidos.
sexo_def <- defunciones %>% count(sexo) %>% 
  mutate(prop = n / sum(n))

p9 <- ggplot(sexo_def, aes(x = 2, y = prop, fill = factor(sexo))) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"),
                    labels = c("Hombre", "Mujer"),
                    name = "Sexo") +
  theme_void() +
  labs(title = "Distribución de defunciones por sexo (2021-2023)")
print(p9)

# 10. Density-ridge – edad al fallecer por sexo 
# Descripción: curvas de densidad superpuestas para comparar edades.
p10 <- ggplot(defunciones,
              aes(edadif,
                  factor(sexo, labels = c("Hombre", "Mujer")),
                  fill = factor(sexo))) +
  ggridges::geom_density_ridges(alpha = .8, show.legend = FALSE) +
  labs(title = "Distribución de edad al fallecer por sexo",
       x = "Edad (años)", y = "")
print(p10)

