library(dplyr)
library(tidyr)
library(formattable)

# Variables a cruzar con cluster
variables <- c("jerarquia", "calificacion", "sexo", "edad_cat", 
               "nivel_educativo", "situacion_conyugal", "decil_IPCF")

# ── 1. Tabla larga con proporciones por cluster ──────────────────────────────
tabla_larga <- lapply(variables, function(var) {
  eph_seleccion %>%
    group_by(cluster, categoria = .data[[var]]) %>%
    summarise(n = sum(PONDIH), .groups = "drop") %>%
    group_by(cluster) %>%
    mutate(prop = round(n / sum(n) * 100, 1)) %>%
    ungroup() %>%
    mutate(variable = var)
}) %>%
  bind_rows()

# ── 2. Pivotear: filas = variable + categoría, columnas = cluster ────────────
tabla_wide <- tabla_larga %>%
  select(variable, categoria, cluster, prop) %>%
  pivot_wider(names_from = cluster, values_from = prop) %>%
  arrange(variable, categoria)

# ── 3. Aplicar formattable con color por columna (cada cluster) ──────────────
cols_cluster <- names(tabla_wide)[-(1:2)]  # todas menos "variable" y "categoria"

formatters_list <- setNames(
  lapply(cols_cluster, function(x) color_tile("white", "steelblue")),
  cols_cluster
)

formattable(tabla_wide, formatters_list)
