# -------------------------------------------------------
# PASO 1: Asignar generación según parentesco (CH03)
# -------------------------------------------------------


eph_ind <- eph_ind %>%
  group_by(CODUSU, NRO_HOGAR) %>%
  mutate(hay_ascendientes = any(CH03 == 6 | CH03 == 7, na.rm = TRUE)) %>%
  ungroup()

eph_ind <- eph_ind %>%
  mutate(
    generacion = ifelse(hay_ascendientes == TRUE, case_when(
      CH03 %in% c(6, 7)    ~ 1,  # Madre/padre, Suegro/a → ascendientes
      CH03 %in% c(1, 2, 8) ~ 2,  # Jefe/a, Cónyuge, Hermano/a
      CH03 %in% c(3, 4)    ~ 3,  # Hijo/a, Yerno/nuera
      CH03 == 5             ~ 4,  # Nieto/a
      CH03 %in% c(9, 10)   ~ 2,  # Otros parientes y no parientes
      TRUE                  ~ NA_integer_
    ),
    case_when(
      CH03 %in% c(1, 2, 8) ~ 1,  # Jefe/a, Cónyuge, Hermano/a → generación 1
      CH03 %in% c(3, 4)    ~ 2,  # Hijo/a, Yerno/nuera → generación 2
      CH03 == 5             ~ 3,  # Nieto/a → generación 3
      CH03 %in% c(9, 10)   ~ 1, # Otros parientes y no parientes 
      TRUE                  ~ NA_integer_
    ))
  )

# -------------------------------------------------------
# PASO 2: Características del hogar
# -------------------------------------------------------

eph_ind <- eph_ind %>%
  group_by(CODUSU, NRO_HOGAR) %>%
  mutate(
    n_miembros       = n(),
    n_generaciones   = n_distinct(generacion, na.rm = TRUE),
    
    # ¿Todos los miembros del hogar superan los 25 años?
    todos_mayores_25 = all(CH06 >= 25, na.rm = TRUE),
    
    # ¿Hay ascendientes ACTIVOS? (solo relevante si hay_ascendientes == TRUE)
    hay_ascend_activos = hay_ascendientes & any(
      generacion == 1 & ESTADO == 1,
      na.rm = TRUE
    ),
    
    # ¿Hay gen 1 en el hogar? (jefe/cónyuge cuando no hay ascendientes)
    hay_gen1 = any(generacion == 1, na.rm = TRUE),
    
    # ¿Todos los de gen 1 son inactivos?
    gen1_todos_inactivos = hay_gen1 & all(
      ESTADO[generacion == 1] %in% c(3, 4),
      na.rm = TRUE
    ),
    
    # ¿Hay gen 2 en el hogar?
    hay_gen2 = any(generacion == 2, na.rm = TRUE),
    
    # ¿Todos los de gen 2 son inactivos?
    gen2_todos_inactivos = hay_gen2 & all(
      ESTADO[generacion == 2] %in% c(3, 4),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# -------------------------------------------------------
# PASO 3: Identificar candidatos a referente
# -------------------------------------------------------

eph_ind <- eph_ind %>%
  mutate(
    es_candidato = case_when(
      
      # Hogar unipersonal: siempre referente
      n_miembros == 1                                         ~ TRUE,
      
      # Gen 1: 
      #   - Si hay ascendientes: solo si son activos
      #   - Si no hay ascendientes: siempre (son jefe/cónyuge)
      generacion == 1 & hay_ascendientes & ESTADO == 1        ~ TRUE,
      generacion == 1 & !hay_ascendientes                     ~ TRUE,
      
      # Gen 2: candidatos si:
      #   - no hay ascendientes activos, Y
      #   - gen1 inactiva, O no hay gen1, O todos > 25 años
      generacion == 2 & !hay_ascend_activos &
        (gen1_todos_inactivos | !hay_gen1 | todos_mayores_25) ~ TRUE,
      
      # Gen 3: candidatos si:
      #   - no hay ascendientes activos, Y
      #   - gen2 toda inactiva
      generacion == 3 & !hay_ascend_activos &
        gen2_todos_inactivos                                   ~ TRUE,
      
      # Resto: no candidatos
      TRUE                                                     ~ FALSE
    )
  )

# -------------------------------------------------------
# PASO 4: Variables de ranking
# -------------------------------------------------------

datos <- datos %>%
  mutate(
    # 1. Prioridad por estado de actividad
    prioridad_estado = case_when(
      ESTADO == 1 ~ 1L,   # Ocupado
      ESTADO == 2 ~ 2L,   # Desocupado
      ESTADO == 3 ~ 3L,   # Inactivo
      TRUE        ~ 4L
    ),
    
    # 2. Calificación (solo para ocupados, resto al final)
    # 1=Profesional, 2=Técnico, 3=Operativo, 4=No calificado
    calificacion_ord = case_when(
      ESTADO != 1       ~ 99L,
      calificacion == 1 ~ 1L,
      calificacion == 2 ~ 2L,
      calificacion == 3 ~ 3L,
      calificacion == 4 ~ 4L,
      TRUE              ~ 98L   # NS/NR
    ),
    
    # 3. Ingreso total individual
    ingreso_ord    = ifelse(is.na(P47T),    -Inf, as.numeric(P47T)),
    
    # 4. Nivel educativo
    educacion_ord  = ifelse(is.na(NIVEL_ED), -Inf, as.numeric(NIVEL_ED)),
    
    # 5. Antigüedad laboral (variable armada por vos)
    antiguedad_ord = ifelse(is.na(antig),    -Inf, as.numeric(antig))
  )

# -------------------------------------------------------
# PASO 5: Seleccionar referente por hogar
# -------------------------------------------------------

referentes <- datos %>%
  filter(es_candidato) %>%
  group_by(CODUSU, NRO_HOGAR) %>%
  arrange(
    prioridad_estado,       # 1. Ocupado > Desocupado > Inactivo
    calificacion_ord,       # 2. Profesional > Técnico > Operativo > No calif.
    desc(ingreso_ord),      # 3. Mayor ingreso
    desc(educacion_ord),    # 4. Mayor educación
    desc(antiguedad_ord),   # 5. Mayor antigüedad
    generacion,             # 6. Generación menor (desempate final)
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE) %>%
  mutate(es_referente = 1L)

# -------------------------------------------------------
# PASO 6: Unir al dataset original
# -------------------------------------------------------

datos <- datos %>%
  left_join(referentes, by = c("CODUSU", "NRO_HOGAR", "COMPONENTE")) %>%
  mutate(es_referente = ifelse(is.na(es_referente), 0L, es_referente))

# -------------------------------------------------------
# VERIFICACIONES
# -------------------------------------------------------

# 1. Cada hogar debe tener exactamente 1 referente
datos %>%
  group_by(CODUSU, NRO_HOGAR) %>%
  summarise(n_ref = sum(es_referente), .groups = "drop") %>%
  count(n_ref)

# 2. ¿Qué parentesco tiene el referente elegido?
datos %>%
  filter(es_referente == 1) %>%
  count(CH03) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# 3. ¿Qué generación tiene el referente elegido?
datos %>%
  filter(es_referente == 1) %>%
  count(generacion) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# 4. ¿Hay hogares sin referente asignado? (no debería haber)
datos %>%
  group_by(CODUSU, NRO_HOGAR) %>%
  summarise(n_ref = sum(es_referente), .groups = "drop") %>%
  filter(n_ref == 0) %>%
  nrow()