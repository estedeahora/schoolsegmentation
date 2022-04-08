# Tablas y figuras del Anexo en línea

# Figuras ------------------------------------

  # Figura B01 --------------------------------------------------------------

  p1 <- fviz_eig(SEC_MFA,
                barcolor = "tomato1", barfill = "tomato1",
                linecolor = "grey30",
                addlabels = T, hjust = 0.5,
                main = "Varianza explicada",
                xlab = "Dimensión",
                ylab = "Porcentaje de varianza explicada [%]")


  p2 <- SEC_MFA$eig[1:10, ] |>
    as.data.frame() |>
    rename(var = 'cumulative percentage of variance') |>
    mutate(Dim = 1:n()) |>
    ggplot(aes(x = Dim, y = var)) +
    geom_point(color = "tomato1", shape = 3) +
    geom_line(color = "grey30", linetype = 2) +
    scale_x_continuous("Dimensión", breaks = 0:10 ) +
    scale_y_continuous("Porcentaje de varianza acumulada [%]", limits = c(0, 100)) +
    labs(title = "Varianza acumulada") +
    theme_minimal()

  ggsave(filename = here::here("analysis/figures/ApB_01_Varianza-explicada.png"),
         plot = p1 + p2,
         width = 30, height = 15, units = "cm")

  rm(p1, p2)

  # Figura 2 ----------------------------------------------------------------

  res <- map2(SEC_MFA$separate.analyses,
              names(SEC_MFA$separate.analyses),
              ~fviz_individual(ind_res = .x, tit = .y) )

  res <- res[!map_lgl(res,is.null ) ]
  n <- 1:length(res)
  n <- ifelse(str_length(n) == 1,
              paste0("0", n), n )

  path_file <- here::here("analysis/figures/ApD_")
  file <- paste0(path_file, n, "_",
                 names(res), ".png")

  walk2(.x = res, .y = file,
        .f = ~ggsave(filename = .y, plot = .x,
                     width = 20, height = 16, units = "cm"))

  rm(res, path_file, file, n)

# Figure F ----------------------------------------------------------------

# a <- map_df(2:10,
#             \(x){
#                 Fclust(SEC_MFA$ind$coord[ , 1:4],
#                 type = "gk", stand= 0,
#                 k = x, noise = T) |>
#                 Fclust.index(alpha = 1)
#                 })
# a |>
#     mutate(ID = 2:10) |>
#     pivot_longer(cols = - ID, names_to = "var") |>
#     ggplot(aes(x =ID, y = value)) +
#     geom_line() +
#     facet_wrap(~var, scales = "free") +
#     scale_x_continuous(n.breaks = 10) +
#     theme_minimal()

# Tablas -------------------------------------

  # Tabla A1 ----------------------------------------------------------------

  options(knitr.kable.NA = '')

  gr_aux <- AUX$GRUPO |>
    arrange(DIM_LAB, ACTIVA, CLASE)

  tab_note <- c(
    "Para el cálculo de indicadores mediante el módulo de estudiantes de la encuesta FEPBA/TESBA, se consideraron sólo aquellas instituciones en las que hayan respondido al cuestionario 5 o más casos, para evitar el sesgo que pueda implicar la tasa de cobertura del relevamiento.\n",
    paste0("Para el cálculo de los porcentajes de repitentes y personas con sobreedad, los ciclos fueron establecidos de acuerdo a la división utilizada habitualmente. Primer ciclo se consideró 1ero y 2do año, mientras que Segundo ciclo 3ero a 5to año.",
           "No se consideró en estos cálculos los cursos de nivelación ni 6to año en las escuelas donde este año es parte de la curricula.\n"),
    "Para moderar la influencia de valores atípicos, se utiliza como indicador la mediana de cada escuela tomando en conjunto los años del RA 2015 a 2018. Para el número de cargos en relación a la matrícula se toma el año 2018"
               )

  l <- gr_aux |>
    mutate(Dimensión = factor(Dimensión,
                              levels = unique(Dimensión) ) ) |>
    count(Dimensión) |>
    deframe()

  TAB$A1 <- gr_aux |>
    mutate(Notas = str_remove_all(Notas, "\\[|\\]"),
           Notas = as.numeric(Notas),
           Indicador = ifelse(!is.na(Notas),
                              paste0(Indicador, " ",
                                     kableExtra::footnote_marker_alphabet(Notas)),
                              Indicador)) |>
    select(Indicador, Tipo, VAR,
           Fuente.de.datos) |>
    kableExtra::kbl(booktabs = TRUE, linesep = "",
                    format = "html", escape = F,
        col.names = c("Indicador",
                      "MFA", "Variable",
                      "Fuente de datos"),
        caption = "Dimensiones, indicadores y fuentes de datos usados en el análisis de segmentación") |>
    kableExtra::pack_rows(index = l) |>
    kableExtra::kable_paper(full_width = F) |>
    kableExtra::footnote(general_title = "Notas:",
                         general = paste0("x: Variables activas; ",
                                          "Sup.: Variables suplementarias cuantitativas; ",
                                          "Sup. Cuali: Variables suplementarias cualitativas; ",
                                          "-: Variable no considerada para el nivel"),
                         alphabet = tab_note,
                         threeparttable = T,
                         footnote_as_chunk = T)

  rm(l, gr_aux, tab_note)

  # Tabla C01: Descripción Variables cuantitativas -------------------------

  TAB$C1 <- C_CUALI |>
    kableExtra::kbl(caption = " ",
                    booktabs = T, row.names = F, linesep = "") %>%
    kableExtra::pack_rows("Promedio sobre la media",
                          1, 5, hline_after = T) %>%
    kableExtra::pack_rows("Promedio debajo de la media",
                          6, 10, hline_after = T)

  rm(C_CUALI)

  # Tabla C02: Descripción Variables cuantitativas -------------------------

  TAB$C2 <- C_CUANTI |>
    kableExtra::kbl(caption =" ",
        booktabs = T, row.names = F, linesep = "") %>%
    kableExtra::pack_rows("Porcentaje encima de la media",
                          1, 5, hline_after = T) %>%
    kableExtra::pack_rows("Porcentaje debajo de la media",
                          6, 10, hline_after = T)

  rm(C_CUANTI)

