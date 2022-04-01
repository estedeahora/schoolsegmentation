# Tablas y figuras del Anexo en línea

# Figuras ------------------------------------



# Tablas -------------------------------------

  # Tabla A1 ----------------------------------------------------------------
  # library(kableExtra)
  options(knitr.kable.NA = '')
  l <- AUX$GRUPO |>
    mutate(Dimensión = factor(Dimensión,
                              levels = unique(Dimensión) ) ) |>
    count(Dimensión) |>
    deframe()

  # TAB$A1 <-
  AUX$GRUPO |>
    select(Indicador, Tipo, VAR,
           Fuente.de.datos, Notas) |>
  # l <- which( c(!is.na(t1$DIM)[-1], F))
    kableExtra::kbl(booktabs = TRUE, linesep = "", format = "html",
        col.names = c("Indicador",
                      "MFA", "Variable",
                      "Fuente de datos", "Notas"),
        caption = "Dimensiones, indicadores y fuentes de datos usados en el análisis de segmentación") |>
    # kable_styling(font_size = 7) %>%
    kableExtra::pack_rows(index = l) |>
    kableExtra::footnote(general_title = "Notas:",
                         general = paste0("x: Variables activas; ",
                                          "Sup.: Variables suplementarias cuantitativas; ",
                                          "Sup. Cuali: Variables suplementarias cualitativas; ",
                                          "-: Variable no considerada para el nivel"),
                         number = notas, #number_title = "Notas",
                         threeparttable = T)

  # rm(t1, l, notas)

  # Tabla C01: Descripción Variables cuantitativas -------------------------

  TAB$C1 <- C_CUALI |>
    kableExtra::kbl(caption = "Variables cualitativas características de cada Clúster",
                    booktabs = T, row.names = F, linesep = "") %>%
    # kable_styling(latex_options = "scale_down", font_size = 7) %>%
    # column_spec(column = 1:4, width = "3cm") %>%
    kableExtra::pack_rows("Promedio sobre la media",
                          1, 5, hline_after = T) %>%
    kableExtra::pack_rows("Promedio debajo de la media",
                          6, 10, hline_after = T)

  rm(C_CUALI)
    # mutate(var_gr = rep(paste0(c("Sobre", "Debajo de"), " la media"),
    #                     each = 5)) |>
    # as_grouped_data(groups = c("var_gr"), columns = NULL) |>
    # as_flextable(hide_grouplabel = T) |>
    # set_caption(caption = "Variables cualitativas características de cada Clúster",
    #             autonum = run_autonum(seq_id = "tab", bkm = "DESC-CUALI",
    #                                   bkm_all = T)) |>
    #
    # hline(i = ~ !is.na(var_gr), border = fp_border(color = "#808080") ) |>
    # italic(i = ~ !is.na(var_gr), italic = TRUE, part = "body") |>
    # # color(i = ~ !is.na(var_gr), color = "#808080", part = "body") |>
    # bg(i = ~!is.na(var_gr), bg = "#606060", part = "body") |>
    # color(i = ~!is.na(var_gr), color = "white", part = "body") |>
    # autofit()

  # Tabla C02: Descripción Variables cuantitativas -------------------------

  TAB$C2 <- C_CUANTI |>
    kableExtra::kbl(caption ="Variables cuantitativas características de cada Clúster",
        booktabs = T, row.names = F, linesep = "") %>%
    # kable_styling(latex_options = "scale_down", font_size = 7) %>%
    # column_spec(column = 1:4, width = "3cm") %>%
    kableExtra::pack_rows("Porcentaje encima de la media",
                          1, 5, hline_after = T) %>%
    kableExtra::pack_rows("Porcentaje debajo de la media",
                          6, 10, hline_after = T)
    # mutate(var_gr = rep(paste0(c("Sobre", "Debajo de"), " la media"),
    #                     each = 5)) |>
    # as_grouped_data(groups = c("var_gr"), columns = NULL) |>
    # as_flextable(hide_grouplabel = T) |>
    # set_caption(caption = "Variables cuantitativas características de cada Clúster",
    #             autonum = run_autonum(seq_id = "tab",
    #                                   bkm = "DESC-CUANTI",
    #                                   bkm_all = T)) |>
    # hline(i = ~ !is.na(var_gr), border = fp_border(color = "#808080") ) |>
    # italic(i = ~ !is.na(var_gr), italic = TRUE, part = "body") |>
    # bg(i = ~ !is.na(var_gr), bg = "#606060", part = "body") |>
    # color(i = ~ !is.na(var_gr), color = "white", part = "body") |>
    # autofit()

  rm(C_CUANTI)
