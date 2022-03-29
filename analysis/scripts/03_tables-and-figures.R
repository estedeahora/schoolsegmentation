# General settings -------------------------------------------------------------

AUX$colores <- c("#7CAE00", "#00BFC4", "#F8766D", "#C77CFF")
TAB <- list()

# Fugiras ------------------------------------

  # Figure 01: Descripción de variables --------------------------------------

  d2 <- data.frame(SEC_MFA$quanti.var$coord,
                   CONTR = SEC_MFA$quanti.var$contrib[ ,1:2],
                   SEC_MFA$summary.quanti |>
                     select(group, variable) |>
                     filter(group %in% 1:3) ) %>%
    mutate(group = factor(group, levels = 1:3,
                          labels = c("Origen", "Rendimiento",
                                     "Homogeneidad")),
           CONTR = CONTR.Dim.1 + CONTR.Dim.2) %>%
    group_by(group) %>%
    mutate(m = mean(CONTR))

  p <- fviz_mfa_var(SEC_MFA, choice = c("quanti.var"), alpha.var = 0,
                    geom = "arrow", col.var.sup = NA, title = "") +
    geom_segment(data = d2,
                 aes(xend = Dim.1, yend = Dim.2,
                     colour = factor(group)),
                 x = 0, y = 0,
                 arrow = arrow(angle = 25, length = unit(0.25, "cm") ) ) +
    geom_text_repel(data = d2, #%>% filter(CONTR > m),
                    aes(x = Dim.1, y = Dim.2, label = variable),
                    size = 3, colour = "black") +
    facet_grid(cols = vars(group)) +
    scale_color_discrete("Dimensión de análisis") +
    # labs(caption = "Fuente: Elaboración propia.") +
    theme(legend.position = "none",
          strip.text.x = element_text(size = 16))

  ggsave(filename = here::here("analysis/figures/01_variables.png"),
         plot = p,
         width = 30, height = 12, units = "cm")

  rm(d2, p)

  # Figure 02: Escuelas por cluster ----------------------------------------------

  dat <- bind_cols(SEC_MFA$ind$coord |> data.frame(),
                   SEC_aux |> select(cl, cl_p, SECTOR))

  subp <- fviz_mfa_var(SEC_MFA, choice = c("quanti.var"), geom = "arrow",
                       axes = c(1,2), title = "Variables de análisis", ) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5) )

  p <- fviz_mfa_var(SEC_MFA, col.var = NA, col.var.sup = NA, title = "" ) +
    geom_point(data = dat |> filter(cl_p >= 0.5),
               mapping = aes(x = Dim.1, y = Dim.2,
                             color = cl, shape = SECTOR)) +
    geom_point(data = dat |> filter(cl_p < 0.5),
               mapping = aes(x = Dim.1, y = Dim.2,
                             shape = SECTOR), color = "grey70") +
    geom_point(data = data.frame(CLUSTER$H) |> add_row(), shape = 23,
               mapping = aes(x = Dim.1, y = Dim.2,
                             fill = factor(c(1:4, "Sin asignar"))),
               color = "black", size = 3) +
    scale_color_manual(values = AUX$colores) +
    scale_shape("Sector", solid = T) +
    scale_fill_manual("Clúster", values = c(AUX$colores, "grey70")) +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.margin = margin(0, 0.8, 0, 0.8, unit="cm")) +
    guides(shape = guide_legend(order = 1),
           fill = guide_legend(order = 2,
                               direction = "horizontal",
                               nrow = 2,
                               title.position = "top"),
           color = "none") +
    annotation_custom(grob = ggplotGrob(subp),
                      ymin = 2, ymax = 4.5, xmin = -3, xmax = -0.5)

  ggsave(filename = here::here("analysis/figures/02_esc-x-cluster.png"),
         plot = p,
         width = 30, height = 25, units = "cm")

  rm(p, subp, dat)

# Tablas -------------------------------------

  # Table 01: Descripción clúster ----------------------------------------------

  TAB$t1 <- CLUSTER$Descrip %>%
    mutate(cl = paste("Clúster", cl),
           P.Asig = round(P.Asig, 1),
           blank1 = NA,
           blank2 = NA) %>%
    select(cl, Cl.Size:Cl.Student, blank1,
           No.Asig:P.Asig, blank2,
           starts_with("deg.")) |>
    flextable() |>
    set_caption(caption = "Descripción general del resultado de los agrupamientos",
                autonum = run_autonum(seq_id = "tab",
                                      bkm = "FUZZY",
                                      bkm_all = T)) |>
    colformat_double(j = 8:10, digits = 2) %>%
    colformat_double(j = 6, digits = 1) %>%
    set_header_labels(cl = " ", Cl.Size = "Escuelas",
                      Cl.Student = "Estudiantes", blank1 = "",
                      No.Asig = "Total", P.Asig = "%", blank2 = "",
                      deg.Min = "Mínimo", deg.Max = "Máximo", deg.Av = "Promedio") |>
    add_header_row(colwidths = c(1, 2, 1, 2, 1, 3),
                   values = c(" ", "Tamaño del clúster", "",
                              "Escuelas difusas", "",
                              "Probabilidad de asignación") ) |>
    merge_h(part = "header") |>
    merge_v(part = "header") |>
    align(j = -1,  align = "center", part = "all") |>
    align(j = 1,  align = "left", part = "all") |>
    color(j = c("deg.Min", "deg.Max", "deg.Av"),
          color = "#9F9F9F", part = "all") |>
    autofit() |>
    add_footer_lines(value = paste("Coeficiente de Partición Modificado (MPC)",
                                   round(CLUSTER$Index["MPC"], 3),
                                   sep = ": ")
    )


  # Tabla 02: Alumnos por sector y clúster ----------------------------------

  TAB$t2 <- CLUSTER$Tab_Sec |>
    flextable() |>
    set_caption(caption = "Cantidad de Alumnos por Clústers según sector de gestión",
                autonum = run_autonum(seq_id = "tab",
                                      bkm = "CL-SECTOR",
                                      bkm_all = T)) |>
    set_header_labels(Circuito = " ", Privado = "Global",
                      SI = "Subsidiada", NO = "Independiente") |>
    add_header_row(colwidths = c(1, 1, 3, 1),
                   values = c(" ", "Estatal", "Privada", "Total") ) |>
    merge_h(part = "header") |>
    merge_v(part = "header") |>
    align(j = 2:6, align = "center", part = "all") |>
    color(j = 4:5, color = "#9F9F9F", part = "all") |>
    add_footer_lines(value = c(paste0("Versión Fuzzy del Índice de ",
                                      c("Rand", "Jaccard"), ": ",
                                      CLUSTER$Ind_Sec[-2]) ) ) |>
    autofit()

  # Tabla 03: Segregación según modelo ---------------------------------------

  TAB$t3 <- SEGRE$t_f |>
    flextable() |>
    set_caption(caption = "Porcentaje de segregación explicada por diferencia inter- e intra- grupo según modelo de agrupamiento",
                autonum = run_autonum(seq_id = "tab",
                                      bkm = "SEGRE1",
                                      bkm_all = T)) |>
    set_header_labels(I = "Modelo de agrupamiento",
                      Prop_ENTRE = "Inter-grupo",
                      Prop_DENTRO = "Intra-grupo") |>
    add_header_row(colwidths = c(1, 2),
                   values = c("Modelo de agrupamiento", "Proporción (%)") ) |>
    merge_v(part = "header") |>
    align(j = 2:3, align = "center", part = "all") |>
    autofit()


  # Tabla 04: Segregación local -inter e -intra --------------------------------

  TAB$t4 <- SEGRE$t_l %>%
    select(-p) |>
    flextable() |>
    set_caption(caption = "Segregación local -inter e -intra clúster",
                autonum = run_autonum(seq_id = "tab",
                                      bkm = "SEGRE2",
                                      bkm_all = T)) |>
    set_header_labels(cl = "Clúster") |>
    add_header_row(colwidths = c(1, 2),
                   values = c("Clúster", "Mutual Index Local") ) |>
    merge_v(part = "header") |>
    align(j = 2:3, align = "center", part = "all") |>
    colformat_double(digits = 3) |>
    autofit()
