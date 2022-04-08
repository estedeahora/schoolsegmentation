# wcomment ----------------------------------------------------------------
# Permite agregar comentarios de Word

wcomment <- function(cm, txt = "", author = "PS", date = Sys.Date(), comN = commentN){
  cmt_str <- paste0('<span class="comment-text">[',cm,']{.comment-start id="', comN, '" author="', author, '" date="', date, '"}', txt, '[]{.comment-end id="',commentN,'"}</span>')
  assign("commentN", commentN + 1, envir = .GlobalEnv)
  return(cmt_str)
}

# f_seg ---------------------------------------------------------------------
# Calculo de Mutual Information Index Total + Between + Within

f_seg <- function(db, VAR, grupo = "group", peso = "peso", id = "ID_s"){
  res <- data.frame(Global = segregation::mutual_total(data = db, group = grupo,
                                                       unit = "ID_s", weight = peso,
                                                       se = F)$est[1],
                    Between = segregation::mutual_total(data = db, group = grupo,
                                                        unit = VAR, weight = peso,
                                                        se = F)$est[1],
                    Within =  segregation::mutual_total(data = db, group = grupo,
                                                        unit = id, weight = peso,
                                                        se = F, within = VAR)$est[1])
  return(res)
}


# fviz_individual ----------------------------------------------------------------
# Definiciones
# ind_res <- SEC_MFA$separate.analyses$`Estructura (sup.)`
# tit <- "d"
# cl  <- SEC_aux[c("cl", "cl_p", "SECTOR")]
# dim <- 1:2
# u   <-  0.5
# colores  <- AUX$colores

fviz_individual <- function(ind_res,
                            tit = "Grupo",
                            cl = SEC_aux[c("cl", "cl_p", "SECTOR")],
                            dim = 1:2,
                            u = 0.5,
                            colores = AUX$colores,
                            clean_outliers = T,
                            blur_cuali = T
                            ){


  if(!is.data.frame(cl)){
    stop("cl debe ser de clase dataframe")
  }
  if(!all(c("cl", "cl_p", "SECTOR") %in% names(cl) ) ){
    stop("cl debe tener variables con los nombres: cl, cl_p y SECTOR")
  }
  if(nrow(cl) != nrow(ind_res$ind$coord )){
    stop("cl debe tener la misma cantidad de filas que coord")
  }

  clase <- ind_res |> class()

  dat <- ind_res$ind$coord |>
    as_tibble() |>
    add_column(cl) |>
    mutate(cl_num = as.numeric(cl),
           cl_f = ifelse(cl_p < u, max(cl_num) + 1, cl_num),
           cl_f = factor(cl_f, levels = 1:(max(cl_num) + 1),
                         labels = c(paste0("Cluster ", 1:max(cl_num) ), "Sin asignar")  )
          )
  colnames(dat)[dim] <- c("x", "y")

  if(nrow(ind_res$eig) > 1 ){
    if(clase[[1]] == "PCA"){

      p <- fviz_pca_var(ind_res, col.var = "black",
                        title = tit, axes = dim, repel = T)

    }else if(clase[[1]] == "MCA"){
      p <- fviz_mca_var(ind_res, repel = T, geom = "text",
                        title = tit)

      if(blur_cuali){
        dat <- dat |>
          mutate(across(.cols = c(x, y),
                        .fns = ~.x +  rnorm(sd = sd(.x) / 20, n = n())
                        )
                 )
      }

    }else{
      stop("ind_res no es una Clase reconocida. Clase:", clase)
    }

    if(clean_outliers){
      dat <- dat |>
        mutate(x_q1 = quantile(x, probs = 0.25),
               x_q3 = quantile(x, probs = 0.75),
               x_IQR = x_q3 - x_q1,
               x_out = x < x_q1 - 1.5 * x_IQR |
                        x >  x_q3 + 1.5 * x_IQR,
               y_q1 = quantile(y, probs = 0.25),
               y_q3 = quantile(y, probs = 0.75),
               y_IQR = y_q3 - y_q1,
               y_out = y < y_q1 - 1.5 * y_IQR |
                        y >  y_q3 + 1.5 * y_IQR
               ) |>
        filter(!x_out & !y_out)
    }

    p +
      geom_point(data = dat, alpha = 0.4,
                 mapping = aes(x = x, y = y,
                               color = cl_f, shape = SECTOR))  +
      scale_color_manual("Cluster", values =  c(colores, "grey70")) +
      scale_shape("Sector", solid = T) +
      theme(legend.position = "bottom") +
      guides(shape = guide_legend(order = 1, direction = "vertical"),
             color = guide_legend(order = 2, direction = "horizontal",
                                  nrow = 2, title.position = "top") )
  }
}
