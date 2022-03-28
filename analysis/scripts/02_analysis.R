# Package -----------------------------------------------------------------

library(flextable)
library(knitr)
library(officer)
library(officedown)

library(ggpubr)
library(ggrepel)
library(patchwork)

library(tidyverse)
library(sf)

library(FactoMineR)
library(factoextra)
library(missMDA)
library(fclust)
library(segregation)

# Load data ---------------------------------------------

load(here::here("analysis/data/school-segmentation.RData"))

SEC_aux <- SEC |> select(-starts_with("N_"))
SEC_seg <- SEC |> st_drop_geometry() |> select(ID_s, starts_with("N_"))

# Impute missing values with PCA ------------------------

  # Active variables names
  varact <- SEC_aux %>%
    st_drop_geometry() %>%
    select(SINP:M) %>%
    names()

  # Impute marker
  imputado <- is.na(SEC_aux[varact]) %>% apply(., 1, sum)

  # Imputation
  SEC_aux[varact] <- SEC_aux[varact] |>
    st_drop_geometry() |>
    imputePCA(scale = T, ncp = 4) |>
    pluck("completeObs")

  AUX$IMPUTA <- data.frame(ID_s = SEC_aux$ID_s, IMPUTADO = imputado)

  rm(varact, imputado)

# MFA para segmentación --------------------------------------

  # Group names
  n_GRUP <- c("Origen", "Rendimiento", "Homogeneidad",
              "Ubicacion", "Estructura", "Oferta", "Desgranamiento",
              "TH", "Sector", "Continuidad", "Turno",
              "Título", "Subsidio/Coop", "Homogeneidad (cuali)")
  # MFA
  SEC_MFA <- SEC_aux |>
    select(-ID_s) |>
    st_drop_geometry() |>
    MFA(group = c(11, 17, 6,
                  2, 5, 4, 1,
                  1, 1, 1, 5, 4, 2, 2),
        type = c(rep("s", 7), rep("n", 7)),
        name.group = n_GRUP,
        num.group.sup = c(4:14 ),
        graph = F)

  rm(n_GRUP)

# Armado de fuzzy cluster -----------------------------------------

SEC_CL <- Fclust(SEC_MFA$ind$coord[ , 1:4], k = 4, noise = T)

CL_ORDEN <- order(SEC_CL$H[, 1])
SEC_aux <- cbind(SEC_aux, cl = SEC_CL$clus[ , "Cluster"],
                 cl_p = SEC_CL$clus[ , "Membership degree"],
                 SEC_CL$U[ , CL_ORDEN]) %>%
  mutate(cl = factor(cl, levels = CL_ORDEN,
                     labels = 1:4) )

colnames(SEC_aux)[str_starts(colnames(SEC_aux), "Clus.")] <- paste0("Clus.", 1:4)

# Indices de calidad de los agrupamientos ---------------------------------
# el indice PC y MPC son mediciones de "difusidad" (fuzzy)
I <- Fclust.index(SEC_CL, alpha = 1)

# Descripción de clusters:
D <- SEC_aux %>%
  st_drop_geometry() %>%
  group_by(cl) %>%
  summarise(Cl.Size = n(),
            Cl.Student = sum(n),
            No.Asig = sum(cl_p <= 0.5),
            P.Asig = No.Asig/Cl.Size * 100,
            deg.Min = min(cl_p),
            deg.Max = max(cl_p),
            deg.Av = mean(cl_p))

# Distancia entre cluster:
H <- SEC_CL$H[CL_ORDEN, ]
rownames(H) <- paste0 ("Clus ", 1:4)
HD <- round(dist(H), 2)

# Comparacion entre Sector y cluster
CSec <- Fclust.compare(VC = SEC_aux$SECTOR,
                       SEC_aux |>
                         select(starts_with("Clus.")) |>
                         st_drop_geometry()) %>%
  round(digits = 3)

# Tabla (Sub)Sector - cluster
t1 <- cbind(round(prop.table(xtabs(n ~ cl + SUBS, data = SEC_aux), 1) * 100, 1),
            Total = xtabs(n ~ cl, data = SEC_aux) )
t1 <- t1 %>%
  as_tibble(rownames = "Circuito") %>%
  mutate(Circuito = paste("Clúster", Circuito),
         Privado = NO + SI) %>%
  select(Circuito, Estatal, Privado, SI, NO, Total)

CLUSTER <- list(H = H, Index = I, Descrip = D,
                Distan = HD, Ind_Sec = CSec, Tab_Sec = t1)

rm(CL_ORDEN, CSec, I, D, H, HD, SEC_CL, t1)

# Descripción de los cluster ----------------------------------------------

dat <- SEC_aux %>%
  st_drop_geometry() %>%
  select(cl, SINP:TIPO_HABITAT,
         CONTINUIDAD_PRE:TIT_TECNICO,
         SUBS, LQ_SUP) %>%
  mutate(SUBS = factor(SUBS,
                       levels = c("Estatal", "SI", "NO"),
                       labels = c("Estatal", "Priv. Con Subsidio",
                                  "Priv. Independiente")))

CL_DESC <- catdes(dat, 1)

# Variables
for(i in c("test.chi2", "quanti.var")){
  CL_DESC[[i]] <- CL_DESC[[i]] %>%
    as_tibble(rownames = "VAR") %>%
    left_join(AUX$lab, by = "VAR")
}

# Variables descriptivas cuantitativas
for(i in 1:length(CL_DESC$quanti)){
  CL_DESC$quanti[[i]] <- CL_DESC$quanti[[i]] %>%
    as_tibble(rownames = "VAR") %>%
    left_join(AUX$lab, by = "VAR")
}
names(CL_DESC$quanti) <- paste0("Clus.", 1:4)

# Variables descriptivas cualitativas
for(i in 1:length(CL_DESC$category )){
  CL_DESC$category[[i]] <- CL_DESC$category[[i]] %>%
    as_tibble(rownames = "V") %>%
    mutate(VAR = str_split(V, "=", simplify = T)[ , 1],
           CAT = str_split(V, "=", simplify = T)[ , 2],
           CAT = case_when(CAT %in% c("FALSE", "NO") ~ "No",
                           CAT %in% c("TRUE", "SI") ~ "Sí",
                           T ~ CAT)  ) %>%
    left_join(AUX$lab, by = "VAR") %>%
    filter(CAT != "NA" &
             !(str_starts(VAR, "T_" ) & CAT == "No") &
             !(str_starts(VAR, "TIT_" ) & CAT == "No") &

             !(str_starts(VAR, "CONTINUIDAD_" ) &
                 CAT == "No")) %>%
    mutate(Indicador = paste(Indicador, CAT, sep = ": "))
}
names(CL_DESC$category) <- paste0("Clus.", 1:4)

# Principales características por cluster
C_CUANTI <- C_CUALI <- data.frame()
for(i in 1:4){
  #cat(paste("\n", "Circuito", i, "\n"))
  c <- paste("Clúster", i)
  b <- CL_DESC$quanti[[i]]
  C_CUANTI[1:10, c] <- b[c(1:5, ((nrow(b)-4):nrow(b))), ]  %>%
    mutate(X_ = `Mean in category`,
           X_ = ifelse(str_starts(Indicador, "%"),
                       X_ * 100, X_),
           X_ = ifelse(X_ > 1, round(X_, 1), round(X_, 3)),
           Indicador = paste(Indicador,
                             round(X_, 3),
                             sep = ": ")) %>%
    select(Indicador)
  b <- CL_DESC$category[[i]]
  C_CUALI[1:10, c] <- b[c(1:5, ((nrow(b)-4):nrow(b))), ]  %>%
    mutate(Indicador = paste(Indicador, " (",
                             round(`Mod/Cla`, 1), "%)",
                             sep = "")) %>%
    select(Indicador)
}

SEC_MFA$CL_DESC <- CL_DESC

rm(dat, i, c, b, CL_DESC)

# Segregación -------------------------------------------------------------

s_aux <- SEC_seg |>
  rename_with(.cols = -ID_s,
              .fn = ~str_remove(.x, pattern = "N_") ) |>
  left_join(SEC_aux |> st_drop_geometry() |> select(ID_s, SECTOR, cl),
            by = "ID_s") |>
  filter(!is.na(cl)) |>
    pivot_longer(cols = PRI:SUP, names_to = "group",
                 values_to = "peso")

# Global = Between Seg + Within Seg
SEGRE <- list()

SEGRE$t_f <- cbind(I = c("Sólo Agrupamiento", "Sólo Sector",
                   "Agrupamiento + Sector"),
             rbind(f_seg(db = s_aux, VAR = "cl"),
                   f_seg(db = s_aux, VAR = "SECTOR"),
                   f_seg(db = s_aux, VAR = c("cl", "SECTOR"))))  |>
        mutate(Prop_ENTRE = round(Between / Global * 100, 1),
               Prop_DENTRO = round(Within / Global * 100, 1)) %>%
        select(I, Prop_ENTRE, Prop_DENTRO)

# Local
SEGRE$t_l <- mutual_local(s_aux, group = "group", unit = "cl",
                    weight = "peso", se =F, wide = T) %>%
          select(cl, p, Inter = ls) %>%
          mutate(cl = paste("Clúster", cl),
                 Intra = map_dbl(1:4,
                                 \(x) mutual_total(data = s_aux[s_aux$cl == x, ],
                                                   group = "group", unit = "ID_s",
                                                   weight = "peso", se = F)$est[1] )
                 )

rm(s_aux, SEC_seg)
